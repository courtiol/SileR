#' Predict the probability of death
#'
#' This function is not actually used. It is a R replicate of the C++ function \code{predictorC}
#' that is actually used. It computes the probability of death for each observation.
#'
#' @inheritParams parameters
#'
#' @return A vector of the probability of death.
#' @export
#' @seealso \code{\link{predictorC}}
#'
#' @examples
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' dmat <- build_design_matrix(data = SurvEles_small, param_matrix = pmat)
#' p <- predictor(data = SurvEles_small, design_matrix = dmat)
#' table(p)
#'
predictor <- function(data, design_matrix, indices = NULL){

  if (is.null(indices)) {
    bool_captured <- data$CaptureMethod != "CAPTIVE"
  } else {
    bool_captured <- indices$b.captured
  }

  prob_base <- design_matrix[, "w1"] * exp(-design_matrix[, "b1"] * data$Age) + design_matrix[, "w2"] * exp(design_matrix[, "b2"] * data$Age) + design_matrix[, "w3"]
  prob <- prob_base + bool_captured * ((design_matrix[, "w4"] - 1)*exp(-design_matrix[, "w5"]*data$Age) + 1) * (exp(-(design_matrix[, "b4"]^(1 + design_matrix[, "b5"]^data$Age))*data$TimeSinceCapture))

  prob[prob >= 1] <- 1 - 1e-9
  prob[prob <= 0] <- 0 + 1e-9

  return(prob)
}


#' Predict the probability of death (R wrapper for C++)
#'
#' This function is called internally by other functions.
#' It is a wrapper for the function \code{predictorC} which
#' computes the probability of death in C++ (for faster computation).
#'
#' @inheritParams parameters
#'
#' @return A vector of the probability of death.
#' @seealso \code{\link{predictor}}
#' @export
#'
#' @examples
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' dmat <- build_design_matrix(data = SurvEles_small, param_matrix = pmat)
#' p <- predictorC_wrap(data = SurvEles_small, design_matrix = dmat)
#' table(p)
#'
predictorC_wrap <- function(data, design_matrix, indices = NULL){

  if (is.null(indices)) {
    bool_captured <- data$CaptureMethod != "CAPTIVE"
  } else {
    bool_captured <- indices$b.captured
  }

  prob <- predictorC(design_matrix[, "w1"],
                     design_matrix[, "b1"],
                     design_matrix[, "w2"],
                     design_matrix[, "b2"],
                     design_matrix[, "w3"],
                     design_matrix[, "w4"],
                     design_matrix[, "w5"],
                     design_matrix[, "b4"],
                     design_matrix[, "b5"],
                     data$Age,
                     data$TimeSinceCapture,
                     bool_captured)
  return(prob)
}


#' Compute the log-likelihood
#'
#' This function is not actually used. It is a R replicate of the C++ function \code{compute_logLikC}
#' that is actually used. It computes the log-likelihood.
#'
#' @inheritParams parameters
#'
#' @return The scaled log-likelihood.
#'
#' @export
#'
#' @examples
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' dmat <- build_design_matrix(data = SurvEles_small, param_matrix = pmat)
#' p <- predictor(data = SurvEles_small, design_matrix = dmat)
#' compute_logLik(pred_prob = p, surv_bin = SurvEles_small$Alive, scale = 1L)
#' compute_logLikC(pred_prob = p, surv_bin = SurvEles_small$Alive, scale = 1L)
#'
compute_logLik <- function(pred_prob, surv_bin, scale = 1L) {
  scale *  (sum(log(pred_prob) * (1 - surv_bin)) + sum(log(1 - pred_prob) * surv_bin))
}


#' Compute the log-likelihood from the design matrix
#'
#' This function is called internally by other functions.
#' It prepares data and calls \code{\link{predictorC_wrap}} to predict the probability of death
#' and then calls the C++ function \code{compute_logLikC} to computes the log-likelihood in C++.
#' @inheritParams parameters
#'
#' @return The scaled log-likelihood.
#' @export
#' @seealso \code{\link{predictorC_wrap}}, \code{\link{compute_logLik}}
#'
#' @examples
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' dmat <- build_design_matrix(data = SurvEles_small, param_matrix = pmat)
#' logLik_from_design_matrix(data = SurvEles_small, design_matrix = dmat)
#'
logLik_from_design_matrix <- function(data, design_matrix, scale = 1, indices = NULL) {
  prob <- predictorC_wrap(data = data, design_matrix = design_matrix, indices = indices)
  logLik <- compute_logLikC(pred_prob = prob, surv_bin = data$Alive, scale = scale)
  return(logLik)
}

#' Compute the log-likelihood from parameter vector
#'
#' This function is called internally by other functions.
#' It is a wrapper to the function \code{\link{logLik_from_design_matrix}} which has
#' inputs consistent to the requirements of optimisation routines.
#'
#' If the argument \code{param_matrix} is given, the parameter matrix will be
#' updated and not build from scratch. This will save some computation time.
#'
#' @inheritParams parameters
#'
#' @return The scaled log-likelihood.
#' @seealso \code{\link{logLik_from_design_matrix}}
#' @export
#'
#' @examples
#' logLik_from_vector_param(param_values = c(0.1, 0.200001), param_str = c("w1" = "s"),
#'                          data = SurvEles_small)
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' logLik_from_vector_param(param_values = c(0.1, 0.200001, rep(0, 14)), param_str = c("w1" = "s"),
#'                          data = SurvEles_small, param_matrix = pmat)
#'
logLik_from_vector_param <- function(param_values, param_str, data,
                                     indices = NULL, scale = 1,
                                     param_matrix = NULL, empty_param_matrix_full = NULL, empty_design_matrix = NULL) {
  if (!is.null(param_matrix)) {
    stopifnot(length(param_values) == nrow(param_matrix))
    pmat <- param_matrix
    pmat[, "value"] <- param_values
  } else {
    pmat <- build_param_matrix(param_values = param_values, param_str = param_str)
  }
  dmat <- build_design_matrix(data = data, param_matrix = pmat, indices = indices, empty_param_matrix_full = empty_param_matrix_full, empty_design_matrix = empty_design_matrix)
  logLik_from_design_matrix(data = data, design_matrix = dmat, scale = scale, indices = indices)
}


#' Fit the model
#'
#' This function is called internally by \code{\link{fit_all_models}}.
#' It is the actual function performing the fitting procedure.
#' To fit all the models, use \code{fit_all_models} instead.
#'
#' The function uses the computation of the likelihood from \code{\link{logLik_from_vector_param}} and
#' optimise the parameter values using the optimisation routine \code{\link[nloptr]{nloptr}} from the package of the
#' same name. Specifically, we are using the method called "NLOPT_LN_BOBYQA".
#'
#' @inheritParams parameters
#'
#' @return A list containing the outcome of the fitting procedure
#' @seealso \code{\link{fit_all_models}}
#' @export
#'
#' @examples
#' pmat_ini <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' pmat_ini
#' ## Warning: increase maxtime for real fit!
#' fit(data = SurvEles_small, param_matrix_ini = pmat_ini, maxtime = 1)
#'
fit <- function(data, param_matrix_ini, xtol_rel = 1.0e-7, maxtime = Inf, lb = 0, ub = 3, print_level = 0) {

  indices <- compute_indices(data = data)

  full_str <- c("w1" = "s", "b1" = "s", "w2" = "s", "b2" = "s", "w3" = "s+t+l",
                "w4" = "s:c", "w5" = "s:c", "b4" = "s:c", "b5" = "s:c")
  empty_param_matrix_full <- build_param_matrix(param_values = 0, param_str = full_str)

  empty_design_matrix <- build_empty_design_matrix(data)

  time <- system.time({
    if (identical( parent.frame(n = 1) , .GlobalEnv)) print("fitting in progress...")
    fit <- nloptr::nloptr(x0 =  param_matrix_ini[, "value"],
                          eval_f = logLik_from_vector_param,
                          param_str = attr(param_matrix_ini, "param_str"),
                          data = data,
                          indices = indices,
                          scale = -1,
                          param_matrix = param_matrix_ini,
                          empty_param_matrix_full = empty_param_matrix_full,
                          empty_design_matrix = empty_design_matrix,
                          lb = rep(lb, nrow(param_matrix_ini)), ub = rep(ub, nrow(param_matrix_ini)),
                          opts = list(algorithm = "NLOPT_LN_BOBYQA",
                                      xtol_rel = xtol_rel,
                                      maxtime = maxtime,
                                      maxeval = -1,
                                      print_level = print_level))
  })

  param_matrix  <- build_param_matrix(param_values = fit$solution, param_str = attr(param_matrix_ini, "param_str"))
  design_matrix <- build_design_matrix(data = data, param_matrix = param_matrix, indices = indices, empty_design_matrix = empty_design_matrix)
  pred <- predictor(data = data, design_matrix = design_matrix)

  out <- list(logLik = fit$objective*-1,
              param_matrix = param_matrix,
              param_matrix_full = expand_param_matrix(param_matrix),
              convergence = fit$status,
              message = fit$message,
              fitting_time = round(as.numeric(time[3]), 1),
              range = range(pred),
              AIC = 2*fit$objective + 2*length(fit$solution))

  return(out)
}
