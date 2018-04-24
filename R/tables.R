#' Build the table with the predicted median lifespans.
#'
#' This is a main function of this package. It builds the table containing the
#' prediction for the median lifespans for the different categories of elephants.
#' The predictions are done for two age at capture (5 and 20 years old).
#'
#' @inheritParams parameters
#' @return A data frame containing the median lifespans.
#' @export
#'
#' @examples
#' table_lifespan(models_list = Models)
#'
table_lifespan <- function(models_list, save_xlsx = FALSE){

  table_lifespan <- data.frame(matrix(NA, nrow = 7, ncol = 4))
  rownames(table_lifespan) <- c("Captive", "STOCKADE_5", "STOCKADE_20", "IMM_5", "IMM_20", "MILARSHI_5", "MILARSHI_20")
  colnames(table_lifespan) <- c("Capture status", "Age at capture (yrs)", "males", "females")
  table_lifespan[, 1] <- c("Captive born", "Captured by stockade", "", "Captured by immobilisation", "", "Captured by milarshi", "")
  table_lifespan[, 2] <- c(NA, 5, 20, 5, 20, 5, 20)

  for (sex in c("males", "females")) {
    d <- data.frame(Age = 0:55, Sex = sex, Region = "Magway", BirthCohort = "1960")
    d_captive <- cbind(d, data.frame(CaptureMethod = "CAPTIVE", TimeSinceCapture = 0))
    table_lifespan["Captive", sex] <- median_lifespan(models_list = models_list, newdata = d_captive)
    for (CaptureMethod in c("IMM", "STOCKADE", "MILARSHI")) {
      d_captured_5yrs  <- cbind(d, data.frame(CaptureMethod = c(rep("CAPTIVE", (length(0:5) - 1)),
                                                                rep(CaptureMethod, length(0:50))),
                                              TimeSinceCapture = c(rep(0, 5), 0:50)))
      d_captured_20yrs <- cbind(d, data.frame(CaptureMethod = c(rep("CAPTIVE", (length(0:20) - 1)),
                                                            rep(CaptureMethod, length(0:35))),
                                              TimeSinceCapture = c(rep(0, 20), 0:35)))
      table_lifespan[paste0(CaptureMethod, "_5"), sex] <- median_lifespan(models_list = models_list,
                                                                      newdata = d_captured_5yrs)
      table_lifespan[paste0(CaptureMethod, "_20"), sex] <- median_lifespan(models_list = models_list,
                                                                       newdata = d_captured_20yrs)
    }
  }

  rownames(table_lifespan) <- NULL
  colnames(table_lifespan)[3:4] <- c("Males", "Females")

  if (save_xlsx) {
    save_xlsx(table = table_lifespan, file_name = "table_half_life", name_first_column = NULL)
  }
  return(table_lifespan)
}


#' Build the table with model parameter estimates for all models
#'
#' This is a main function of this package. It builds the table containing all
#' parameter estimates for all models, as well as those produce produced by
#' model averaging.
#'
#' @inheritParams parameters
#'
#' @return A data.frame containing the estimates.
#' @seealso \code{\link{fit_all_models}}, \code{\link{boot_all_models}}
#' @export
#'
#' @examples
#' table_estimates(models_list = Models,
#'                 models_list_boot = Models_boot)
#'
table_estimates <- function(models_list, models_list_boot, save_xlsx = FALSE) {

  table_est <- do.call("cbind", lapply(models_list, function(mod) mod$param_matrix_full))
  colnames(table_est) <- names(models_list)

  if (save_xlsx) {
    save_xlsx(table = table_est, file_name = "table_estimates", name_first_column = "Parameter")
  }

  return(table_est)
}


#' Build the table of the results of the model averaging across the 17 fitted models
#'
#' This is a main function of this package. It performs the model averaging.
#'
#' To compute averaged parameter estimates, we used the so-called "shrinkage estimator".
#' That is, the parameter estimates from all models are being considered
#' and parameters that were not estimated are considered to be equal to zero.
#'
#' To compute the confidence intervals, we used the approximation [\code{pnorm(0.025)*SE}; \code{pnorm(0.975)*SE}].
#'
#' To compute the standard error, we used equation 5 in Lukacs et al. 2010 (see References).
#'
#' We indicated estimates higher than 1e-5 by a star stored in the last column of the table.
#'
#' @references
#' Lukacs, Burnham & Anderson (2010). Model selection bias and Freedman’s paradox.
#' Ann. Inst. Stat. Math. 62:117–125
#'
#'
#' @inheritParams parameters
#'
#' @return A data.frame storing the output of the model average.
#' @seealso \code{\link{boot_all_models}}, \code{\link{table_boot}}, \code{\link{list_tables_boot}}, \code{\link{table_SE}}
#' @export
#'
#' @examples
#' table_model_averaging(models_list = Models,
#'                       models_list_boot = Models_boot)
#'
#'
table_model_averaging <- function(models_list, models_list_boot, save_xlsx = FALSE) {
  if (save_xlsx & !requireNamespace("openxlsx", quietly = TRUE)) {
    warning("The table cannot be exported to *.xlsx because the package 'openxlsx' is not installed.")
  }
  aic_weights <- compute_aic_w(models_list)
  params_mean <- compute_estimates_average(models_list = models_list)
  SEs <- table_SE(models_list = models_list,
                  models_list_boot = models_list_boot,
                  print = FALSE)
  expanded_param_matrix_list <- lapply(models_list, function(m) m$param_matrix_full)
  bias <- do.call("cbind", lapply(expanded_param_matrix_list, function(p) p[, "value"] - params_mean))
  raw <- SEs^2 + bias^2
  table_MA <- data.frame(Mean = params_mean, SE = sqrt(raw %*% aic_weights))
  table_MA$Lwr <- table_MA$Mean + stats::pnorm(0.025)*table_MA$SE
  table_MA$Upr <- table_MA$Mean + stats::pnorm(0.975)*table_MA$SE
  table_MA$Star <- ifelse(table_MA$Lwr > 1e-5, "*", "")

  colnames(table_MA)[ncol(table_MA)] <- " "

  if (save_xlsx) {
    save_xlsx(table = table_MA, file_name = "table_model_averaging", name_first_column = "Parameter")
  }

  return(table_MA)
}


#' Build a table with the standard error of all parameter estimates for all models
#'
#' This function is called internally by other functions.
#' It is used for performing the model averaged computation of the confidence
#' intervals on parameter estimates. It provides the standard error on each parameter
#' estimates as estimated by bootstrap.
#'
#' @inheritParams parameters
#'
#' @return A data.frame storing the standard errors.
#' @export
#' @seealso \code{\link{table_model_averaging}}
#'
#' @examples
#' table_SE(models_list = Models, models_list_boot = Models_boot, print = FALSE)
#'
table_SE <- function(models_list, models_list_boot, print = TRUE, save_xlsx = FALSE) {

  if (length(models_list) < 2) {
    stop("This function requires more than one fitted model.")
  }

  list_boot <- list_tables_boot(models_list = models_list,
                                models_list_boot = models_list_boot,
                                print = print)

  table_SE <- sapply(1:length(models_list),
                     function(i) {
                       param_matrix <- models_list[[i]]$param_matrix
                       param_matrix[, "value"] <- list_boot[[i]][, "SE"]
                       expand_param_matrix(param_matrix = param_matrix)
                     })

  colnames(table_SE) <- names(models_list)
  rownames(table_SE) <- rownames(expand_param_matrix(models_list[[1]]$param_matrix))

  if (save_xlsx) {
    save_xlsx(table = table_SE, file_name = "table_SE", name_first_column = "Parameter")
  }

  return(table_SE)
}


#' Build the list of tables showing the results of the non-parametric bootstrap on each model
#'
#' This function is called internally by other functions.
#' It processes the outcomes of the fit of each of the 17 models on different bootstrap
#' replicates.
#'
#' @inherit table_boot details
#' @inherit table_boot return
#' @inheritParams parameters
#'
#' @return A list storing the tables created by \code{\link{table_boot}}.
#' @seealso \code{\link{boot_all_models}}, \code{\link{table_boot}}, \code{\link{table_model_averaging}}
#'
#' @export
#'
#' @examples
#' list_tables_boot(models_list = Models, models_list_boot = Models_boot)
#'
list_tables_boot <- function(models_list, models_list_boot, print = TRUE) {
  names_boot <- names(models_list_boot)
  basenames_boot <- unlist(lapply(strsplit(names_boot, "_"), function(i) i[1]))
  if (!identical(unique(basenames_boot), names(models_list))) {
    stop("The names of the models in models_list and models_list_boot do not match.")
  }
  tables <- sapply(names(models_list), function(model) {
    models_list_boot_sub <- models_list_boot[basenames_boot == model]
    table_boot(model = models_list[[model]], models_list_boot = models_list_boot_sub, print = print)
  }, simplify = FALSE)
  return(tables)
}


#' Build the table of the results of the non-parametric bootstrap
#'
#' This function is called internally by other functions.
#' It processes the outcomes of the fit of the same model on different bootstrap
#' replicates.
#'
#' The table(s) produced contain the following columns:
#' \enumerate{
#'   \item \code{beta} the parameter estimate in the original fit
#'   \item \code{mean} the mean of the parameter estimates across the fits on the bootstrapped data
#'   \item \code{bias} the difference between the column \var{mean} and \var{beta}
#'   \item \code{SE} the standard _error_ of the parameter estimates as computed as the standard _deviation_ of the parameter estimates across the fits on the bootstrapped data
#'   \item \code{lwr} the lower boundary of the 95\% confidence interval approximated as \code{pnorm(0.025)*SE}
#'   \item \code{upr} the upper boundary of the 95\% confidence interval approximated as \code{pnorm(0.975)*SE}
#' }
#'
#' @inheritParams parameters
#'
#' @return A data frame storing the results.
#' @seealso \code{\link{boot_all_models}}, \code{\link{list_tables_boot}}, \code{\link{table_model_averaging}}
#' @export
#'
#' @examples
#' index <- unlist(lapply(strsplit(names(Models_boot), "_"),
#'                 function(i) i[1] == "mod1")) ## find which bootstrap belong to mod1
#' mod1_boot <- Models_boot[index]
#' table_boot(model = Models$mod1, models_list_boot = mod1_boot)
#'
table_boot <- function(model, models_list_boot, print = TRUE) {
  convergences <- unlist(lapply(models_list_boot, function(m) m$convergence))
  messages <- unlist(lapply(models_list_boot, function(m) m$message))
  if (!all(convergences == 1) & print) {
    print("Summary of convergence status:")
    print(table(factor(messages)))
    print("Issues with  models:")
    print(names(models_list_boot)[convergences != 1])
    cat("\n")
  }
  params <- do.call("cbind", lapply(models_list_boot, function(i) i$param_matrix[, "value"]))
  out <- data.frame(Beta = model$param_matrix[, "value"])
  out$Mean <- apply(params, 1, mean)
  out$Bias <- out$Mean - out$Beta
  out$SE <- apply(params, 1, stats::sd)
  out$Lwr <- out$Beta + stats::pnorm(0.025)*out$SE
  out$Upr <- out$Beta + stats::pnorm(0.975)*out$SE
  return(out)
}


#' Build the table comparing all 17 models
#'
#' This is a main function of this package. It creates the table comparing all
#' 17 models contained in the paper.
#'
#' @inheritParams parameters
#'
#' @return The table of comparing all model fits.
#' @export
#' @seealso \code{\link{fit_all_models}}
#'
#' @examples
#' table_all_models(Models)
#'
#'
table_all_models <- function(models_list, save_xlsx = FALSE, print = TRUE) {
  if (save_xlsx & !requireNamespace("openxlsx", quietly = TRUE)) {
    warning("The table cannot be exported to *.xlsx because the package 'openxlsx' is not installed.")
  }
  convergences <- unlist(lapply(models_list, function(m) m$convergence))
  messages <- unlist(lapply(models_list, function(m) m$message))
  if (!all(convergences == 1) & print) {
    print("Summary of convergence status:")
    print(table(factor(messages)))
    print("Issues with  models:")
    print(names(models_list)[convergences != 1])
    cat("\n")
  }
  logLiks   <- unlist(lapply(models_list, function(m) m$logLik))
  Ks <- unlist(lapply(models_list, function(m) nrow(m$param_matrix)))
  aics   <- unlist(lapply(models_list, function(m) m$AIC))
  table_models <- data.frame(Model = order(aics),
                             logLik = logLiks[order(aics)],
                             K = Ks[order(aics)],
                             AIC = aics[order(aics)])
  table_models$w <- compute_aic_w(models_list[order(aics)])
  table_models$w_sum <- cumsum(table_models$w)
  params <- data.frame(do.call("rbind", lapply(models_list, function(m) attr(m$param_matrix, "param_str"))),
                       stringsAsFactors = FALSE)
  table_models <- cbind(table_models, params[order(aics), ])

  if (save_xlsx) {
    save_xlsx(table = table_models, file_name = "table_models", name_first_column = NULL)
  }

  return(table_models)
}


#' Build the table comparing the top models
#'
#' This function provides evidence ratios and AIC differences among top models.
#' The top models are the set of best models for which the sum of weights amount
#' to 95% of the sum on all 17 models.
#'
#' @inheritParams parameters
#'
#' @return A data frame containing the results.
#' @export
#'
#' @examples
#' evidence_ratios_top_models(models_list = Models)
#'
evidence_ratios_top_models <- function(models_list) {
  all <- table_all_models(models_list)
  top <- all[all$w_sum < 0.95, c("AIC", "w"), drop = FALSE]
  top$Evidence_ratio <- max(top$w) / top$w
  top$delta_AIC <- top$AIC - min(top$AIC)
  return(top)
}


