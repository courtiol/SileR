#' Predict mortality using model averaging
#'
#' This function is called internally by other functions. It predicts the
#' mortality rate (per year) of elephants based on a covariate information
#' provided as a data frame and based on a list of models used to compute the
#' parameter estimates by model averaging.
#'
#' @inheritParams parameters
#'
#' @return A vector with the mortality prediction(s).
#' @export
#'
#' @examples
#' d <- data.frame(Sex = "males", CaptureMethod = "STOCKAGE",
#'                 CaptureAge = 20, Age = 30, TimeSinceCapture = 10,
#'                 Region = "Kachin", BirthCohort = "2000")
#' predict_avg(models_list = Models, newdata = d)
#'
predict_avg <- function(models_list, newdata) {
  estimates_avg <- compute_estimates_average(models_list = models_list)

  full_str <- c("w1" = "s", "b1" = "s", "w2" = "s", "b2" = "s", "w3" = "s+t+l",
                "w4" = "s:c", "w5" = "s:c", "b4" = "s:c", "b5" = "s:c")

  param_matrix  <- build_param_matrix(param_values = estimates_avg,
                                      param_str = full_str)

  design_matrix <- build_design_matrix(data = newdata,
                                       param_matrix = param_matrix,
                                       print = FALSE)

  pred <- predictor(data = newdata, design_matrix = design_matrix)
  names(pred) <- NULL

  return(pred)
}


#' Predict mortality based on a single model
#'
#' This function is called internally by other functions. It predicts the
#' mortality rate (per year) of elephants based on a covariate information
#' provided as a data frame and based on one model.
#'
#' @inheritParams parameters
#'
#' @return A vector with the mortality prediction(s).
#' @export
#'
#' @examples
#' d <- data.frame(Sex = "males", CaptureMethod = "STOCKAGE",
#'                 CaptureAge = 20, Age = 30, TimeSinceCapture = 10,
#'                 Region = "Kachin", BirthCohort = "2000")
#' predict(model = Models[[1]], newdata = d)
#'
predict <- function(model, newdata) {
  design_matrix <- build_design_matrix(data = newdata,
                                       param_matrix = model$param_matrix_full,
                                       print = FALSE)

  pred <- predictor(data = newdata, design_matrix = design_matrix)
  names(pred) <- NULL

  return(pred)
}

#' Predict the mortality of captive born individuals
#'
#' This function predicts the mortality of captive born individuals.
#'
#' @inheritParams parameters
#'
#' @return A list containing the predicted mortality of captive born males and females.
#' @export
#' @seealso \code{\link{predict}}
#'
#' @examples
#' predict_captive_mortality(models_list = Models)
#'
predict_captive_mortality <- function(models_list){

  estimates_avg <- compute_estimates_average(models_list = models_list)

  smallest_param_cohort   <- names(which.min(estimates_avg[names(estimates_avg) %in% build_param_names(meta_param_name = "w3", meta_param_type = "t", check = FALSE)]))
  smallest_param_region <- names(which.min(estimates_avg[names(estimates_avg) %in% build_param_names(meta_param_name = "w3", meta_param_type = "l", check = FALSE)]))
  best_cohort <- get_param_names_without_meta(smallest_param_cohort)
  best_region <- get_param_names_without_meta(smallest_param_region)

  print(paste0("Best conditions identified: cohort = ", best_cohort, "; region = ", best_region))
  cat("\n")

  newdata_males <- expand.grid(Sex = "males",
                               CaptureMethod = "CAPTIVE",
                               CaptureAge = 0,
                               Age = age_males <- seq(0, 55, 0.01),
                               TimeSinceCapture = 0,
                               Region = best_region,
                               BirthCohort = best_cohort)

  newdata_females <- expand.grid(Sex = "females",
                               CaptureMethod = "CAPTIVE",
                               CaptureAge = 0,
                               Age = age_females <- seq(0, 54, 0.01),
                               TimeSinceCapture = 0,
                               Region = best_region,
                               BirthCohort = best_cohort)

  pred_males <- predict_avg(models_list = models_list, newdata = newdata_males)
  pred_females <- predict_avg(models_list = models_list, newdata = newdata_females)

  return(list(males = data.frame(cbind(pred = pred_males, age = age_males)),
              females = data.frame(cbind(pred = pred_females, age = age_females))))
}
