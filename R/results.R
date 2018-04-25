#' Provide demographic information about the data
#'
#' This is a main function of this package. It displays the information about
#' the demography of elephants that we reported in the paper.
#'
#' @inheritParams parameters
#'
#' @return Nothing, only displays the outcome.
#' @export
#'
#' @examples
#' compute_demography(data = SurvEles_small)
#'
compute_demography <- function(data){
  cat("All elephants:")
  cat("\n")
  cat("\n")
  print(paste0("Number of elephant-year observation = ", nrow(data)))
  print(paste0("Number of elephants = ", length(unique(data$ID))))
  print(paste0("Min age = ", min(data$Age)))
  print(paste0("Max age = ", max(data$Age)))
  print("breakdowns for observations:")
  print(table(data$Sex, data$CaptureMethod))
  print(table(data$Region, data$BirthCohort))
  print("breakdowns for individuals:")
  grouped_data <- do.call("rbind", by(data[, c("Sex", "CaptureMethod", "Region", "BirthCohort")], data$ID, unique))
  print(table(grouped_data$Sex, grouped_data$CaptureMethod))
  print(table(grouped_data$Region, grouped_data$BirthCohort))
  cat("\n")
  cat("Captive-born elephants:")
  cat("\n")
  cat("\n")
  print(paste0("Number of captive-born elephants = ", length(unique(data$ID[data$CaptureMethod == "CAPTIVE"]))))
  print(paste0("Number of males captive-born = ", length(unique(data$ID[data$CaptureMethod == "CAPTIVE" & data$Sex == "males"]))))
  print(paste0("Number of females captive-born = ", length(unique(data$ID[data$CaptureMethod == "CAPTIVE" & data$Sex == "females"]))))
  print(paste0("Earliest birth year = ", min(data$BirthYear[data$CaptureMethod == "CAPTIVE"])))
  print(paste0("Latest birth year = ", max(data$BirthYear[data$CaptureMethod == "CAPTIVE"])))
  cat("\n")
  cat("Captured elephants:")
  cat("\n")
  cat("\n")
  print(paste0("Number of captured elephants = ", length(unique(data$ID[data$CaptureMethod != "CAPTIVE"]))))
  print(paste0("Number of males captured = ", length(unique(data$ID[data$CaptureMethod != "CAPTIVE" & data$Sex == "males"]))))
  print(paste0("Number of females captured = ", length(unique(data$ID[data$CaptureMethod != "CAPTIVE" & data$Sex == "females"]))))
  print(paste0("Earliest capture = ", min(data$BirthYear[data$CaptureMethod != "CAPTIVE"] + data$CaptureAge[data$CaptureMethod != "CAPTIVE"])))
  print(paste0("Latest capture = ", max(data$BirthYear[data$CaptureMethod != "CAPTIVE"] + data$CaptureAge[data$CaptureMethod != "CAPTIVE"])))
  print(paste0("Earliest birth year = ", min(data$BirthYear[data$CaptureMethod != "CAPTIVE"])))
  print(paste0("Latest birth year = ", max(data$BirthYear[data$CaptureMethod != "CAPTIVE"])))
  cat("\n")
  cat("Elephants capture by stockage:")
  cat("\n")
  cat("\n")
  print(paste0("Number of males captured by stockage = ", length(unique(data$ID[data$CaptureMethod == "STOCKADE" & data$Sex == "males"]))))
  print(paste0("Number of females captured by stockage = ", length(unique(data$ID[data$CaptureMethod == "STOCKADE" & data$Sex == "females"]))))
  print(paste0("Earliest capture year = ", min(data$BirthYear[data$CaptureMethod == "STOCKADE"] + data$CaptureAge[data$CaptureMethod == "STOCKADE"])))
  print(paste0("Latest capture year = ", max(data$BirthYear[data$CaptureMethod == "STOCKADE"] + data$CaptureAge[data$CaptureMethod == "STOCKADE"])))
  print(paste0("Min age at capture = ", min(data$CaptureAge[data$CaptureMethod == "STOCKADE"])))
  print(paste0("Max age at capture = ", max(data$CaptureAge[data$CaptureMethod == "STOCKADE"])))
  print(paste0("Mean age at capture = ", round(mean(data$CaptureAge[data$CaptureMethod == "STOCKADE"]), 2)))
  print(paste0("SD of age at capture = ", round(stats::sd(data$CaptureAge[data$CaptureMethod == "STOCKADE"]), 2)))
  print(paste0("median of age at capture = ", round(stats::median(data$CaptureAge[data$CaptureMethod == "STOCKADE"]), 2)))
  cat("\n")
  cat("Elephants capture by immobilisation:")
  cat("\n")
  cat("\n")
  print(paste0("Number of males captured by immobilisation = ", length(unique(data$ID[data$CaptureMethod == "IMM" & data$Sex == "males"]))))
  print(paste0("Number of females captured by immobilisation = ", length(unique(data$ID[data$CaptureMethod == "IMM" & data$Sex == "females"]))))
  print(paste0("Earliest capture year = ", min(data$BirthYear[data$CaptureMethod == "IMM"] + data$CaptureAge[data$CaptureMethod == "IMM"])))
  print(paste0("Latest capture year = ", max(data$BirthYear[data$CaptureMethod == "IMM"] + data$CaptureAge[data$CaptureMethod == "IMM"])))
  print(paste0("Min age at capture = ", min(data$CaptureAge[data$CaptureMethod == "IMM"])))
  print(paste0("Max age at capture = ", max(data$CaptureAge[data$CaptureMethod == "IMM"])))
  print(paste0("Mean age at capture = ", round(mean(data$CaptureAge[data$CaptureMethod == "IMM"]), 2)))
  print(paste0("SD of age at capture = ", round(stats::sd(data$CaptureAge[data$CaptureMethod == "IMM"]), 2)))
  print(paste0("median of age at capture = ", round(stats::median(data$CaptureAge[data$CaptureMethod == "IMM"]), 2)))
  cat("\n")
  cat("Elephants capture by milarshikar:")
  cat("\n")
  cat("\n")
  print(paste0("Number of males captured by milarshikar = ", length(unique(data$ID[data$CaptureMethod == "MILARSHI" & data$Sex == "males"]))))
  print(paste0("Number of females captured by milarshikar = ", length(unique(data$ID[data$CaptureMethod == "MILARSHI" & data$Sex == "females"]))))
  print(paste0("Earliest capture year = ", min(data$BirthYear[data$CaptureMethod == "MILARSHI"] + data$CaptureAge[data$CaptureMethod == "MILARSHI"])))
  print(paste0("Latest capture year = ", max(data$BirthYear[data$CaptureMethod == "MILARSHI"] + data$CaptureAge[data$CaptureMethod == "MILARSHI"])))
  print(paste0("Min age at capture = ", min(data$CaptureAge[data$CaptureMethod == "MILARSHI"])))
  print(paste0("Max age at capture = ", max(data$CaptureAge[data$CaptureMethod == "MILARSHI"])))
  print(paste0("Mean age at capture = ", round(mean(data$CaptureAge[data$CaptureMethod == "MILARSHI"]), 2)))
  print(paste0("SD of age at capture = ", round(stats::sd(data$CaptureAge[data$CaptureMethod == "MILARSHI"]), 2)))
  print(paste0("median of age at capture = ", round(stats::median(data$CaptureAge[data$CaptureMethod == "MILARSHI"]), 2)))
  cat("\n")
  return(invisible(NULL))
  }


#' Provide survival information
#'
#' This is a main function of this package. It displays the information about
#' the survival of elephants that we reported in the paper (e.g. half life...).
#'
#' @inheritParams parameters
#'
#' @return Nothing, only displays the outcome.
#' @export
#'
#' @examples
#' compute_survival_info(models_list = Models)
#'
compute_survival_info <- function(models_list) {
  pred_cap <- predict_captive_mortality(models_list = models_list)
  mean_param <- compute_estimates_average(models_list = models_list)

  cat("Captive-born elephants:")
  cat("\n")
  cat("\n")
  print(paste0("Age at highest mortality for males in best condition = ", pred_cap$males$age[which.max(pred_cap$males$pred)]))
  print(paste0("Age at highest mortality for females in best condition = ", pred_cap$females$age[which.max(pred_cap$females$pred)]))
  print(paste0("Highest mortality rate for males in best condition (in %) = ", round(100*pred_cap$males$pred[which.max(pred_cap$males$pred)], 2)))
  print(paste0("Highest mortality rate for females in best condition (in %) = ", round(100*pred_cap$females$pred[which.max(pred_cap$females$pred)], 2)))
  cat("\n")
  print(paste0("Intial half-life of decrease in males = ", round(log(2)/mean_param["b1.males"], 2)))
  print(paste0("Intial half-life of decrease in females = ", round(log(2)/mean_param["b1.females"], 2)))
  print(paste0("Age at lowest mortality for males in best condition = ", pred_cap$males$age[which.min(pred_cap$males$pred)]))
  print(paste0("Age at lowest mortality for females in best condition = ", pred_cap$females$age[which.min(pred_cap$females$pred)]))
  cat("\n")
  print(paste0("Half-life of ageing in males = ", round(log(2)/mean_param["b2.males"], 2)))
  print(paste0("Half-life of ageing in females = ", round(log(2)/mean_param["b2.females"], 2)))
  cat("\n")
  w3_region <- sapply(build_param_names(meta_param_name = "w3", meta_param_type = "l", check = FALSE),
         function(level) mean_param[level])
  w3_birthcohort <- sapply(build_param_names(meta_param_name = "w3", meta_param_type = "t", check = FALSE),
                           function(level) mean_param[level])
  print(paste0("Maximal increase in mortality due to region (in %) = ", round(100*max(w3_region), 2)))
  print(paste0("Maximal increase in mortality due to birth cohort (in %) = ", round(100*max(w3_birthcohort), 2)))
  return(invisible(NULL))
}


#' Provide information about the best model
#'
#' This is a main function of this package. It displays the information about
#' the best model.
#'
#' @inheritParams parameters
#'
#' @return Nothing but display the results.
#' @export
#'
#' @examples
#' best_model_description(models_list = Models)
#'
best_model_description <- function(models_list){
  w <- compute_aic_w(models_list)
  best_mod <- models_list[[names(models_list)[which.max(w)]]]
  ## note: choices for covariates do not matter, since we look at relative difference
  new_captured <- data.frame(Sex = "females", CaptureMethod = "STOCKADE", Age = c(5, 10, 20, 40),
                             TimeSinceCapture = c(0, 0, 0, 0), Region = "Mandalay", BirthCohort = "2000")
  new_captive_born <- new_captured
  new_captive_born$CaptureMethod <- "CAPTIVE"
  pred_captured <- predict(model = best_mod, newdata = new_captured)
  pred_captive_born <- predict_avg(models_list = models_list, newdata = new_captive_born)
  res <- data.frame(Age_at_capture = new_captured$Age, Excess_mortality_in_pct = pred_captured - pred_captive_born)
  res[, 2] <- round(100*res[, 2], 2)
  print("Increase in mortality caused by capture (at year of capture):")
  print(res)
  cat("\n")

  param <- best_mod$param_matrix[, "value"]

  if (!all(c("w4.unique", "w5.unique", "b4.unique") %in% names(param))) {
    stop("This function can only handle models with unique estimate for w4, w5 and b4,
         but another model seems to be the top model. This is probably because you did
         not use the complete dataset to fit your models.")
    }

  half_life_excess <- log(2)/param[["b4.unique"]]

  print(paste0("The excess in mortality caused by capture reduces by half every ", round(half_life_excess, 2), " years."))
  cat("\n")

  belowpct <- function(age_cap, pct) {
    do <- TRUE
    age <- 0
    while (do) {
      age <- age + 0.01
      do <- ((param[["w4.unique"]] - 1)*exp(-param[["w5.unique"]]*age_cap) + 1)*exp(-param[["b4.unique"]]*age) > pct
    }
    age
  }

  thresholds <- sapply(new_captured$Age, function(age_at_capture) belowpct(age_at_capture, 0.001))
  res2 <- data.frame(Age_at_capture = new_captured$Age, Age = thresholds)
  print("Time since capture for which increase in mortality becomes lower than 0.1%:")
  print(res2)

  return(invisible(NULL))
}
