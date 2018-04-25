#' Compute the AIC weights
#'
#' This function is called internally by other functions.
#' It computes the AIC weights from a list of fitted models.
#'
#' @inheritParams parameters
#'
#' @return The vector of AIC weights.
#' @export
#' @seealso \code{\link{table_boot}}
#'
#' @examples
#' compute_aic_w(Models)
#'
compute_aic_w <- function(models_list) {
  AICs   <- unlist(lapply(models_list, function(m) m$AIC))
  delta_AICs <- AICs - min(AICs)
  relative_strength_evidence <- exp(-1/2*delta_AICs)
  aics_weights <- relative_strength_evidence/sum(relative_strength_evidence)
  return(aics_weights)
}


#' Compute parameter estimates by model averaging
#'
#' This function is called internally by other functions.
#' It computes parameter estimates by model averaging.
#'
#' We used the so-called "shrinkage estimator".
#' That is, the parameter estimates from all models are being considered
#' and parameters that were not estimated are considered to be equal to zero.
#'
#' @inheritParams parameters
#'
#' @return A vector of estimates.
#' @seealso \code{\link{table_model_averaging}}
#' @export
#'
#' @examples
#' compute_estimates_average(models_list = Models)
#'
compute_estimates_average <- function(models_list) {
  aic_weights <- compute_aic_w(models_list)
  expanded_param_matrix_list <- lapply(models_list, function(m) m$param_matrix_full)
  all_parameters <- do.call("cbind", expanded_param_matrix_list)
  params_mean <- apply(all_parameters, 1, function(l) sum(l*aic_weights))
  return(params_mean)
}


#' Interpolate yearly survivorship
#'
#' This function is called internally by other functions. It interpolates yearly
#' survivorship probablitites. The method is described in Slud 2001.
#'
#' @references
#' Slud 2001 Actuarial Mathematics and Life-Table Statistics
#' (\url{http://www2.math.umd.edu/~hmg/Stat470book.pdf})
#'
#' @inheritParams parameters
#'
#' @return A data frame containing the interpolated survivorship probabilities for each age.
#' @export
#'
#' @examples
#' interpol_surv(death_prob = c(0.1, 0.8, 0.9, 0.9), ages = 1:4)
#'
interpol_surv <- function(death_prob, ages, steps = 10){
  method <- function(S1, S2, t) {exp(t*log(S2) + (1 - t)*log(S1))} ## piecewise const (p93 Slud 2001)
  surv <- 1 - death_prob
  cum_surv <- cumprod(surv)
  rec_cum_surv <- rep(NA, (length(cum_surv) - 1)*steps)
  rec_age <- rep(NA, (length(cum_surv) - 1)*steps)
  i <- 1
  for (s in 1:(length(cum_surv) - 1)) {
    for (t in seq(0, 1, 1/steps)) {
      rec_cum_surv[i] <- method(S1 = cum_surv[s], S2 = cum_surv[s + 1], t = t)
      rec_age[i] <- ages[s] + t
      i <- i + 1
    }
    i <- i - 1
  }

  return(data.frame(Age = rec_age, Surv = rec_cum_surv))
}


#' Compute the median lifespan
#'
#' This function is called internally by other functions. It computes the
#' predicted median lifespan.
#'
#' @inheritParams parameters
#'
#' @return The value of the median lifespan.
#' @export
#'
#' @examples
#' d <- data.frame(Sex = "males", CaptureMethod = "CAPTIVE",
#'                 CaptureAge = 0, Age = 0:100, TimeSinceCapture = 0,
#'                 Region = "Mandaley", BirthCohort = "1960")
#' median_lifespan(newdata = d, models_list = Models)
#'
median_lifespan <- function(newdata, models_list) {
  pred_death <- predict_avg(models_list = models_list, newdata = newdata)
  pred_survivorship <- interpol_surv(death_prob = pred_death, ages = newdata$Age, steps = 100)
  res <- pred_survivorship[pred_survivorship$Surv < 0.5, "Age"][1]
  if (is.na(res)) {
    stop("The half life cannot be computed on your data because the predictive survivorship remains higher than 0.5 for all ages.
This is probably because you did not use the complete dataset.")
  }
  return(res)
}


#' Save a table in the *.xlsx format
#'
#' This function is called internally by other functions. It is a wrapper for
#' the function \code{\link[openxlsx:write.xlsx]{write.xlsx}} of the package \pkg{openxlsx}.
#'
#' @inheritParams parameters
#' @return The input table (but the return is invisible).
#' @export
#'
save_xlsx <- function(table, file_name, name_first_column = NULL){
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    tryCatch({## the function can crash if no zip program is known to R
      if (!is.null(name_first_column)) {
        table <- eval(parse(text = paste0("cbind(", name_first_column, " = rownames(table), table)")))
      }
      hs <- openxlsx::createStyle(border = "TopBottom",
                                  textDecoration = "Bold",
                                  halign = "center")
      openxlsx::write.xlsx(table,
                           paste0(file_name, ".xlsx"),
                           headerStyle = hs,
                           numFmt = "0.00")
      print(paste("The table has been created and is saved at location:",
                  normalizePath(paste0(file_name, ".xlsx"))))
    },
    error = function(e) {
      print("Unfortunately, the export as *.xlsx did not work. The package openxlsx requires that a zip program is registered to R. Here is the original error message:")
      print(e)
      cat("\n")
    },
    finally = return(invisible(table)))
  }
}
