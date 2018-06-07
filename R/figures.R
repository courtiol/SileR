#' Plot the baseline mortality rate
#'
#' This is a main function of this package. It plots the baseline mortality rate.
#'
#' @inheritParams parameters
#'
#' @return Nothing, the function only plots.
#' @export
#'
#' @examples
#' plot_baseline(data = SurvEles_small, models_list = Models)
#'
plot_baseline <- function(data, models_list, save_pdf = FALSE) {

  if (save_pdf) {
    print("The plot is being build...")
    grDevices::pdf("plot_baseline.pdf", width = 7, height = 7, fonts = "sans")
  }

  graphics::par(mfrow = c(1, 1))
  graphics::par(las = 1, cex = 2, mar = c(4.5, 4.5, 1.5, 1), oma = c(0, 0, 0, 0), mgp = c(3.5, 1, 0))
  cex_legend <- 0.7

  ages_for_predictions_males   <- seq(0, max(data[data$Sex == "males" & data$CaptureMethod == "CAPTIVE", "Age"]), 0.01)
  ages_for_predictions_females <- seq(0, max(data[data$Sex == "females" & data$CaptureMethod == "CAPTIVE", "Age"]), 0.01)

  newdata_captive_males   <- data.frame(Age = ages_for_predictions_males, Sex = "males", Region = "Sagaing", BirthCohort = "1980", CaptureMethod = "CAPTIVE", TimeSinceCapture = 0)
  newdata_captive_females <- data.frame(Age = ages_for_predictions_females, Sex = "females", Region = "Sagaing", BirthCohort = "1980", CaptureMethod = "CAPTIVE", TimeSinceCapture = 0)

  predictions_captive_males   <- predict_avg(models_list = models_list, newdata = newdata_captive_males)
  predictions_captive_females <- predict_avg(models_list = models_list, newdata = newdata_captive_females)

  graphics::plot(predictions_captive_males ~ ages_for_predictions_males, ylim = c(0, 0.15),
                 type = "l", lwd = 2, ylab = "Baseline mortality rate", xlab = "Age (yrs)", col = "black", axes = FALSE, xlim = c(0, 55))
  graphics::points(predictions_captive_females ~ ages_for_predictions_females, type = "l", lwd = 2, col = "darkgrey")

  ages_for_males <- unique(round(ages_for_predictions_males))
  graphics::points(sapply(ages_for_males, function(x) 1 - mean(data[data$Sex == "males" & data$Age == x, "Alive"])) ~
                   ages_for_males,
                   type = "p",
                   cex = 1/1000*sapply(ages_for_males,
                                       function(x) length(data[data$Sex == "males" & data$Age == x, "Alive"])))

  ages_for_females <- unique(round(ages_for_predictions_females))
  graphics::points(sapply(ages_for_females, function(x) 1 - mean(data[data$Sex == "females" & data$Age == x, "Alive"]))~
                   ages_for_females,
                   type = "p",
                   col = "darkgrey",
                   cex = 1/1000*sapply(ages_for_females,
                                       function(x) length(data[data$Sex == "females" & data$Age == x, "Alive"])))

  graphics::legend("topright", fill = c("black", "darkgrey"), cex = cex_legend, title = "Sex:",
                   legend = c("males", "females"), bty = "n")
  graphics::legend("top", pt.cex = 1/1000*c(1500, 1000, 500, 100), pch = rep(1, 4), cex = cex_legend, title = "Sample size:",
                   legend = c(1500, 1000, 500, 100), bty = "n")

  graphics::axis(2, at = seq(0, 0.15, 0.025))
  graphics::axis(1, at = seq(0, 60, 10))
  graphics::box()

  if (save_pdf) {
    grDevices::dev.off()
    print(paste("The pdf of the plot has been created and is saved at location:",
                normalizePath(paste0("plot_baseline.pdf"))))
  }
  return(invisible(NULL))
}


#' Prepare the data for plotting the effect of capture on the mortality rate
#'
#' This function is called internally by other functions. It prepares the data
#' for the function \code{\link{plot_capture}}.
#'
#' @inheritParams parameters
#'
#' @return A list containing the data required for the plotting function.
#' @export
#'
#' @seealso \code{\link{plot_capture}}
#'
#' @examples
#' res <- prepare_data_for_capture_plot(models_list = Models,
#'                                      data = SurvEles_small,
#'                                      age_at_capture = 5,
#'                                      sex = "males",
#'                                      capture_method = "IMM")
#' lapply(res, range)
#'
prepare_data_for_capture_plot <- function(models_list, data, age_at_capture = 0, sex, capture_method) {

  raw_ages_for_predictions   <- seq(0, max(data[data$Sex == sex & data$CaptureMethod == capture_method, "Age"]), 0.01)
  ages_for_predictions       <- raw_ages_for_predictions[raw_ages_for_predictions >=  age_at_capture]
  time_since_cap   <- seq(from = 0, by = 0.01, length.out = length(ages_for_predictions))

  newdata_CAPTIVE   <- data.frame(Age = ages_for_predictions, Sex = sex, Region = "Mandalay", BirthCohort = "1980", TimeSinceCapture = 0, CaptureMethod = "CAPTIVE")
  newdata_immediate <- data.frame(Age = ages_for_predictions, Sex = sex, Region = "Mandalay", BirthCohort = "1980", TimeSinceCapture = 0, CaptureMethod = capture_method)
  newdata_delayed   <- data.frame(Age = ages_for_predictions, Sex = sex, Region = "Mandalay", BirthCohort = "1980", TimeSinceCapture = time_since_cap, CaptureMethod = capture_method)

  pred_CAPTIVE   <- predict_avg(models_list = models_list, newdata = newdata_CAPTIVE)
  immediate_cost <- predict_avg(models_list = models_list, newdata = newdata_immediate) - pred_CAPTIVE
  delayed_cost   <- predict_avg(models_list = models_list, newdata = newdata_delayed) - pred_CAPTIVE

  output <- list(age = ages_for_predictions,
                 time_since_cap = time_since_cap,
                 immediate_cost = immediate_cost,
                 delayed_cost = delayed_cost)
  return(output)
}


#' Plot the effect of capture on the mortality rate
#'
#' This is a main function of this package. It plots the effect of capture on the mortality rate.
#'
#' @inheritParams parameters
#'
#' @return Nothing, the function only plots.
#' @export
#'
#' @examples
#' plot_capture(data = SurvEles_small, models_list = Models)
#'
plot_capture <- function(data, models_list, save_pdf = FALSE) {

  if (save_pdf) {
    print("The plot is being build...")
    grDevices::pdf("plot_capture.pdf", width = 7, height = 7/2, fonts = "sans")
  }

  graphics::par(mfrow = c(1, 2), las = 1, mar = c(4.5, 4.5, 1.5, 1), oma = c(0, 0, 0, 0), mgp = c(3.5, 1, 0))

  cex <- 1
  cex_axis <- cex
  cex_lab <- cex
  cex_legend <- 0.7*cex

  sex <- "males"

  IMM_0 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 0, sex = sex, capture_method = "IMM")
  MILARSHI_0 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 0, sex = sex, capture_method = "MILARSHI")
  STOCKADE_0 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 0, sex = sex, capture_method = "STOCKADE")

  IMM_5 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 5, sex = sex, capture_method = "IMM")
  MILARSHI_5 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 5, sex = sex, capture_method = "MILARSHI")
  STOCKADE_5 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 5, sex = sex, capture_method = "STOCKADE")

  IMM_20 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 20, sex = sex, capture_method = "IMM")
  MILARSHI_20 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 20, sex = sex, capture_method = "MILARSHI")
  STOCKADE_20 <- prepare_data_for_capture_plot(models_list = models_list, data = data, age_at_capture = 20, sex = sex, capture_method = "STOCKADE")

  graphics::plot(immediate_cost ~ age, data = IMM_0, xlim = c(0, 55),
                 type = "l", lwd = 1.5, ylab = "Increase in mortality at year of capture",
                 xlab = "Age at capture (yrs)", las = 1, lty = 4, col = "black", axes = FALSE,
                 cex.lab = cex_lab)
  graphics::points(immediate_cost ~ age, data = MILARSHI_0, type = "l", lwd = 1.5, col = "black", lty = 2)
  graphics::points(immediate_cost ~ age, data = STOCKADE_0, type = "l", lwd = 3.5, col = "black", lty = 3)

  graphics::box()
  graphics::axis(2, cex.axis = cex_axis)
  graphics::axis(1, at = seq(0, 50, 10), cex.axis = cex_axis)
  graphics::legend("bottomright", legend = c("immobilization", "milarshikar", "stockade"),
                   col = rep(1, 3), bty = "n", lty = c(4,2,3), lwd = c(rep(1.5, 2), 3.5),
                   cex = cex_legend, title = "Capture method:")

  graphics::mtext("a", cex = 1, at = 0, line = 0, font = 1, las = 1)

  graphics::plot(delayed_cost ~ time_since_cap, data = IMM_5,
                 type = "l", lwd = 1.5, ylab = "Increase in mortality after capture", xlab = "Time since capture (yrs)",
                 las = 1, lty = 4, col = "black", log = "y",  ylim = c(1e-3, 0.05), xlim = c(0, 13), axes = FALSE,
                 cex.lab = cex_lab)
  graphics::points(delayed_cost ~ time_since_cap, data = IMM_20, type = "l", lwd = 1.5, lty = 4, col = "darkgrey")

  graphics::points(delayed_cost ~ time_since_cap, data = MILARSHI_5, type = "l", lwd = 1.5, col = "black", lty = 2)
  graphics::points(delayed_cost ~ time_since_cap, data = MILARSHI_20, type = "l", lwd = 1.5, col = "darkgrey", lty = 2)

  graphics::points(delayed_cost ~ time_since_cap, data = STOCKADE_5, type = "l", lwd = 3.5, col = "black", lty = 3)
  graphics::points(delayed_cost ~ time_since_cap, data = STOCKADE_20, type = "l", lwd = 3.5, col = "darkgrey", lty = 3)

  graphics::legend("topright", fill = c("black", "darkgrey"), cex = cex_legend, title = "Age at capture:",
                   legend = c("5 yrs", "20 yrs"), bty = "n")
  graphics::box()
  graphics::axis(2, cex.axis = cex_axis)
  graphics::axis(1, at = seq(0, 20, 2), cex.axis = cex_axis)
  graphics::mtext("b", cex = 1, at = 0, line = 0, font = 1, las = 1)

  if (save_pdf) {
    grDevices::dev.off()
    print(paste("The pdf of the plot has been created and is saved at location:",
                normalizePath(paste0("plot_capture.pdf"))))
  }

  return(invisible(NULL))
}



#' Plot demographic information for the captured elephants
#'
#' This is a main function of this package. It plots information about when
#' elephants have been captured.
#'
#' @inheritParams parameters
#'
#' @return Nothing, the function only plots.
#' @export
#'
#' @seealso ElesCaptured
#'
#' @examples
#' plot_captured_demog(ElesCaptured)
#'
plot_captured_demog <- function(data, save_pdf = FALSE){

  if (save_pdf) {
    print("The plot is being build...")
    grDevices::pdf("plot_capture_demog.pdf", width = 7, height = 7/2, fonts = "sans")
  }

  data_age <- data$ElesCapturedByAge
  graphics::par(mfrow = c(1, 2))
  graphics::par(las = 1, cex = 0.8, mar = c(4.5,4.5,1.5,1), oma = c(0,0,0,0), mgp = c(3.5,1,0))

  cex <- 1
  cex_axis <- cex
  cex_lab <- cex
  cex_legend <- 0.7*cex

  ages <- 0:max(data_age$CaptureAge)
  N_IMM <- with(data_age[data_age$CaptureMethod == "IMM", ], tapply(N, CaptureAge, sum))
  N_MILARSHI <- with(data_age[data_age$CaptureMethod == "MILARSHI", ], tapply(N, CaptureAge, sum))
  N_STOCKADE <- with(data_age[data_age$CaptureMethod == "STOCKADE", ], tapply(N, CaptureAge, sum))
  graphics::plot(N_IMM ~ ages, type = "o", ylim = c(0, 100), xlim = c(0, 55),
                 lty = 4, pch = 21, bg = "black", cex.axis = cex_axis,
                 xlab = "Age at capture (yrs)", ylab = "Total no. captures", axes = FALSE)
  graphics::points(N_MILARSHI ~ ages, type = "o", lty = 2)
  graphics::points(N_STOCKADE ~ ages, type = "o", lty = 3, pch = 25, bg = "black")
  graphics::legend("topright", legend = c("immobilization", "milarshikar", "stockade"),
         col = rep(1, 3), bty = "n", lty = c(4, 2, 3),
         pch = c(21, 1, 25),
         pt.bg = c("black", "black", "black"),
         cex = cex_legend, title = "Capture method:")
  graphics::axis(1, at = seq(0, 50, 10), cex.axis = cex_axis)
  graphics::axis(2, cex.axis = cex_axis)
  graphics::box()
  graphics::mtext("a", cex = 1, at = 0, line = 0, font = 1, las = 1)

  data_decade <- data$ElesCapturedByDecade
  M <- matrix(data_decade$N, nrow = 5, byrow = FALSE)
  colnames(M) <- c("IMM", "MILARSHI", "STOCKADE")
  rownames(M) <- c("1950", "1960", "1970", "1980", "1990")
  graphics::barplot(t(M), ylab = "Total no. captures", xlab = "Decade of the capture event",
          ylim = c(0, 1000), col = c("black", "lightgrey", "darkgrey"), las = 1,
          cex.axis = cex_axis, cex.lab = cex_lab)
  graphics::legend("topright", legend = c("immobilization", "milarshikar", "stockade"),
         bty = "n",
         fill = c("black", "lightgrey", "darkgrey"),
         cex = cex_legend, title = "Capture method:")
  graphics::box()
  graphics::mtext("b", cex = 1, at = 0.15, line = 0, font = 1, las = 1)

  if (save_pdf) {
    grDevices::dev.off()
    print(paste("The pdf of the plot has been created and is saved at location:",
                normalizePath(paste0("plot_capture_demog.pdf"))))
  }

  return(invisible(NULL))
}


#' Plot the survivorship curves
#'
#' This is a main function of this package. It plots the survivorship curve for
#' each capture method.
#'
#' The survivorship curves are averages of individual predictions matching each
#' observation. They thus capture the effects of differences in sample size
#' according to birth cohorts, regions, time since capture... between
#' different capture methods.
#'
#' @inheritParams parameters
#'
#' @return Nothing, the function only plots.
#' @export
#'
#' @examples
#' plot_survivorship(data = SurvEles_small, models_list = Models)
#'
plot_survivorship <- function(data, models_list, save_pdf = FALSE) {
  if (save_pdf) {
    print("The plot is being build...")
    grDevices::pdf("plot_survivorship.pdf", width = 7, height = 7, fonts = "sans")
  }

  graphics::par(mfrow = c(1, 1))
  graphics::par(cex = 2, las = 1, mar = c(4.5, 4.5, 1.5, 1), oma = c(0, 0, 0, 0), mgp = c(3.5, 1, 0))
  cex_legend <- 0.7

  ## predictions for all observations
  all_pred <- predict_avg(models_list = models_list, newdata = data)

  ### processing survival
  pred_captive_males <- pred_imm_males <- pred_mila_males <- pred_stock_males <- rep(NA, 54)
  pred_captive_females <- pred_imm_females <- pred_mila_females <- pred_stock_females <- rep(NA, 54)

  for (i in 1:56) {
    index_captive_m <- with(data, Sex == "males" & CaptureMethod == "CAPTIVE" & Age == (i - 1))
    index_imm_m <- with(data, Sex == "males" & CaptureMethod == "IMM" & Age == (i - 1))
    index_mila_m <- with(data, Sex == "males" & CaptureMethod == "MILARSHI" & Age == (i - 1))
    index_stock_m <- with(data, Sex == "males" & CaptureMethod == "STOCKADE" & Age == (i - 1))
    index_captive_f <- with(data, Sex == "females" & CaptureMethod == "CAPTIVE" & Age == (i - 1))
    index_imm_f <- with(data, Sex == "females" & CaptureMethod == "IMM" & Age == (i - 1))
    index_mila_f <- with(data, Sex == "females" & CaptureMethod == "MILARSHI" & Age == (i - 1))
    index_stock_f <- with(data, Sex == "females" & CaptureMethod == "STOCKADE" & Age == (i - 1))

    pred_captive_males[i] <- mean(all_pred[index_captive_m])
    pred_imm_males[i] <- mean(all_pred[index_imm_m])
    pred_mila_males[i] <- mean(all_pred[index_mila_m])
    pred_stock_males[i] <- mean(all_pred[index_stock_m])
    pred_captive_females[i] <- mean(all_pred[index_captive_f])
    pred_imm_females[i] <- mean(all_pred[index_imm_f])
    pred_mila_females[i] <- mean(all_pred[index_mila_f])
    pred_stock_females[i] <- mean(all_pred[index_stock_f])

    if (i < 11) { ## young individuals before capture are assumed to have same survival as captive borns
      if (is.na(pred_captive_females[i])) pred_captive_females[i] <- pred_captive_males[i]
      if (is.na(pred_imm_males[i])) pred_imm_males[i] <- pred_captive_males[i]
      if (is.na(pred_mila_males[i])) pred_mila_males[i] <- pred_captive_males[i]
      if (is.na(pred_stock_males[i])) pred_stock_males[i] <- pred_captive_males[i]
      if (is.na(pred_imm_females[i])) pred_imm_females[i] <- pred_captive_females[i]
      if (is.na(pred_mila_females[i])) pred_mila_females[i] <- pred_captive_females[i]
      if (is.na(pred_stock_females[i])) pred_stock_females[i] <- pred_captive_females[i]
    }
  }

  ## computing survivorship
  surv_captive_males   <- rbind(c(0, 1), interpol_surv(pred_captive_males, ages = 0:55))
  surv_captive_females <- rbind(c(0, 1), interpol_surv(pred_captive_females, ages = 0:55))
  surv_imm_males       <- rbind(c(0, 1), interpol_surv(pred_imm_males, ages = 0:55))
  surv_imm_females     <- rbind(c(0, 1), interpol_surv(pred_imm_females, ages = 0:55))
  surv_stock_males     <- rbind(c(0, 1), interpol_surv(pred_stock_males, ages = 0:55))
  surv_stock_females   <- rbind(c(0, 1), interpol_surv(pred_stock_females, ages = 0:55))
  surv_mila_males      <- rbind(c(0, 1), interpol_surv(pred_mila_males, ages = 0:55))
  surv_mila_females    <- rbind(c(0, 1), interpol_surv(pred_mila_females, ages = 0:55))

  ## plotting
  graphics::plot(Surv ~ Age, data = surv_captive_males, axes = FALSE, lwd = 1.5, lty = 1, type = "l",
                 xlim = c(0, 55), ylim = c(0, 1), ylab = "Survivorship", xlab = "Age (yrs)")
  graphics::points(Surv ~ Age, data = surv_imm_males, type = "l", lty = 4, lwd = 1.5)
  graphics::points(Surv ~ Age, data = surv_mila_males, type = "l", lty = 2, lwd = 1.5)
  graphics::points(Surv ~ Age, data = surv_stock_males, type = "l", lty = 3, lwd = 3.5)
  graphics::points(Surv ~ Age, data = surv_captive_females, type = "l", lty = 1, lwd = 1.5, col = "darkgrey")
  graphics::points(Surv ~ Age, data = surv_imm_females,type = "l", lty = 4, lwd = 1.5, col = "darkgrey")
  graphics::points(Surv ~ Age, data = surv_mila_females, type = "l", lty = 2, lwd = 1.5, col = "darkgrey")
  graphics::points(Surv ~ Age, data = surv_stock_females, type = "l", lty = 3, lwd = 3.5, col = "darkgrey")

  graphics::legend("bottomleft",
                   legend = c("immobilization", "milarshikar", "stockade", "none (captive born)"),
                   bty = "n",
                   lty = c(4, 2, 3, 1),
                   lwd = c(1.5, 1.5, 3.5, 1.5),
                   title = "Capture method:",
                   cex = cex_legend)

  graphics::legend("topright",
                   legend = c("males", "females"),
                   bty = "n",
                   fill = c("black", "darkgrey"),
                   title = "Sex:",
                   cex = cex_legend)

  graphics::axis(2, at = seq(0, 1, 0.2))
  graphics::axis(1, at = seq(0, 55, 10))
  graphics::box()

  if (save_pdf) {
    grDevices::dev.off()
    print(paste("The pdf of the plot has been created and is saved at location:",
                normalizePath(paste0("plot_capture_demog.pdf"))))
  }

  return(invisible(NULL))
}
