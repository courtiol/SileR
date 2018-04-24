#' Compute the indices
#'
#' This function is called internally by other functions.
#' It computes indices (i.e. row numbers) referring to different characteristics of the elephants which are required
#' to build the design matrix of the mode.
#'
#' @inheritParams parameters
#'
#' @return A names list of indices.
#' @seealso \code{\link{build_design_matrix}}
#' @export
#'
#' @examples
#' indices <- compute_indices(data = SurvEles_small)
#' lapply(indices, head)  ## display first 6 indices of each element
#'
compute_indices <- function(data){
  l <- list(
    i.males = data$Sex == "males",
    i.females = data$Sex == "females",
    i.1940 = data$BirthCohort == "1940",
    i.1960 = data$BirthCohort == "1960",
    i.1980 = data$BirthCohort == "1980",
    i.2000 = data$BirthCohort == "2000",
    i.Unknown = data$Region == "Unknown",
    i.Ayeyarwa = data$Region == "Ayeyarwa",
    i.Bago = data$Region == "Bago",
    i.Kachin = data$Region == "Kachin",
    i.Magway = data$Region == "Magway",
    i.Mandalay = data$Region == "Mandalay",
    i.Rakhine = data$Region == "Rakhine",
    i.Sagaing = data$Region == "Sagaing",
    i.Shan = data$Region == "Shan",
    i.Tanintha = data$Region == "Tanintha",
    i.males_IMM = (data$Sex == "males" & data$CaptureMethod == "IMM"),
    i.males_MILARSHI = (data$Sex == "males" & data$CaptureMethod == "MILARSHI"),
    i.males_STOCKADE = (data$Sex == "males" & data$CaptureMethod == "STOCKADE"),
    i.females_IMM = (data$Sex == "females" & data$CaptureMethod == "IMM"),
    i.females_MILARSHI = (data$Sex == "females" & data$CaptureMethod == "MILARSHI"),
    i.females_STOCKADE = (data$Sex == "females" & data$CaptureMethod == "STOCKADE")
  )
  l <- lapply(l, which)
  l[["b.captured"]] <- data$CaptureMethod != "CAPTIVE"
  return(l)
}
