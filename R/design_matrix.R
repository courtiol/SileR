#' Build the design matrix
#'
#' This function is called internally by other functions.
#' It builds the design matrix for the models.
#'
#' @inheritParams parameters
#'
#' @return The design matrix.
#' @export
#'
#' @examples
#' pmat <- build_param_matrix(param_values = c(0.1, 0.2), param_str = c("w1" = "s"))
#' dmat <- build_design_matrix(data = SurvEles_small, param_matrix = pmat)
#' table(dmat[, "w1"])
#'
build_design_matrix <- function(data, param_matrix, indices = NULL, empty_param_matrix_full = NULL, empty_design_matrix = NULL, print = TRUE) {
  if (is.null(indices)) {
    indices <- compute_indices(data = data)
    if (print) {
      warning("creating indices outside the function is more efficient if the function is called multiple times!")
    }
  }
  if (is.null(empty_design_matrix)) {
    design_matrix <- build_empty_design_matrix(data)
    if (print) {
      warning("creating the empty design matrix outside the function is more efficient if the function is called multiple times!")
    }
  } else {
    design_matrix <- empty_design_matrix
  }

  param_matrix_full <- expand_param_matrix(param_matrix, empty_param_matrix_full)

  for (meta_param in c("w1", "b1", "w2", "b2", "w3", "w4", "w5", "b4", "b5")) {
    for (type in attr(param_matrix_full, "param_str")[meta_param]) {
    names_i <- build_param_names(meta_param_name = "i", meta_param_type = type, check = FALSE)
    names_p <- build_param_names(meta_param_name = meta_param, meta_param_type = type)
      for (i in 1:length(names_i)) {
        name_i <- names_i[i]
        name_p <- names_p[i]
        indices_i <- indices[name_i][[1]]
        design_matrix[indices_i, meta_param] <- design_matrix[indices_i, meta_param] + param_matrix_full[name_p, "value"]
      }
    }
  }
  return(design_matrix)
}


#' Create an empty design matrix
#'
#' This function is called internally by other functions.
#' It builds an empty design matrix.
#'
#' @inheritParams parameters
#' @return The empty design matrix.
#' @seealso\code{\link{build_design_matrix}}
#'
#' @export
#'
#' @examples
#' build_empty_design_matrix(SurvEles_small)
#'
build_empty_design_matrix <- function(data) {
  m <- matrix(0, nrow = nrow(data), ncol = 9L)
  colnames(m) <- c("w1", "b1", "w2", "b2", "w3", "w4", "w5", "b4", "b5")
  return(m)
}
