#' Definition for parameters
#'
#' Here is the list of all parameters used in the package.
#'
#' @param age_at_capture The age at capture
#' @param ages A vector of ages for which to perform the interpolation
#' @param capture_method A character indicating the method of capture of the elephant: \var{"CAPTIVE"}, \var{"IMM"}, \var{"MILARSHI"} or \var{"STOCKADE"}
#' @param check Whether the arguments should be checked against expected types
#' @param data The dataset to be used
#' @param death_prob A vector of yearly probabilities of death
#' @param design_matrix The design matrix
#' @param empty_design_matrix The empty design matrix
#' @param empty_param_matrix_full The empty expanded or complete parameter matrix
#' @param file_name The file name for the exported table
#' @param fit_original A fitted model
#' @param indices The list of indices computed by \code{compute_indices}
#' @param lb Lower boundary for parameter values during the optimisation (default = 0)
#' @param maxtime Maximum time (in seconds) allowed for nloptr to fit; use Inf for best fit!
#' @param meta_param_name The name of the meta parameter
#' @param meta_param_type The type of subcategories for the meta parameters
#' @param model A fitted model
#' @param models_list The list of fitted models
#' @param models_list_boot The list of fitted models after bootstrap
#' @param n_boot The number of bootstrapped dataset to generate and fit
#' @param n_CPU The number of CPU to use for fitting the bootstrapped datasets
#' @param name_first_column The name of the first column which is taken from the row names of the table
#' @param newdata A data frame providing the covariate values for predictions
#' @param param_matrix The matrix storing parameter values
#' @param param_matrix_full The expanded or complete parameter matrix
#' @param param_matrix_ini The matrix storing initial values for the parameters
#' @param param_names A vector of parameter names
#' @param param_str The named vector indicating the parameter structure, that is, the type of subcategory for each meta parameter
#' @param param_values The vector of values for the parameters
#' @param pred_prob The predicted probability of death computed by \code{predictor()}
#' @param print A boolean indicating whether or not to print information about potential issues
#' @param print_level Printing setting for nloptr
#' @param save_pdf A boolean indicating whether saving the plot as a *.pdf object or not
#' @param save_rda A boolean indicating whether saving the model as a *.rda object or not
#' @param save_xlsx A boolean indicating whether saving the table as a *.xlsx document or not
#' @param scale The scaling parameter (usefull if the optimisation function can only minimise)
#' @param seed An integer giving the seed for the (pseudo-)random generator
#' @param sex A character indicating the sex of the elephant: \var{"males"} or \var{"females"}
#' @param steps An integer giving the number of increments for the interpolation (within each year)
#' @param surv_bin The binary response variable taking 1 for alive and 0 for death
#' @param table The table to be exported as *.xlsx
#' @param ub Upper boundary for parameter values during the optimisation (default = 3)
#' @param xtol_rel Tolerance parameter for nloptr
#'
parameters <- function(age_at_capture,
                       ages,
                       capture_method,
                       check,
                       data,
                       death_prob,
                       design_matrix,
                       empty_design_matrix,
                       empty_param_matrix_full,
                       file_name,
                       fit_original,
                       indices,
                       lb,
                       maxtime,
                       meta_param_name,
                       meta_param_type,
                       model,
                       models_list,
                       models_list_boot,
                       n_boot,
                       n_CPU,
                       name_first_column,
                       newdata,
                       param_matrix,
                       param_matrix_full,
                       param_matrix_ini,
                       param_names,
                       param_str,
                       param_values,
                       pred_prob,
                       print,
                       print_level,
                       save_pdf,
                       save_rda,
                       save_xlsx,
                       scale,
                       seed,
                       sex,
                       steps,
                       surv_bin,
                       table,
                       ub,
                       xtol_rel) {
  NULL
  }
