#' Build the parameter names
#'
#' This function is called internally by other functions. It creates the names
#' of the parameters based on the name of the meta parameter and its type. For
#' example, if the meta parameter "w1" must take a different value for males and
#' females (i.e. type = "s"), then the function will create the parameter names
#' "w1.males" and "w1.females".
#'
#' For a given meta parameter, we only implemented the different types which we
#' consider relevant for our purpose: the meta parameters "w1", "b2", "w2", "b2"
#' can take the types "0", "1", or "s" (value fixed at zero, a unique parameter
#' value, and one parameter value per sex); "w3" can either be "0+t+l", "1+t+l",
#' or "s+t+l" (one parameter fixed at 0, or a unique parameter, or one per sex,
#' followed by one parameter value per birth cohort and location); "w4", "w5",
#' "b4" and "b5 can take "0", "1", "s", "c" (value fixed at zero, a unique
#' value, one per sex, one per capture method). (The syntax "s:c" is also used
#' for creating sex capture-method combination but should be only used internally.)
#'
#' @inheritParams parameters
#'
#' @return A vector of parameter names.
#' @export
#'
#' @examples
#' build_param_names(meta_param_name = "w1", meta_param_type = "0")
#' build_param_names(meta_param_name = "w1", meta_param_type = "1")
#' build_param_names(meta_param_name = "w1", meta_param_type = "s")
#' build_param_names(meta_param_name = "w3", meta_param_type = "1+t+l")
#' #build_param_names(meta_param_name = "w3", meta_param_type = "t+l") ## error as it should
#' build_param_names(meta_param_name = "w3", meta_param_type = "t+l", check = FALSE)
#' #build_param_names(meta_param_name = "w3",
#' #                 meta_param_type = "something_wrong") ## error as it should
#'
build_param_names <- function(meta_param_name, meta_param_type, check = TRUE){

  ## checks
  if (check) {
    if (meta_param_name %in% c("w1", "b1", "w2", "b2") & !meta_param_type %in% c("0", "1", "s"))
      stop("The parameter can only be '0', '1' or 's'.")
    if (meta_param_name == "w3" & !meta_param_type %in% c("0+t+l", "1+t+l", "s+t+l"))
      stop(paste("The parameter cannot be", meta_param_type, "but only '0+t+l', '1+t+l', or 's+t+l'."))
    if (meta_param_name %in% c("w4", "w5", "b4", "b5") & !meta_param_type %in% c("0", "1", "s", "c", "s:c"))
      stop("The parameter can only be '0', '1', 's', 'c', 's:c'.")
  }

  ## build names
  ## Note: do not change the order unless unless all models are refitted with new code.
  f <- "fix"
  u <- "unique"
  s <- c("females", "males")
  t <- c("2000", "1940", "1960", "1980")
  l <- c("Unknown", "Ayeyarwa", "Bago", "Kachin", "Magway", "Mandalay", "Rakhine", "Sagaing", "Shan", "Tanintha")
  c <- c("IMM", "MILARSHI", "STOCKADE")


  names <- switch(meta_param_type,
                  "0" = f,
                  "1" = u,
                  "s" = s,
                  "t" = t,
                  "l" = l,
                  "t+l" = c(t, l), ## needed for expand_param_matrix()
                  "0+t+l" = c(f, t, l),
                  "1+t+l" = c(u, t, l),
                  "s+t+l" = c(s, t, l),
                  "c" = c,
                  "s:c" = apply(expand.grid(s, c), 1, function(i) paste(i[1], i[2], sep = "_")) ## needed for expand_param_matrix()
                  )

  ## ouput
  paste(meta_param_name, names, sep = ".")
}

#' Build the matrix storing parameters
#'
#' This function is called internally by other functions.
#' It initializes the matrix storing parameter values. This function
#' is used to set the initial parameter values before optimisation and to update
#' the parameter values during optimisation. The matrix created by the function
#' contains as many rows as the number of parameters and one column storing the
#' parameter values. The parameter names are stored as row names. The matrix also
#' contains the completed attribute \code{param_str}, used internally.
#'
#' @inheritParams parameters
#' @inherit build_param_names details
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' build_param_matrix()
#' build_param_matrix(param_values = 0.1)
#' build_param_matrix(param_values = 0.1, param_str = c("b1" = "s", "w3" = "1+t+l", w4 = "c"))
#' build_param_matrix(param_values = c(0.1, 0.2, 0.4, 0.3),
#'                    param_str = c("b1" = "s", "b2" = "s"))
#' build_param_matrix(param_values = 0.1,
#'                    param_str = c("w1" = "1",
#'                                  "b1" = "s",
#'                                  "w2" = "1",
#'                                  "b2" = "1",
#'                                  "w3" = "1+t+l",
#'                                  "w4" = "1",
#'                                  "w5" = "1",
#'                                  "b4" = "1",
#'                                  "b5" = "1"))
#'
#'
build_param_matrix <- function(param_values = 0, param_str = NULL){
  ## mind that param_values must be given in the right order!

  stopifnot(all(names(param_str) %in% c("w1", "b1", "w2", "b2", "w3", "w4", "w5", "b4", "b5")))

  ## build parameter names
  param_str_ini <- c("w1" = "0", "b1" = "0", "w2" = "0", "b2" = "0", "w3" = "0+t+l",
                     "w4" = "0", "w5" = "0", "b4" = "0", "b5" = "0")

  param_str_ini[names(param_str)] <- param_str

  param_names_input <- as.character(unlist(sapply(names(param_str), function(p)
    build_param_names(meta_param_name = p, meta_param_type = param_str[p]))))

  param_names_input <- param_names_input[!endsWith(param_names_input, "fix")]

  param_names_all <- as.character(unlist(sapply(names(param_str_ini), function(p)
    build_param_names(meta_param_name = p, meta_param_type = param_str_ini[p]))))

  ## build parameter values (code a little complex due to inputs check)
  if (length(param_values) == 1 & (is.null(param_str) | length(param_str) == length(param_str_ini))) {
    param_values_all <- param_values
  } else if (length(param_values) == 1 | (length(param_values) == length(param_names_input))) {
    param_values_all <- rep(0, length(param_names_all))
    names(param_values_all) <- param_names_all
    param_values_all[param_names_input] <- param_values
  } else {
    stop(paste("The inputs are not consistent, expecting", length(param_names_input), "values."))
  }

  ## prepare ouput
  m <- matrix(param_values_all,
              ncol = 1, nrow = length(param_names_all),
              dimnames = list(param_names_all, "value"))

  ## trim fixed parameters
  m_clean <- m[!endsWith(param_names_all, "fix"), ,drop = FALSE]
  attr(m_clean, "param_str") <- param_str_ini

  ## output
  return(m_clean)
}


#' Expand the parameter matrix
#'
#' This function is called internally by other functions.
#' Since the parameter matrix created with \code{build_param_matrix} only
#' contains the parameter values for the parameter that needs to be optimised,
#' the parameter matrix varies in length. This causes troubles to work with the
#' other functions, so this function expands the matrix to its full size by
#' replicating parameter values for the different parameters of the same meta
#' parameter which have not been defined when building the parameter matrix (see
#' example).
#'
#' @inheritParams parameters
#'
#' @return The expanded parameter matrix.
#' @export
#'
#' @examples
#' pmat <- build_param_matrix(param_values = seq(0.01, 0.21, length = 21),
#'                            param_str = c("w1" = "1", "w3" = "1+t+l", "w4" = "s", "w5" = "c"))
#' pmat
#' expand_param_matrix(pmat)
#'
expand_param_matrix <- function(param_matrix, param_matrix_full = NULL) {

  full_str <- c("w1" = "s", "b1" = "s", "w2" = "s", "b2" = "s", "w3" = "s+t+l",
                "w4" = "s:c", "w5" = "s:c", "b4" = "s:c", "b5" = "s:c")
  if (is.null(param_matrix_full)) {
    param_matrix_full <- build_param_matrix(param_values = 0, param_str = full_str)
  }

  param_names_ini  <- rownames(param_matrix)
  param_names_full <- rownames(param_matrix_full)

  for (metaparam in names(full_str)) {
    index_ini  <- startsWith(param_names_ini, metaparam)
    index_full <- startsWith(param_names_full, metaparam)
    if (sum(index_ini) == 0) next()
    if (metaparam %in% c("w1", "b1", "w2", "b2")) {
      param_matrix_full[index_full, "value"] <- param_matrix[index_ini, "value"]
    } else if (metaparam == "w3") {
      if (any(rownames(param_matrix) == "w3.unique")) {
        param_matrix_full[c("w3.males", "w3.females"), "value"] <- param_matrix["w3.unique", "value"]
      }
      other_w3_param <- build_param_names(meta_param_name = "w3", meta_param_type = "t+l", check = FALSE)
      param_matrix_full[other_w3_param, "value"] <- param_matrix[other_w3_param, "value"]
    } else if (metaparam %in% c("w4", "w5", "b4", "b5")) {
      type <- attr(param_matrix, "param_str")[metaparam]
      if (type == "1") {
         param_matrix_full[index_full, "value"] <- param_matrix[index_ini, "value"]
       } else if (type == "s") {
         index_full_male   <- startsWith(param_names_full, paste0(metaparam, ".males"))
         index_full_female <- startsWith(param_names_full, paste0(metaparam, ".females"))
         param_matrix_full[index_full_male, "value"] <- param_matrix[paste0(metaparam, ".males"), "value"]
         param_matrix_full[index_full_female, "value"] <- param_matrix[paste0(metaparam, ".females"), "value"]
       } else if (type %in% c("c", "s:c")) {
         index_full_IMM       <- grepl(paste0("^", metaparam, ".*IMM"), x = param_names_full, fixed = FALSE)
         index_full_MILARSHI  <- grepl(paste0("^", metaparam, ".*MILARSHI"), x = param_names_full, fixed = FALSE)
         index_full_STOCKADE  <- grepl(paste0("^", metaparam, ".*STOCKADE"), x = param_names_full, fixed = FALSE)
         index_ini_IMM       <- grepl(paste0("^", metaparam, ".*IMM"), x = param_names_ini, fixed = FALSE)
         index_ini_MILARSHI  <- grepl(paste0("^", metaparam, ".*MILARSHI"), x = param_names_ini, fixed = FALSE)
         index_ini_STOCKADE  <- grepl(paste0("^", metaparam, ".*STOCKADE"), x = param_names_ini, fixed = FALSE)
         param_matrix_full[index_full_IMM, "value"] <- param_matrix[index_ini_IMM, "value"]
         param_matrix_full[index_full_MILARSHI, "value"] <- param_matrix[index_ini_MILARSHI, "value"]
         param_matrix_full[index_full_STOCKADE, "value"] <- param_matrix[index_ini_STOCKADE, "value"]
       }
    } else {
      stop("A least one parameter is unknown.")
    }
  }
  param_matrix_full
}


#' This function selects the names of parameters belonging to the same meta parameter
#'
#' This function is called internally by other functions. We distinguish meta
#' parameters (e.g. "w1") from actual parameters (e.g. "w1.females",
#' "w1.males"). This function allows for the retrieval of all parameters that
#' have the same meta parameter name. If no name is given for the meta
#' parameter, the function extracts the meta parameter names from the parameter
#' names.
#'
#' @inheritParams parameters
#' @return A vector of parameter names.
#' @export
#' @seealso \code{\link{get_param_names_without_meta}}
#' @examples
#' get_param_name(meta_param_name = "w1", param_names = rownames(Models$mod1$param_matrix_full))
#'
get_param_name <- function(meta_param_name = NULL, param_names) {
  meta_param_names <- unlist(lapply(sapply(param_names, function(param_name) strsplit(param_name, split = ".", fixed = TRUE)), function(i) i[1]))
  if (is.null(meta_param_name)) {
    return(meta_param_names)
  }
  if (length(meta_param_name) > 1) {
    stop("meta_name must be of length 1")
  }
  return(param_names[meta_param_names == meta_param_name])
}


#' This function trims out the meta suffix from the names of parameters
#'
#' This function is called internally by other functions. It is a simple
#' utility function.
#'
#' @inheritParams parameters
#' @return A vector of names.
#' @export
#' @seealso \code{\link{get_param_name}}
#' @examples
#' get_param_names_without_meta(param_names = rownames(Models$mod1$param_matrix_full)[1:2])
#'
get_param_names_without_meta <- function(param_names) {
  params_names <- unlist(lapply(sapply(param_names, function(param_name) strsplit(param_name, split = ".", fixed = TRUE)), function(i) i[2]))
  names(params_names) <- NULL
  return(params_names)
}


#' Create the list of parameter structures
#'
#' This function is called internally by other functions. It creates the
#' attributes \code{param_str} for each of the 17 models to be fitted.
#'
#' @return The list storing the parameter structure for all 17 models.
#' @export
#'
#' @examples
#' make_param_str_list()
#'
make_param_str_list <- function() {
  param_str_list <- list(
    mod1  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "c",   w5 = "c",   b4 = "c",   b5 = "c"),
    mod2  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "c",   w5 = "c",   b4 = "c",   b5 = "0"),
    mod3  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "c",   w5 = "0",   b4 = "c",   b5 = "0"),
    mod4  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "c",   w5 = "0",   b4 = "c",   b5 = "c"),

    mod5  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "s",   w5 = "s",   b4 = "s",   b5 = "s"),
    mod6 = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "s",   w5 = "s",   b4 = "s",   b5 = "0"),
    mod7 = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "s",   w5 = "0",   b4 = "s",   b5 = "0"),
    mod8 = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "s",   w5 = "0",   b4 = "s",   b5 = "s"),

    mod9  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "1",   w5 = "1",   b4 = "1",   b5 = "1"),
    mod10  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "1",   w5 = "1",   b4 = "1",   b5 = "0"),
    mod11  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "1",   w5 = "0",   b4 = "1",   b5 = "0"),
    mod12  = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "1",   w5 = "0",   b4 = "1",   b5 = "1"),

    mod13 = c(w1 = "s", b1 = "s", w2 = "s", b2 = "s", w3 = "s+t+l", w4 = "0",   w5 = "0",   b4 = "0",   b5 = "0"),

    mod14 = c(w1 = "1", b1 = "1", w2 = "1", b2 = "1", w3 = "0+t+l", w4 = "0",   w5 = "0",   b4 = "0",   b5 = "0"),
    mod15 = c(w1 = "1", b1 = "1", w2 = "0", b2 = "0", w3 = "0+t+l", w4 = "0",   w5 = "0",   b4 = "0",   b5 = "0"),
    mod16 = c(w1 = "0", b1 = "0", w2 = "1", b2 = "1", w3 = "0+t+l", w4 = "0",   w5 = "0",   b4 = "0",   b5 = "0"),
    mod17 = c(w1 = "0", b1 = "0", w2 = "0", b2 = "0", w3 = "0+t+l", w4 = "0",   w5 = "0",   b4 = "0",   b5 = "0")
  )
  return(param_str_list)
}
