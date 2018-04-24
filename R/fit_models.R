#' Fit all 17 models
#'
#' This is a main function of this package. It fits all main 17 models presented in the paper.
#'
#' @inheritParams parameters
#'
#' @return The list of fitted models.
#' @export
#' @seealso \code{\link{fit}}
#'
#' @examples
#' ## Warning: increase maxtime for real fit!
#' models_test <- fit_all_models(data = SurvEles_small, maxtime = 1)
#' models_test
#'
fit_all_models <- function(data, xtol_rel = 1.0e-7, maxtime = Inf, n_CPU = 2L, save_rda = FALSE){
  param_str_list <- make_param_str_list()
  cl <- parallel::makeCluster(n_CPU)

  ## copy objects in current environment to facilitate the export to nodes:
  ## (It is often not needed but I choose to do that because depending on the
  ## environment in which the object are being defined, the export sometimes fail)
  data_export <- data
  xtol_rel_export <- xtol_rel
  maxtime_export <- maxtime
  save_rda_export <- save_rda

  if (save_rda) {
    if (dir.exists("./models")) {
      stop("You need to delete the folder './models/' before running this function.")
      }
    dir.create("./models")
    }

  pb <- utils::txtProgressBar(max = length(names(param_str_list)), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  cl <- parallel::makeCluster(n_CPU)
  doSNOW::registerDoSNOW(cl)
  mod <- NULL ## to trick R CMD check
  boot <- foreach::foreach(mod = names(param_str_list), .options.snow = opts)

  mod_list <- foreach::`%dopar%`(boot, {
    param_matrix <- build_param_matrix(param_values = 0.01,
                                       param_str = param_str_list[[mod]])
    fit <- fit(data = data_export,
               param_matrix_ini = param_matrix,
               xtol_rel = xtol_rel_export,
               maxtime = maxtime_export)
    if (save_rda_export) {
      assign(mod, fit)
      todo <- paste0("save(", mod, ", file = './models/", mod, ".rda', compress = 'xz')")
      eval(parse(text = todo))
      }
    return(fit)
    })
  parallel::stopCluster(cl)
  if (save_rda) {
    print(paste("The models have been fitted and are saved at location:",
                normalizePath("./models")))
  }
  names(mod_list) <- names(param_str_list)
  return(mod_list)
}


#' Refit all models several times on bootstrapped data
#'
#'
#' This is a main function of this package. It refits each model multiple times
#' after performing a non-parametric bootstrap on the data. This is used to
#' ultimately derive confidence intervals on parameter estimates.
#'
#' @inheritParams parameters
#'
#' @return The list containing all the fits.
#' @export
#' @seealso \code{\link{fit_all_models}}
#'
#' @examples
#' ## Warning: increase n_boot and maxtime for real fits!
#' Models_boot_test <- boot_all_models(models_list = Models[1:2],
#'                                     data = SurvEles_small,
#'                                     n_boot = 2L, maxtime = 1)
#' Models_boot_test
#'
boot_all_models <- function(models_list, data, n_boot = 40L, xtol_rel = 1.0e-7, maxtime = Inf, n_CPU = 2L, save_rda = FALSE, seed = 1L){
  param_str_list <- make_param_str_list()

  ## checks
  if (!all(names(models_list) %in% names(param_str_list))) {
    stop("All models must have a name that corresponds to those used in make_param_str_list().")
    }

  if (save_rda) {
    if (dir.exists("./models/boot/")) {
      stop("You need to delete the folder './models/boot/' before running this function.")
    }
    dir.create("./models/boot/", recursive = TRUE)
  }

  boot_to_do <- expand.grid(replicate = 1:n_boot,
                            mod = names(models_list),
                            stringsAsFactors = FALSE)

  ## setting the RNG seed
  set.seed(seed)

  ## copy objects in current environment to facilitate the export to nodes:
  ## (It is often not needed but I choose to do that because depending on the
  ## environment in which the object are being defined, the export sometimes fail)
  data_export <- data
  xtol_rel_export <- xtol_rel
  maxtime_export <- maxtime

  ## bootstraps
  pb <- utils::txtProgressBar(max = nrow(boot_to_do), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  cl <- parallel::makeCluster(n_CPU)
  doSNOW::registerDoSNOW(cl)
  i <- NULL ## to trick R CMD check
  boot <- foreach::foreach(i = 1:nrow(boot_to_do), .options.snow = opts)

  mod_list <- foreach::`%dopar%`(boot, {
    mod <- boot_to_do[i, "mod"]
    replicate <- boot_to_do[i, "replicate"]
    param_matrix_ini <- build_param_matrix(param_values = 0.01,
                                           param_str = param_str_list[[mod]])
    ID_boot <- sample(levels(data_export$ID), replace = TRUE)
    data_boot <- do.call("rbind", sapply(ID_boot, function(ID) data_export[data_export$ID == ID, ], simplify = FALSE))
    assign(paste0(mod, "_", replicate),
           fit(data = data_boot,
               param_matrix_ini = param_matrix_ini,
               xtol_rel = xtol_rel_export,
               maxtime = maxtime_export))
    if (save_rda) {
      assign(paste0(mod, "_boot_", replicate), get(paste0(mod, "_", replicate)))
      todo <- paste0("save(", mod, "_boot_", replicate, ", file = './models/boot/", mod, "_boot_", replicate,".rda', compress = 'xz')")
      eval(parse(text = todo))
    }
    return(get(paste0(mod, "_", replicate)))
    })
  parallel::stopCluster(cl)
  names(mod_list) <- paste0(boot_to_do[, "mod"], "_", boot_to_do[, "replicate"])
  ## output
  if (save_rda) {
    print(paste("The models have been fitted and are saved at location:",
                normalizePath("./models/boot")))
  }
  return(mod_list)
}
