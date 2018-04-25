#' A subset of the dataset for elephant survival
#'
#' This dataset contains 100 rows of the 83026 ones used in the real analysis.
#' Unfortunately, we cannot share the original data due to property restriction.
#' The subset can be used to test all our functions. It has the same structure
#' as the complete dataset we used (see example below).
#'
#' @note See \code{\link{ElesAll}} for the processed dataset containing
#' information on all individuals from the complete dataset.
#'
#' @seealso ElesAll, ElesCaptured
#'
#' @name SurvEles_small
#' @docType data
#' @format A \var{dataframe} with 100 elephant year observations.
#' @references This study.
#' @keywords datasets
#' @examples
#'
#' ## Basic information about the data:
#' head(SurvEles_small)
#' str(SurvEles_small)
#' nrow(SurvEles_small)
#'
#' ## Number of observations in each sex:
#' table(SurvEles_small$Sex)
#'
NULL


#' The dataset of information at the individual level for all elephants
#'
#' This dataset contains the breakdowns for the counts of elephants used in the
#' paper.
#'
#' @seealso SurvEles_small, ElesCaptured
#'
#' @name ElesAll
#' @docType data
#' @format A \var{dataframe} with 5150 elephants.
#' @references This study.
#' @keywords datasets
#' @examples
#'
#' ## Basic information about the data:
#' head(ElesAll)
#' str(ElesAll)
#' sum(ElesAll$N)
#'
#' ## Number of elephants in each sex:
#' tapply(ElesAll$N, ElesAll$Sex, sum)
#'
#' ## Number of elephants in each region:
#' tapply(ElesAll$N, ElesAll$Region, sum)
#'
NULL


#' Datasets of information at the individual level for captured elephants
#'
#' This object contains a list with two datasets containing the breakdowns for
#' the counts of captured elephants used in the paper. One list provides
#' breakdowns per capture age and the other one provides breakdowns per capture
#' decade.
#'
#' @seealso SurvEles_small, ElesAll
#'
#' @name ElesCaptured
#' @docType data
#' @format A \var{list} with two \var{data.frame} describing the 2072 captured elephants.
#' @references This study.
#' @keywords datasets
#' @examples
#'
#' ## Basic information about the data:
#' lapply(ElesCaptured, head)
#' lapply(ElesCaptured, function(i) sum(i$N))
#'
#' ## Number of elephants by age at capture:
#' ElesCaptured$ElesCapturedByAge
#'
#' ## Number of elephants by decade at capture:
#' ElesCaptured$ElesCapturedByDecade
#'
NULL


#' The list of the fitted models
#'
#' This dataset contains the 17 models used in the paper. These models have been
#' fitted on the complete dataset!
#'
#' @name Models
#' @docType data
#' @format A \var{list} with the 17 outputs of the function \code{fit_all_models}.
#' @references This study.
#' @keywords datasets
#' @examples
#' ## The names of the models:
#' names(Models)
#'
#' ## The AIC of model 1:
#' Models[["mod1"]]$AIC
#'
#' ## The convergence status of model 1:
#' Models[["mod1"]]$message
#'
#' ## Studying the effect of potential convergence issues:
#' table(unlist(lapply(Models, function(mod) mod$convergence))) ## 1 = perfect convergence
#' ### Conclusion: there is no sign of convergence issue.
#' ### All 17 models fitted without any trouble.
#'
#' ## The estimates from model1:
#' data.frame(estimates = Models[["mod1"]]$param_matrix[, "value"])
#'
NULL


#' The list of the models refitted on bootstrapped data
#'
#' This dataset contains the 40 bootstraps replicate for each of the 17 models.
#' These models have been fitted on the complete dataset!
#'
#' @name Models_boot
#' @docType data
#' @format A \var{list} with the 680 outputs of the function \code{CI_all_models}.
#' @references This study.
#' @keywords datasets
#' @examples
#' ## The names of the models:
#' names(Models_boot)
#'
#' ## The AIC of model 1 replicate 1:
#' Models_boot[["mod1_1"]]$AIC
#'
#' ## The convergence status of model 1 replicate 1:
#' Models_boot[["mod1_1"]]$message
#'
#' ## The estimates from model 1 replicate 1:
#' data.frame(estimates = Models_boot[["mod1_1"]]$param_matrix[, "value"])
#'
#' ## Studying the effect of potential convergence issues:
#' table(unlist(lapply(Models_boot, function(mod) mod$convergence))) ## 1 = perfect convergence
#' models_with_pb <- which(unlist(lapply(Models_boot, function(mod) mod$convergence))!=1)
#' CI_with_pb <- table_model_averaging(models_list = Models,
#'                                     models_list_boot = Models_boot)
#' CI_without_pb <- table_model_averaging(models_list = Models,
#'  models_list_boot = Models_boot[!(names(Models_boot) %in% names(models_with_pb))])
#' max(abs(100*((CI_with_pb$SE - CI_without_pb$SE) /  CI_without_pb$SE)), na.rm = TRUE)
#' ### Conclusion: only 3 fits out of 680 reached maximal time allowed before
#' ### reaching full convergence. Those 3 model fits impact the SE of parameter
#' ### estimates by less than 1.8% in the worst situation, which is negligeable.
#' ### We thus chose not to exclude them and base all CI computation on all 680
#' ### fits.
#'
#'
NULL
