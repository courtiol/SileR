#' Survival Analysis of elephants using an extension of the William Siler's
#' Framework
#'
#' This R package aims at documenting the workflow behind the results from the
#' paper entitled "Differences in age-specific mortality between wild-caught and
#' captive-born Asian elephants" by Mirkka Lahdenperä, Khyne U Mar, Alexandre
#' Courtiol and Virpi Lummaa (2018) Nature Communicaitons. The main function of
#' the package is to fit survival models derived from a model introduced by
#' Siler (1979). The main differences are that the effect of covariate(s) are
#' being investigated and that the survival models have to be fitted on right
#' and left censored longitudinal data.
#'
#' This package has not been conceived for general use!
#'
#' All main functions of this package contain a small documentation and examples
#' which could be useful for those who try to understand our code. Type
#' \code{ls("package:SileR")} for a list of all exported functions.
#'
#' We recommend you to directly explore the files contained in the package after
#' uncompressing the content of the *.tar.gz file (link available on the GitHub page
#' \url{https://github.com/courtiol/SileR}). You can use
#' the R function \code{\link{untar}} to extract the content of the tarball.
#'
#' The package contains the original fitted models, that is those fitted on the
#' complete dataset. For those who would like to try our fitting function, we have
#' also provided a subset of the original data which we are not allowed to share.
#'
#' In the examples below, we provide the workflow leading the results presented
#' in the paper. Many outputs will however look very different because they would
#' require to be derived from the complete dataset.
#'
#' @name SileR-package
#' @aliases SileR-package SileR
#' @docType package
#'
#' @references
#' Mirkka Lahdenperä, Khyne U Mar, Alexandre Courtiol and Virpi Lummaa (2018).
#' Nature Communications. Differences in age-specific mortality between
#' wild-caught and captive-born Asian elephants.
#'
#' William Siler (1979). A competiting-risk model for animal mortality. Ecology 60(4), 750-757.
#'
#' @keywords package
#' @examples
#'
#'
#' ################################################
#' ##  Setting general options for this workflow ##
#' ################################################
#'
#' ### Note: set the following options to TRUE or FALSE depending on what you want.
#'
#' save_displays_on_disk <- FALSE # export tables & figures on drive?
#' save_fits_on_disk <- FALSE     # export fitted models on drive?
#' refit_models <- TRUE           # refit the 17 main models?
#' compute_CI <- FALSE            # compute CI using bootstrap?
#'
#'
#' #######################################
#' ##  Basic information about the data ##
#' #######################################
#'
#' ### Note: the following outputs would be different on the complete dataset!
#'
#' compute_demography(data = SurvEles_small)
#'
#'
#' ################################################################
#' ##  Fitting the survival models using Siler's extended method ##
#' ################################################################
#'
#' ### Note: the following outputs would be different on the complete dataset!
#'
#' if (refit_models | compute_CI){
#'
#' ## Defining maximal resources allowed for computations:
#'
#' ### 1. We define the maximal time in seconds allowed for each fit
#' maxtime <- 10 # here we allow for 10 sec per model fit
#'               # to obtain results rather quickly
#'               # we used 24 hours for the paper!
#'
#' ### 2. We define the maximal number of cores/CPU allowed for computation
#' n_CPU <- 2L # here specify the max number of CPU to use
#' }
#'
#' if (refit_models) {
#'
#' ## Fitting and saving all 17 models described in the paper:
#'
#' ### Note: the output would be different on the complete dataset!
#' Models_small <- fit_all_models(data = SurvEles_small,
#'                                maxtime = maxtime,
#'                                n_CPU = n_CPU,
#'                                save_rda = save_fits_on_disk)
#' }
#'
#'
#' if (compute_CI) {
#'
#' ## Computing the confidence intervals on model parameter estimates:
#'
#' n_replicates <- 2 ## we used 40 replicates per model for the paper!
#'
#' Models_boot_small <- boot_all_models(models_list = Models,
#'                                      data = SurvEles_small,
#'                                      n_boot = n_replicates,
#'                                      maxtime = maxtime,
#'                                      n_CPU = n_CPU,
#'                                      save_rda = save_fits_on_disk)
#' }
#'
#'
#' ###################################################
#' ##  Basic information about survival predictions ##
#' ###################################################
#'
#' ### Note: the following outputs are computed on the models fitted on the complete
#' ### dataset. So they should be the same as in the paper!
#' ### If you have computed Models_small above, use that
#' ### instead of Models to see the results based on your fits.
#'
#' compute_survival_info(models_list = Models)
#'
#'
#' #########################################
#' ##  Basic information about top models ##
#' #########################################
#'
#' ### Note: the following outputs are computed on the models fitted on the complete
#' ### dataset. So they should be the same as in the paper!
#' ### If you have computed Models_small above, use that
#' ### instead of Models to see the results based on your fits.
#'
#' round(evidence_ratios_top_models(models_list = Models), 2)
#'
#'
#' #############################################
#' ##  Basic information about the best model ##
#' #############################################
#'
#' ### Note: the following outputs are computed on the models fitted on the complete
#' ### dataset. So they should be the same as in the paper!
#' ### If you have computed Models_small above, use that
#' ### instead of Models to see the results based on your fits.
#'
#' best_model_description(models_list = Models)
#'
#'
#' #######################################
#' ##  Creating the tables of the paper ##
#' #######################################
#'
#' ### Note: the following outputs are computed on the models fitted on the complete
#' ### dataset. So they should be the same as in the paper!
#' ### If you have computed Models_small and Models_boot_small above, use those
#' ### instead of Models and Models_boot to see the results based on your fits.
#'
#' ## Creating the table comparing the models: (table 1)
#' table_all_models(Models, save_xlsx = save_displays_on_disk)
#'
#' ## Creating the table with model averaged estimates and CI (supplementary table 1):
#' table_model_averaging(models_list = Models,
#'                       models_list_boot = Models_boot,
#'                       save_xlsx = save_displays_on_disk)
#'
#' ## Creating the table with parameter estimates for all models (supplementary table 2):
#' table_estimates(models_list = Models,
#'                 models_list_boot = Models_boot,
#'                 save_xlsx = save_displays_on_disk)
#'
#' ## Creating the table with parameter SE for all models (supplementary table 3):
#' table_SE(models_list = Models,
#'          models_list_boot = Models_boot,
#'          save_xlsx = save_displays_on_disk)
#'
#' ## Creating the table with the median lifespan (supplementary table 4):
#' table_lifespan(models_list = Models,
#'                save_xlsx = save_displays_on_disk)
#'
#'
#' ########################################
#' ##  Creating the figures of the paper ##
#' ########################################
#'
#' ### Note: the following outputs would be different on the complete dataset,
#' ### except for figure 3 that uses counts derived from the compete dataset.
#' ### In particular, the figure 4 cannot be properly drawn as it requires
#' ### yearly observations of elephants at each age, which is not the case of in
#' ### the subset of the data.
#' ###
#' ### Also, if you have computed Models_small and Models_boot_small above,
#' ### use those instead of Models and Models_boot to see the results based on
#' ### your fits.
#'
#' ## Creating the figure 1:
#' plot_baseline(data = SurvEles_small, models_list = Models, save_pdf = save_displays_on_disk)
#'
#' ## Creating the figure 2:
#' plot_capture(data = SurvEles_small, models_list = Models, save_pdf = save_displays_on_disk)
#'
#' ## Creating the figure 3:
#' plot_captured_demog(data = ElesCaptured, save_pdf = save_displays_on_disk)
#'
#' ## Creating the figure 4:
#' plot_survivorship(data = SurvEles_small, models_list = Models, save_pdf = save_displays_on_disk)
#'
#'
NULL
