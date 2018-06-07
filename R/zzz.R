.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n Welcome to SileR,",
    "\n ",
    "\n This package has not been conceived for general use.",
    "\n It has only been created to document the analysis of the paper:",
    "\n ",
    "\n Lahdenper\u00E4, Mar, Courtiol and Lummaa (2018)",
    "\n Differences in age-specific mortality between wild-caught and captive-born Asian elephants.",
    "\n ",
    "\n Type ?SileR for information about how to use this package!",
    "\n"
  )
}

#' Provide some basic information about the code of the package
#'
#' This is a function for development purpose only. This function provides the
#' number of functions and lines of code of the package. The results depend on
#' whether the function is called from the compiled or the raw package. When
#' called from the compiled package, the number of functions only gives the
#' number of exported functions, and the number of lines of code no longer
#' includes documentation and examples. This function is for development purpose
#' only and is thus not exported.
#'
info_package <- function() {
  print(paste("number of functions =", length(ls("package:SileR"))))
  if (requireNamespace("R.utils", quietly = TRUE)) {
    files <- dir(paste0(system.file(package = "SileR"), "/R/"))
    filenames_R <- paste0(system.file(package = "SileR"), "/R/", files)
    lines_code <- sum(sapply(filenames_R, function(file) R.utils::countLines(file)))
    print(paste("number of lines of code =", lines_code))
  } else {message("Install the package R.utils for more info.")}
  return(invisible(NULL))
}

#' Build and upload the package on drat
#'
#' This is a function for development purpose only. Drat is a package that
#' allows for the installation of R packages from GitHub. We use this system to
#' distribute the package (see Readme.Rmd). The function defined here builds the
#' package, commits the changes and push them on the drat repository on GitHub.
#' It is designed to work on the maintainer computer only. This function is for
#' development purpose only and is thus not exported.
#'
#' @inheritParams parameters
#'
upload_on_drat <- function(binary = FALSE){
  if (requireNamespace("devtools") & requireNamespace("drat")) {
    print("Building the package...")
    path <- devtools::build(binary = TRUE)
    print("Moving the package to local drat folder and commiting changes in git...")
    drat::insertPackage(path, "~/Boulot/Mes_projets_de_recherche/R_packages/drat", commit = TRUE)
    print("Pushing the package on GitHub...")
    system("cd ~/Boulot/Mes_projets_de_recherche/R_packages/drat; git push")
    print("done")
  } else {
    stop("devtools and drat must both be installed for this function to work.")
  }
}
