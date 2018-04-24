.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n Welcome to SileR,",
    "\n this package has not been conceived for general use. It has only been created to replicate the results of the paper:",
    "\n ",
    "\n Mar, Lahdenper\u00E4, Courtiol and Lummaa (2018) Differences in age-specific mortality between wild-caught and captive-born Asian elephants.",
    "\n ",
    "\n We invite you to explore the content of this package and replicate the results of the paper.",
    "\n ",
    "\n Type ?SileR for more details",
    "\n"
  )
}

#' Provide some basic information about the code of the package
#'
#' This function provides the number of functions and lines of code of the
#' package. The results depend on whether the function is called from within the
#' compiled or the raw package. When called from the compiled package, the
#' number of functions only gives the number of exported functions, and the
#' number of lines of code no longer includes documentation and examples. This
#' function is for development purpose only and is thus not exported.
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
