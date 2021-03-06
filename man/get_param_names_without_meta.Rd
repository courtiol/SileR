% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{get_param_names_without_meta}
\alias{get_param_names_without_meta}
\title{This function trims out the meta suffix from the names of parameters}
\usage{
get_param_names_without_meta(param_names)
}
\arguments{
\item{param_names}{A vector of parameter names}
}
\value{
A vector of names.
}
\description{
This function is called internally by other functions. It is a simple
utility function.
}
\examples{
get_param_names_without_meta(param_names = rownames(Models$mod1$param_matrix_full)[1:2])

}
\seealso{
\code{\link{get_param_name}}
}
