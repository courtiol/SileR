% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design_matrix.R
\name{build_empty_design_matrix}
\alias{build_empty_design_matrix}
\title{Create an empty design matrix}
\usage{
build_empty_design_matrix(data)
}
\arguments{
\item{data}{The dataset to be used}
}
\value{
The empty design matrix.
}
\description{
This function is called internally by other functions.
It builds an empty design matrix.
}
\examples{
build_empty_design_matrix(SurvEles_small)

}
\seealso{
\code{\link{build_design_matrix}}
}
