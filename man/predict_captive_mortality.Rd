% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictions.R
\name{predict_captive_mortality}
\alias{predict_captive_mortality}
\title{Predict the mortality of captive born individuals}
\usage{
predict_captive_mortality(models_list)
}
\arguments{
\item{models_list}{The list of fitted models}
}
\value{
A list containing the predicted mortality of captive born males and females.
}
\description{
This function predicts the mortality of captive born individuals.
}
\examples{
predict_captive_mortality(models_list = Models)

}
\seealso{
\code{\link{predict}}
}
