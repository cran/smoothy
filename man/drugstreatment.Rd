% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drugstreatment.R
\docType{data}
\name{drugstreatment}
\alias{drugstreatment}
\title{Drug Administration Data}
\format{
A data frame with the following columns:
\describe{
\item{id}{Unique identifier for each drug administration event.}
\item{start_date}{The start date of drug administration.}
\item{end_date}{The end date of drug administration.}
\item{drug}{The name of the drug administered.}
}
}
\usage{
drugstreatment
}
\description{
This dataset contains information about drug administration. Each row represents a unique drug administration event.
}
\examples{
data("drugstreatment")
head(drugstreatment)

}
\keyword{data}
