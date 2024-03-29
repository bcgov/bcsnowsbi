% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getextra_snowdata.R
\name{getextra_snowdata}
\alias{getextra_snowdata}
\title{Function for getting extra snow data (such as SWE for previous years and snow depth) to append to statistics table for ASWE data}
\usage{
getextra_snowdata(ASWE_sites_i, get_year, survey_period)
}
\arguments{
\item{ASWE_sites_i}{ASWE sites that you want to get data and use to calculate SBI data for}

\item{get_year}{Year}

\item{survey_period}{Survey period}
}
\description{
Meant to be an internal function called within an lapply() loop by the aswe_sbidata() function. The lapply() approach is used in case data retrieve functions miss data when asked to retrieve a large amount of data
}
\examples{
\dontrun{}
}
\keyword{internal}
