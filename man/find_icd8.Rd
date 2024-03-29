% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icdR.R
\name{find_icd8}
\alias{find_icd8}
\title{Find relevant icd-8 codes}
\usage{
find_icd8(icd_8 = icd_8, icd_8_code = icd_8_code, regex, negate = FALSE)
}
\arguments{
\item{icd_8}{is the data with icd-8 codes}

\item{icd_8_code}{is the column (variable) with icd-8 codes}

\item{regex}{is the regex of icd-8 codes}
}
\value{
the dataframe with icd-8 codes
}
\description{
Finds specific icd-8 codes provided as regex
}
\examples{
find_icd8(regex = "^249")
}
