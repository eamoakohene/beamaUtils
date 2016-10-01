\name{ts_to_df}
\alias{ts_to_df}
\title{ Convert time series object to a dataframe}
\usage{
ts_to_df( x )
}
\arguments{
 \item{x}{ time series object }
 }
\value{
 A data frame with 4 columns: \code{date}, \code{value}, \code{yr} and \code{mth}
}
\description{
 Convert time series object to a dataframe
}
\examples{

ts_to_df(UKDriverDeaths)
}
