\name{view_object}
\alias{view_object}
\title{View an R object}
\usage{
view_object(p)
}
\arguments{
 \item{p}{R object}
}
\value{
 The details of object \code{p}
}
\description{
Prints 'view_object, world!'.
}
\examples{
df <- data.frame( a=rnorm(10), y=rnorm(10) )
view_object(df)
}
