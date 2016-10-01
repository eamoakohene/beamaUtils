\name{to_clipboard}
\alias{to_clipboard}
\title{ Copy dataframe to clipboard}
\usage{
to_clipboard( x, row.names=FALSE, col.names=TRUE)
}
\arguments{
 \item{x}{ dataframe }
 \item{row.names}{ TRUE if row names are to be included in the dataframe }
 \item{col.names}{ TRUE if column names are to be included in the dataframe }
}
\description{
 Copy dataframe to clipboard
}
\examples{
df <- data.frame{x=1:10, y=21:30}
to_clipboard(df)
}
