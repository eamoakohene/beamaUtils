\name{install_packages}
\alias{install_packages}
\title{Install essential packages if not already installed and optionally load them}
\usage{
install_packages(load_packages = FALSE)
}
\arguments{
 \item{load_packages}{If true, packages are loaded after install}
}
\description{
 Install essential packages if not already installed and optionally load them
}
\examples{
install_packages()  #install if not present
install_packages(T) #install if not present and load
}
