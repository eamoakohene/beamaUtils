#' Set the number of decimal places
#' x = numeric
#' k = integer
set_decimal <- function(x, k){
  if(x[1]<200){
    format(round(x, k), nsmall=k,big.mark=",")
  }else {
    format(round(x, 0), nsmall=0,big.mark=",")
  }
}

#' General object viewer
view_object <- function( obj ){
  obj_attr <- attributes(obj);
  n <- length( obj_attr$names )
  return( obj[obj_attr$names[1:n]] )
}

#' Load R library. Install if not already installed
use_package <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
