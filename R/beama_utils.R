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
  obj_attr <- attributes( obj )
  n <- length( obj_attr$names )
  return( obj[ obj_attr$names[1:n]] )
}

#' Load R library. Install if not already installed
use_package <- function(p) {

  if (!is.element(p, installed.packages()[,1])){
    install.packages(p, dep = TRUE)
  }

  require(p, character.only = TRUE)

}

plot_latest_packages <- function(

     since="2016-01-01",
     cran_url = "http://dirk.eddelbuettel.com/cranberries/",
     top = 25
){

  page <- xml2::read_html(cran_url)
  titles <-  rvest::html_text(rvest::html_nodes(page,"b"))
  new_packages <- unique(
    gsub(
      "^New package (.*) with initial .*", "\\1",
      titles[grepl("^New package", titles)]
    )
  )

  logs <- pbapply::pblapply(
    new_packages,
    function(x) {

      down <- cranlogs::cran_downloads(x, from = since)$count
      if(sum(down) > 0) {
        public <- down[which(down > 0)[1]:length(down)]
      } else {
        public <- 0
      }

      return(
        data.frame(
          package = x,
          downloads = sum(down),
          avg = mean(public)
        )
      )
    }

  )

  logs <- do.call( rbind, logs)
  logs <- dplyr::arrange(logs,desc(downloads))
  logs$package <- factor(logs$package,levels = logs$package)

  p <- ggplot2::ggplot(logs[1:top],ggplot2::aes(x=package, y=downloads, fill=package))
  p <- p + ggplot2::geom_bar( stat='identity' )
  p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::labs(y='')
  print(p)
  return(logs)
}
