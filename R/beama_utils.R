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

  require(ggplot2)
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
  gg_data <- logs[1:top,]

  p <- ggplot(gg_data,ggplot2::aes(x=package, y=downloads, fill=package))
  p <- p + geom_bar( stat='identity' )
  p <- p + coord_flip()
  p <- p + theme(legend.position = "none")
  p <- p + labs(y='Downloads')
  print(p)
  return(gg_data)
}

split_str <- function( s="CHAY,CHAW,D7BT"){
  abc <- base::gsub(",","','",s)
  abc <- base::paste0("('",abc,"')")
  return(abc)
}

to_clipboard <- function( x, row.names=FALSE, col.names=TRUE, ...) {
    write.table( x,"clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
}

ts_to_df <- function( my_ts){

  my_df <- data.frame( date=zoo::as.Date(zoo::as.yearmon(time(my_ts))), value=as.matrix(my_ts ))
  my_df$yr <- lubridate::year(my_df$date)
  my_df$mth <- lubridate::month(my_df$date)

  return(my_df)
}
