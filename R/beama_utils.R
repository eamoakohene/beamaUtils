#' Set the number of decimal places
#' x = numeric
#' k = integer

set_decimal <- function(x, k, cut = 200){

  if( !is.null(x) && length(x)> 0 ){


    if( x[ 1 ]< cut ){
      format(round(x, k), nsmall=k,big.mark=",")
    }else {
      format(round(x, 0), nsmall=0,big.mark=",")
    }

  }

}

#' General object viewer
view_object <- function( obj ){
  obj_attr <- attributes( obj )
  n <- length( obj_attr$names )
  return( obj[ obj_attr$names[1:n]] )
}

#' Load R library. Install if not already installed
use_package <- function(p, load_packages = TRUE) {

  if (!is.element(p, installed.packages()[,1])){
    install.packages(p, dep = TRUE)
  }

  if(load_packages) { require(p, character.only = TRUE) }

}

#devtools::install_github('eamoakohene/beamaUtils')
use_ea_github<- function(p , load_packages = TRUE) {

  if (!is.element(p, installed.packages()[,1])){
    devtools::install_github(sprintf('eamoakohene/%s',p))
  }

  if( load_packages) { require(p, character.only = TRUE) }

}

install_packages <- function(load_packages = FALSE){

  if (!is.element("devtools", installed.packages()[, 1])) {

    install.packages("devtools", dep = TRUE)
    require("devtools")

  }


  use_package("installr", load_packages = load_packages)
  use_package("digest", load_packages = load_packages)
  use_package("quantmod", load_packages = load_packages)
  use_package("gridExtra", load_packages = load_packages)
  use_package("reshape2", load_packages = load_packages)
  use_package('tidyr', load_packages = load_packages)
  use_package("ggplot2", load_packages = load_packages)
  use_package("MASS", load_packages = load_packages)
  use_package("tseries", load_packages = load_packages)
  use_package("x12", load_packages = load_packages)
  use_package("R.oo", load_packages = load_packages)
  use_package("forecast", load_packages = load_packages)
  use_package("plyr", load_packages = load_packages)
  use_package("scales", load_packages = load_packages)
  use_package("RODBC", load_packages = load_packages)
  use_package("xtable", load_packages = load_packages)
  use_package("gdata", load_packages = load_packages)
  use_package("RJSONIO", load_packages = load_packages)
  use_package("WDI", load_packages = load_packages)
  use_package("lubridate", load_packages = load_packages)
  use_package("grid", load_packages = load_packages)
  use_package("Quandl", load_packages = load_packages)
  use_package("dplyr", load_packages = load_packages)
  use_package("plotly", load_packages = load_packages)
  use_package("sqldf", load_packages = load_packages)
  use_package("PerformanceAnalytics", load_packages = load_packages)
  use_package("ggthemes", load_packages = load_packages)
  use_package("RCurl", load_packages = load_packages)
  use_package("XML", load_packages = load_packages)
  use_package("codetools", load_packages = load_packages)

  use_package("lattice", load_packages = load_packages)
  use_package("Rcpp", load_packages = load_packages)
  use_package("inline", load_packages = load_packages)
  use_package("animation", load_packages = load_packages)
  #use_package("rstan")

  use_package("microbenchmark", load_packages = load_packages)
  use_package("XLConnect", load_packages = load_packages)
  use_package("mosaic", load_packages = load_packages)
  use_package("qdap", load_packages = load_packages)
  use_package("pipeR", load_packages = load_packages)
  use_package("colorspace", load_packages = load_packages)
  use_package("MAPA", load_packages = load_packages)

  use_package('networkD3', load_packages = load_packages)
  use_package('dygraphs', load_packages = load_packages)
  #use_package("bigrquery")

  use_package("rlist", load_packages = load_packages)
  use_package("stsm", load_packages = load_packages)
  use_package("knitr", load_packages = load_packages)
  use_package("DT", load_packages = load_packages)

  use_package("shiny", load_packages = load_packages)
  use_package("shinyjs", load_packages = load_packages)
  use_package("shinyBS", load_packages = load_packages)
  use_package("shinydashboard", load_packages = load_packages)

  ## EA github repositories
  use_ea_github("beamaSankey", load_packages = load_packages)
  use_ea_github("beamaTrends", load_packages = load_packages)
  use_ea_github("beamafx", load_packages = load_packages)
  use_ea_github("storedQry", load_packages = load_packages)
  use_ea_github("onsR2", load_packages = load_packages)
  use_ea_github("beamaColours", load_packages = load_packages)

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

#' Timeseries to Dataframe
#' Convert timeseries object \code{my_ts} to a data frame with columns \code{yr, mth, value, date}
#'
ts_to_df <- function( my_ts, na.rm = FALSE){

  my_df <- data.frame( date=zoo::as.Date(zoo::as.yearmon(time(my_ts))), value=as.matrix(my_ts ))
  my_df$yr <- lubridate::year(my_df$date)
  my_df$mth <- lubridate::month(my_df$date)

  if( na.rm ){

    return(
      dplyr::filter( my_df , is.na( value ) == FALSE)
    )

  }else{

   return(my_df)

  }
}

#' Data days
#' Convert a date to days using assuming \code{d360} days in a year
#' Returns number of days
#'
ddays <- function(yr, mth, dy = 0 , d360 = 31 ){

  return( yr * d360 * 12 + mth * d360 + dy )

}

#' Difference between dates
#'
days_diff <- function(d1="1969-09-28", d2=Sys.Date() ){

    y1 <- lubridate::year(d1)
    m1 <- lubridate::month(d1)
    d1 <- lubridate::day(d1)

    y2 <- lubridate::year(d2)
    m2 <- lubridate::month(d2)
    d2 <- lubridate::day(d2)

    return( ddays(y2,m2,d2) - ddays(y1,m1,d1) )

}

days_to <- function(d="2050-01-01"){
   return(
     days_diff(Sys.Date(), d)
   )
}

days_since <- function(d="2050-01-01"){
  return(
    days_diff( d, Sys.Date())
  )
}

run_sql <- function(qry, db='R:/shiny/beama/bmonitor/bss.sqlite'){
  conn <- DBI::dbConnect( RSQLite::SQLite(), dbname= db )
  results <- RSQLite::dbGetQuery(conn, qry)
  DBI::dbDisconnect(conn)
  return(results)
}

get_fxn <- function(name, db='R:/shiny/beama/bmonitor/bss.sqlite'){

  my_name <- tolower(name)
  my_sql <- sprintf("select fxn from stored_fxns where lower(name) ='%s'", my_name)
  my_data <- run_sql(qry =  my_sql, db = db )
  my_fxn <- NULL

  if( nrow(my_data) > 0){

    my_fxn <- my_data$fxn
    return( eval(parse(text = my_fxn)) )

  }
  return(NULL)

}
