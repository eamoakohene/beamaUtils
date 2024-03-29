#' Set the number of decimal places
#' x = numeric
#' k = integer

set_decimal <- function(x, k, cut = 200 ){

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
    devtools::install_github(sprintf('eamoakohene/%s',p), upgrade = "never", force = "true")
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
  use_package("pbapply", load_packages = load_packages)
  use_package("cranlogs", load_packages = load_packages)

  use_package("NMF", load_packages = load_packages)
  use_package("doParallel", load_packages = load_packages)
  use_package("foreach", load_packages = load_packages)
  use_package("googleVis", load_packages = load_packages)
  use_package("gridBase", load_packages = load_packages)
  use_package("irlba", load_packages = load_packages)
  use_package("iterators", load_packages = load_packages)
  use_package("pkgmaker", load_packages = load_packages)
  use_package("registry", load_packages = load_packages)
  use_package("rngtools", load_packages = load_packages)
  use_package("seasonal", load_packages = load_packages)
  use_package("RSQLite", load_packages = load_packages)
  use_package("zeallot", load_packages = load_packages)
  use_package("rlang", load_packages = load_packages)
  use_package("selectr", load_packages = load_packages)

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
  use_package("bsplus", load_packages = load_packages)

  use_package("ggpol", load_packages = load_packages)
  use_package("fredr", load_packages = load_packages)
  use_package("ggrepel", load_packages = load_packages)
  use_package("geosphere", load_packages = load_packages)
  use_package("extrafont", load_packages = load_packages)
  use_package("curl", load_packages = load_packages)
  use_package("ggforce", load_packages = load_packages)
  use_package("sjmisc", load_packages = load_packages)
  use_package("RJDemetra", load_packages = load_packages)
  use_package("showtext", load_packages = load_packages)
  use_package("gganimate", load_packages = load_packages)
  use_package("gifski", load_packages = load_packages)
  use_package("packcircles", load_packages = load_packages)
  use_package("likert", load_packages = load_packages)
  use_package("RODBCext", load_packages = load_packages)
  use_package("sparkline", load_packages = load_packages)
  use_package("rsconnect", load_packages = load_packages)

  #use_package("", load_packages = load_packages)

  ## EA github repositories
  use_ea_github("beamaColours", load_packages = load_packages)
  use_ea_github("storedQry", load_packages = load_packages)
  use_ea_github("beamaSankey", load_packages = load_packages)
  use_ea_github("beamafx", load_packages = load_packages)
  use_ea_github("onsR2", load_packages = load_packages)


  #use_ea_github("beamaTrends", load_packages = load_packages)





}


plot_latest_packages <- function(

     since="2019-01-01",
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

split_int <- function( s="CHAY,CHAW,D7BT"){
  #abc <- base::gsub(",","','",s)
  abc <- base::paste0("",s,")")
  return(abc)
}

to_clipboard <- function( x, row.names=FALSE, col.names=TRUE, ...) {
    write.table( x,"clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
}

#' Timeseries to Dataframe
#' Convert timeseries object \code{my_ts} to a data frame with columns \code{yr, mth, value, date}
#'
ts_to_df <- function( my_ts, na.rm = FALSE){

  my_freq <- frequency( my_ts )
  my_date <- my_yr <- my_mon<- my_df <- NULL

  if( my_freq %in% c(1,12) ){

    my_df <- data.frame( date=zoo::as.Date(zoo::as.yearmon(time(my_ts))), value=as.matrix(my_ts ))
    my_df$mth <- lubridate::month(my_df$date)
    my_df$yr <- lubridate::year( my_df$date)

  }else if( my_freq == 4){

    my_df <- data.frame( date=zoo::as.Date(zoo::as.yearqtr(time(my_ts))), value=as.matrix(my_ts ))
    my_df$mth <- lubridate::month(my_df$date) + 2
    my_df$yr <- lubridate::year(my_df$date)
    my_df$date <- as.Date( paste(my_df$yr, my_df$mth, 1, sep='-'))
  }




  if( na.rm ){

    return(
      dplyr::filter( my_df , is.na( value ) == FALSE)
    )

  }else{

   return(my_df)

  }
}

ts_dates <- function(x){

  if(! is.ts( x )){ return( NULL )}

  if(frequency(x)==12){
    return(
      seq(
        as.Date(paste(c(start(x),28), collapse = "/")),
        by = "month",
        length.out = length(x)
      )
    )
  }else if(frequency(x)==4){
    return(
      seq.Date(
        as.Date(paste(start(x)[1],start(x)[2]*3,28,sep="/")),
        length.out = length(x),
        by="3 months"
      )
    )
  }else if(frequency(x)==1){
    return(
      seq(
        as.Date(paste(c(start(x),1), collapse = "/")),
        by = "year",
        length.out = length(x)
      )
    )

  }else{
    stop("Frequency of time series UNKNOWN")
  }
}

df_to_ts <- function(my_df, frq = 12, my_start = NULL){

  if(!is.data.frame(my_df)){
    cat("Please supply dataframe\n")
    return( NULL)
  }

  if(frq %in% c(4,12)){
    cat("Please ensure that frequency is 4 or 12\n")
    return(NULL)
  }

  ts_start <- my_start

  if(is.null(my_start)){

    ts_start <- c(df$yr[1], df$mth[1])

  }

  if(is.null(my_start)){
    cat("Please supply start\n")
    return(NULL)
  }

  ts_freq <- frq
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

run_sql_db <- function(sql, db, drv = "R:"){


  db_name <- dbp(db,drv)

  if(nchar(db_name) > 5){

    return(
      run_sql(sql, db_name)
    )

  }

  return (NULL)
}

get_fxn <- function(name, db='R:/shiny/beama/bmonitor/bss.sqlite', view = FALSE){

  my_name <- tolower(name)
  my_sql <- sprintf("select fxn from stored_fxns where lower(name) ='%s'", my_name)
  my_data <- run_sql(qry =  my_sql, db = db )
  my_fxn <- NULL

  if( nrow(my_data) > 0){

    my_fxn <- my_data$fxn
    if(!view){

      return( eval(parse(text = my_fxn)) )

    }else{

      cat( my_fxn,"\n")

    }
  }
  return(NULL)

}

scrap_fxn <- function(){
  return(
    get_fxn("scrap_web_data")
  )
}

save_plot <- function(

   file="glance.png",
   width=633,
   height=219,
   path="W:/reports/latex/images/",
   ppi=72

){
  ggplot2::ggsave(
    file = paste0(path,file),
    height = height / ppi,
    width = width / ppi,
    dpi = ppi,
    units = "in"
)
}

df_to_list <- function(df,col_name, col_value){

  e <- base::new.env(hash = TRUE)
  for (i in 1:nrow(df) ) { base::assign( df[i, col_name], df[i, col_value], e) }

  return( as.list(e))
}

db_paths <- function(){
  return(
    data.frame(
      db=c(
        'bistats',
        'beamafx',
        'sankeys',
        'ibeama_indices',
        'ibtrends',
        'bnetworks',
        'datastore',
        'fame',
        'onsR2',
        'orgalime',
        'bts',
        'storedfxns',
        'bmm',
        'bts_emails',
        'cepe',
        'prodcom',
        'badd',
        'centre_point',
        'uktrade',
        'uktrade_info',
        'covid19',
        'surveys',
        'bindx',
        'bmonitor',
        'badd'
      ),
      path=c(
        '/packages/bistats/inst/extdata/bistats.sqlite',
        '/packages/beamafx/inst/extdata/beamafx.sqlite',
        '/packages/beamaSankey/inst/extdata/sankeys.sqlite',
        '/packages/bindx/R/beama_indices.sqlite',
        '/packages/bindx/R/btrends.sqlite',
        '/packages/bnetworks/inst/networks/bnetworks1/bnetworks.sqlite',
        '/packages/datastore/R/datastore.sqlite',
        '/packages/fame27/inst/extdata/fame.sqlite',
        '/packages/onsR2/inst/extdata/onsR2.sqlite',
        '/packages/orgalime/inst/extdata/orgalime.sqlite',
        '/packages/surveyapp/bts.sqlite',
        '/packages/storedFxns/storedfxns.sqlite',
        '/shiny/beama/bmonitor/bss.sqlite',
        '/data/lite/bts_emails.sqlite',
        '/shiny/cepe/cepe.sqlite',
        '/data/lite/prodcom.sqlite',
        '/data/badd.db',
        '/shiny/centrePoint/centre_point.sqlite',
        '/data/lite/uktrade.sqlite',
        '/data/lite/uktrade_info.sqlite',
        '/data/lite/covid19',
        '/packages/surveys/surveys.sqlite',
        '/packages/bindx/R/beama_indices.sqlite',
        '/shiny/beama/bmonitor/bss.sqlite',
        '/data/badd.db'
      ),
      stringsAsFactors = FALSE
    )
  )
}

dbp <- function(db, drv = 'R:' ){
  ldb <- db_paths()
  df <- sqldf::sqldf(
    sprintf("select path from ldb where db ='%s';", db )
  )
  if(nrow(df)>0){
    return(
      paste0(drv,df$path[1])
    )
  }

  NULL
}

view_tbls <- function(db, drv = 'R:'){
  # db='bindx'
  sql <- "SELECT name FROM sqlite_master WHERE type ='table' AND name NOT LIKE 'sqlite_%';"



 return(
    run_sql(
      sql,
      db=dbp(db, drv=drv)
    )
 )

}

df_trends <- get_data <- function(codes, db='bistats', fmt = 'wide', y1 = 2010, tbl = 'trends_data', drv = 'R:', dp = 1){
  # db ='bindx'
  ldb <- dbp(db, drv)

  if(!is.null(ldb)){


     mcode <- split_str( tolower(codes))

     sql <- sprintf(
       "select yr,mth,dy, data_code,data_value from %s where yr>=%s and lower(data_code) in %s order by data_code, yr, mth, dy", tbl,y1,mcode
     )

     df <- run_sql( sql, db=ldb )

     if(nrow(df)>0){

       if(tolower(fmt) =='wide'){

         return(

            tidyr::spread( df, data_code, data_value)
         )

       }else if(tolower(fmt) =='ts'){
         ts_start <- c(df$yr[1], df$mth[1])
         ts_freq <- run_sql(

           sprintf("select data_frq from trends_meta where lower(data_code) in %s limit 1", mcode),
           db = ldb

         )$data_frq

         return( stats::ts( round(df$data_value,1), start = ts_start, frequency = ts_freq))

       }else{

         return( df)

       }
     }

     return (NULL)

  }else{

    return (NULL)

  }
}

search_code <- function(x, db = 'bistats', drv = 'R:'){
  ldb <- dbp( db, drv)
  sql <- sprintf("select data_code, data_frq from trends_meta where data_code like '%%%s%%'", x)
  return(
    run_sql(sql, db = ldb)
  )
}

bif <- function(..., env=parent.frame()) {

  fx <- function(dots) {
    if (length(dots) == 1) {

      return(dots[[1]])

    } else {

      tmp <- dots[[1]]

      return(call("ifelse", tmp[[3]], tmp[[2]], fx(dots[-1])))
    }
  }

  isa <- function(x) {
    if (length(x) > 1) {

      return (identical(x[[1]], quote(`<-`)))

    }else{

      return(FALSE)
    }
  }

  isv <- function(dots) {

    check <- sapply(dots, isa)

    if (all(head(check, -1)) && !tail(check, 1)) {

      return("HDF")

    } else if (all(check)) {

      return("NDF")

    } else {

      stop("invalid bif arguments", call. = FALSE)

    }
  }

  dots <- eval(substitute(alist(...)))
  status <- isv(dots)

  if (status == "NDF") {
    dots <- c(dots, expression(NA))
  }
  eval(fx(dots), envir = env)
}

test_url<- web_url <- function(url = "https://www.beama.org/?pg=404"){

  require(magrittr)
  require(curl)

  curl_fetch_memory(url = url ,  handle = new_handle())%$%
    content %>%
    rawToChar

}

web_tbls <- function(url="https://www.bbc.co.uk/sport/olympics/57836709"){

  x <- rvest::read_html(url)
  tbls <- x|> rvest::html_table(fill=T)
  if( length(tbls) == 0){
    return(NULL)
  }
  return(tbls)

}

#### DAYS IN MONTH
dmth <- function(mth,yr) {

  dt <- as.Date(sprintf("%s-%02d-01", yr, mth))
  days <- 28:31

  return(
    rev(
      days[
        which(
          !is.na(
              as.Date(paste0( substr(dt, 1, 8), days), '%Y-%m-%d')
          )
       )
     ]
    )[1]
  )
}
