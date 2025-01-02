# Imports and processes data, outputs list(forecast, preliminary print, final print)
forecast.prep <- function(fcast_series, outturn, start_year="2023", P=FALSE)
{
  # Preparing dates and names
  fcast_dates <- c()
  fcast_dates_names <- c()
  quarters <- c('Q1', 'Q2', 'Q3', 'Q4')
  for (y in as.numeric(start_year):(year(Sys.Date())+1)) {
    for (q in 1:4) {
      fcast_dates[length(fcast_dates)+1] <- paste(quarters[q], substr(as.character(y),3,4), sep="")
      fcast_dates_names[length(fcast_dates_names)+1] <- paste(quarters[q], substr(as.character(y),3,4), sep=":")
    }
  }
  
  fcast_dates <- fcast_dates[c(-(length(fcast_dates)-1),-length(fcast_dates))]
  fcast_dates_names <- fcast_dates_names[c(-(length(fcast_dates_names)-1),-length(fcast_dates_names))]
  
  #fcast_dates_names
  start_date <- as.Date(paste(start_year,"-01-01",sep=""))
  
  # Settings for getting both preliminary and final prints
  if(file_name=='us') {
    ovrd_p <- c("RELEASE_STAGE_OVERRIDE"="A")
    ovrd_f <- c("RELEASE_STAGE_OVERRIDE"="T")
  } else if (file_name=='ez') {
    ovrd_p <- c("RELEASE_STAGE_OVERRIDE"="A")
    ovrd_f <- c("RELEASE_STAGE_OVERRIDE"="F")
  } else {
    ovrd_p <- c("RELEASE_STAGE_OVERRIDE"="P")
    ovrd_f <- c("RELEASE_STAGE_OVERRIDE"="F")
  }
  
  # Create combinations of forecast data names
  comb     <- expand.grid(fcast_series, fcast_dates)
  fcast_codes <- apply(comb, 1, function(x) paste(x[1], " ", x[2], " Index", sep = ""))
  
  # Get forecast data from BBG
  fcast_data <- bdh(fcast_codes, c("PX_LAST"), start.date=start_date)
  #fcast_data[[1]]
  fcast <- fcast_data[names(fcast_data)==fcast_codes[1]][[1]]
  for (i in 2:length(fcast_data)) {
    fcast <- merge(fcast,fcast_data[names(fcast_data)==fcast_codes[i]][[1]], by="date", all=T)
  }
  colnames(fcast) <- c("date", fcast_dates_names)
  # print(tail(fcast))
  
  # Get outturns from BBG
  if (P==TRUE) {
    outturns_p <- bdh(outturn, c("ECO_RELEASE_DT", "ACTUAL_RELEASE", "BN_SURVEY_MEDIAN"), start.date = start_date, overrides = ovrd_p)
    outturns_f <- bdh(outturn, c("ECO_RELEASE_DT", "ACTUAL_RELEASE"), start.date = start_date, overrides = ovrd_f)
    
    if (outturns_f$ECO_RELEASE_DT[dim(outturns_f)[1]] == outturns_p$ECO_RELEASE_DT[dim(outturns_p)[1]]) {
      outturns_f <- outturns_f[-dim(outturns_f)[1],]
    }
  } else {
    outturns_p <- bdh(outturn, c("ECO_RELEASE_DT", "ACTUAL_RELEASE", "BN_SURVEY_MEDIAN"), start.date = start_date)
  }
  
  # Assign quarters
  outturns_p$period <- format(as.yearqtr(outturns_p$date), format = "Q%q:%y")
  if (P==TRUE) {
    outturns_f$period <- format(as.yearqtr(outturns_f$date), format = "Q%q:%y")
  }
  
  # Filter forecasts
  for (col in 2:(dim(fcast)[2]-3)) {
    if (sum(outturns_p$period==colnames(fcast)[col]) > 0) {
      period_end <- outturns_p[outturns_p$period==colnames(fcast)[col],]$ECO_RELEASE_DT
      period_start <- lubridate::floor_date(outturns_p[outturns_p$period==colnames(fcast)[col],]$date, unit = "quarter")
      fcast[!((fcast[,c(1,col)]$date >= period_start) & (fcast[,c(1,col)]$date <= period_end)),col] <- NA
    } else {
      period_start <- lubridate::floor_date(Sys.Date(), unit = "quarter")
      fcast[!(fcast[,c(1,col)]$date >= period_start),col] <- NA
    }
  }
  for (col in (dim(fcast)[2]-2):(dim(fcast)[2]-2)) {
    period_start <- lubridate::floor_date(Sys.Date() - days(90), unit = "quarter")
    fcast[!(fcast[,c(1,col)]$date >= period_start),col] <- NA
  }
  for (col in (dim(fcast)[2]-1):dim(fcast)[2]) {
    period_start <- lubridate::floor_date(Sys.Date() - days(60), unit = "quarter")
    fcast[!(fcast[,c(1,col)]$date >= period_start),col] <- NA
  }
  
  # Override data one week prior to release if there is ECO consensus
  for (i in 1:dim(outturns_p)[1]) {
    # print(i)
    # print(fcast[fcast$date >= outturns_p$ECO_RELEASE_DT[i]-7 & !is.na(fcast[,i+1]),])
    # print(fcast[fcast$date >= outturns_p$ECO_RELEASE_DT[i]-7 & !is.na(fcast[,i+1]),i+1])
    # print(outturns_p$BN_SURVEY_MEDIAN[i])
    fcast[fcast$date >= outturns_p$ECO_RELEASE_DT[i]-7 & !is.na(fcast[,i+1]),i+1] <- outturns_p$BN_SURVEY_MEDIAN[i]
  }
  
  # Override data one week prior to release if there is ECO consensus
  # if (is.na(outturns_p$ACTUAL_RELEASE[dim(outturns_p)[1]]) & !is.na(outturns_p$BN_SURVEY_MEDIAN[dim(outturns_p)[1]])) {
  #   fcast[fcast$date >= outturns_p$ECO_RELEASE_DT[dim(outturns_p)[1]]-7,dim(fcast)[2] - (4-quarter(outturns_p$date[dim(outturns_p)[1]]))] <- 
  #     outturns_p$BN_SURVEY_MEDIAN[dim(outturns_p)[1]]
  # }
  
  # Drop rows where no outturns yet
  outturns_p <- drop_na(outturns_p)
  if (P==TRUE) {
    outturns_f <- drop_na(outturns_f)
  }
  
  # Return results
  if (P==TRUE) {
    return(list(fcast, outturns_p, outturns_f))
  } else {
    return(list(fcast, outturns_p))
  }
}

# Takes list(forecast, preliminary print, final print) as input and plots
forecast.plot <- function(l, P=FALSE, series_name="Real GDP Growth (%qoq)", y_label="%", file_name)
{
  
  # Unpack inputs
  fcast <- l[[1]]
  outturns_p <- l[[2]]
  if (P==TRUE) {
    outturns_f <- l[[3]]
  }
  
  # Shift lines to make chart readable
  l <- unlist(lapply(fcast[,2:dim(fcast)[2]], as.numeric))
  if (P==TRUE) {
    range <- max(l[!is.na(l)], max(outturns_p$ACTUAL_RELEASE), max(outturns_f$ACTUAL_RELEASE), 0) - 
      min(l[!is.na(l)], min(outturns_p$ACTUAL_RELEASE), min(outturns_f$ACTUAL_RELEASE), 0)
    # maxi <- max(l[!is.na(l)], max(outturns_p$ACTUAL_RELEASE), max(outturns_f$ACTUAL_RELEASE))
    # mini <- min(l[!is.na(l)], min(outturns_p$ACTUAL_RELEASE), min(outturns_f$ACTUAL_RELEASE))
  } else {
    range <- max(l[!is.na(l)], max(outturns_p$ACTUAL_RELEASE), 0) - 
      min(l[!is.na(l)], min(outturns_p$ACTUAL_RELEASE), 0)
    # maxi <- max(l[!is.na(l)], max(outturns_p$ACTUAL_RELEASE))
    # mini <- min(l[!is.na(l)], min(outturns_p$ACTUAL_RELEASE))
  }
  
  fcast_jitter <- fcast
  for (i in 2:(dim(fcast_jitter)[2]-3)) {
    bool <- (abs(fcast_jitter[,i] - fcast_jitter[,i+1]) < range / 45)
    by <- abs(abs(fcast_jitter[,i] - fcast_jitter[,i+1]) - range / 45)/2
    bool[is.na(bool)] <- FALSE
    fcast_jitter[bool,i] <- fcast_jitter[bool,i] - by[bool]
    fcast_jitter[bool,i+1] <- fcast_jitter[bool,i+1] + by[bool]
  }
  #tail(fcast_jitter)
  
  # Tidy up last part of the graph
  gap1 <- (abs(fcast_jitter[,dim(fcast_jitter)[2]-2] - fcast_jitter[,dim(fcast_jitter)[2]-1]) < range / 45)
  gap1[is.na(gap1)] <- FALSE
  #gap1
  gap2 <- (abs(fcast_jitter[,dim(fcast_jitter)[2]-1] - fcast_jitter[,dim(fcast_jitter)[2]]) < range / 45)
  gap2[is.na(gap2)] <- FALSE
  #gap2
  gap3 <- (abs(fcast_jitter[,dim(fcast_jitter)[2]-2] - fcast_jitter[,dim(fcast_jitter)[2]]) < range / 45)
  gap3[is.na(gap3)] <- FALSE
  #gap3
  
  three <- gap1 & gap2 & gap3
  by <- abs(abs(fcast_jitter[,dim(fcast_jitter)[2]-2] - fcast_jitter[,dim(fcast_jitter)[2]]) - range / 45)/2
  fcast_jitter[three,dim(fcast_jitter)[2]-2] <- fcast_jitter[three,dim(fcast_jitter)[2]-2] - 2 * by[three]
  fcast_jitter[three,dim(fcast_jitter)[2]] <- fcast_jitter[three,dim(fcast_jitter)[2]] + 2 * by[three]
  by <- abs(abs(fcast_jitter[,dim(fcast_jitter)[2]-2] - fcast_jitter[,dim(fcast_jitter)[2]-1]) - range / 45)/2
  fcast_jitter[gap1 & !three,dim(fcast_jitter)[2]-2] <- fcast_jitter[gap1 & !three,dim(fcast_jitter)[2]-2] - by[gap1 & !three]
  fcast_jitter[gap1 & !three,dim(fcast_jitter)[2]-1] <- fcast_jitter[gap1 & !three,dim(fcast_jitter)[2]-1] + by[gap1 & !three]
  by <- abs(abs(fcast_jitter[,dim(fcast_jitter)[2]-1] - fcast_jitter[,dim(fcast_jitter)[2]]) - range / 45)/2
  fcast_jitter[gap2 & !three,dim(fcast_jitter)[2]-1] <- fcast_jitter[gap2 & !three,dim(fcast_jitter)[2]-1] - by[gap2 & !three]
  fcast_jitter[gap2 & !three,dim(fcast_jitter)[2]] <- fcast_jitter[gap2 & !three,dim(fcast_jitter)[2]] + by[gap2 & !three]
  by <- abs(abs(fcast_jitter[,dim(fcast_jitter)[2]-2] - fcast_jitter[,dim(fcast_jitter)[2]]) - range / 45)/2
  fcast_jitter[gap3 & !three,dim(fcast_jitter)[2]-2] <- fcast_jitter[gap3 & !three,dim(fcast_jitter)[2]-2] - by[gap3 & !three]
  fcast_jitter[gap3 & !three,dim(fcast_jitter)[2]] <- fcast_jitter[gap3 & !three,dim(fcast_jitter)[2]] + by[gap3 & !three]
  
  # Transfor data and graph
  fcast_trans <- melt(fcast_jitter, id = "date") 
  plt <- ggplot() + 
    geom_line(fcast_trans, mapping=aes(x=date, y=value, color=variable), size = 3) +
    geom_hline(yintercept=0.0, lty=4) +
    # scale_color_paletteer_d("ggthemes::calc") +
    scale_color_manual(values = c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 8))) +
    scale_x_date(breaks = "3 months",
                 date_labels=("%Y-%m")) +
    scale_y_continuous(n.breaks = 8)
  
  # if (maxi >= 0 & mini <= 0) {
  #   plt <- plt +
  #     geom_hline(yintercept=0.0, lty=4)
  # }
  
  
  if (P==TRUE) { 
    plt <- plt + 
      geom_point(data=outturns_p, aes(x=ECO_RELEASE_DT, y=ACTUAL_RELEASE, color=period), size=5, shape=4, stroke = 3) +
      geom_point(data=outturns_f, aes(x=ECO_RELEASE_DT, y=ACTUAL_RELEASE, color=period), size=5, shape=16, stroke = 3) +
      labs(title = paste(series_name, "- Evolution of Median Forecasts", sep=" "), 
           subtitle = "Median forecast, preliminary print, final print",
           y = y_label,
           caption = "Source: Bloomberg, Eisler Capital",
           color = NULL)
  } else {
    plt <- plt + 
      geom_point(data=outturns_p, aes(x=ECO_RELEASE_DT, y=ACTUAL_RELEASE, color=period), size=5, shape=16, stroke = 3) +
      labs(title = paste(series_name, "- Evolution of Median Forecasts", sep=" "), 
           subtitle = "Median forecast, final print",
           y = y_label,
           caption = "Source: Bloomberg, Eisler Capital",
           color = NULL)
  }
  # print(plt)
  
  # Save graph in output folder
  ggsave(paste("output/", file_name, "_", substr(start_year,3,4), ".png", sep=""), 
         plot = plt, 
         # device = "png",
         width = 8+3*(year(Sys.Date()) - as.numeric(start_year)), 
         height = 4+2*(year(Sys.Date()) - as.numeric(start_year)), 
         dpi = 140-30*(year(Sys.Date()) - as.numeric(start_year)), units = "in", device='png')
}


build.mb.dataset <- function(seriesList,labels,not.found.fill = NULL,freq='Monthly'){
  # Returns xts table containing all the data for the codes in 'seriesList' 
  # with column names in 'labels'
  
  # Set up MacroBond objects
  seriesRequest <- CreateUnifiedTimeSeriesRequest()
  setFrequency(seriesRequest, freq)
  
  # Turn each variable code into a MB Series and bind it to the request object
  for (i in seriesList) {addSeries(seriesRequest, i)}
  
  # Pull data from MacroBond
  seriesRequest     <- FetchTimeSeries(seriesRequest)
  
  # Get the series into an xts object, handling not found data as directed
  
  # Loop through all series requests
  data.list <- list()
  not.found <- c()
  for(ii in seq_along(seriesRequest)){
    series <- seriesRequest[[ii]]
    label  <- labels[[ii]]
    if(!grepl('Not found',getErrorMessage(series))){
      # If found, create an xts object and bind it to the dataset & attach name
      data.list[[label]] <- as.xts(series)
    }else{
      not.found <- c(not.found,label)
    }
  }
  
  # Merge found series & get time index
  seriesRequest.xts <- do.call("merge", data.list)
  names(seriesRequest.xts) <- names(data.list)
  idx <- index(seriesRequest.xts)
  
  # Deal with not found series as guided
  if(!is.null(not.found.fill)){
    for(label in not.found){
      len          <- dim(seriesRequest.xts)[1]
      dummy        <- xts(rep(not.found.fill,len),order.by = idx)
      names(dummy) <- label
      seriesRequest.xts <- merge(seriesRequest.xts,dummy)   
    } 
  }
  
  return(seriesRequest.xts)
}

rigorous.sa <- function(series, transform = 'auto',  double = FALSE, second.round = FALSE){
  # Takes a monthly xts series (levels) and performs a rigorous seasonal adjustment on it:
  #  - Uses full-year information to ensure full wavelengths are observed
  #  - X13-SEATS method is used
  #  
  #  Options
  #  - double: adjustment is performed twice to really strip seasonality
  #  - transform: sets the transform.function option (i.e. additive (none) or multiplicative (log))
  
  cat('\nSeasonally adjusting: ',names(series))
  
  # Define a useful object for getting indexes right
  dummy  <- xts(,order.by=as.Date(index(series)))
  tform  <- transform
  original.series <- series
  series <- na.omit(series)
  
  # Identify slice to use for full-year adjustment
  series.T         <- length(index(series))
  full.year.T      <- series.T - series.T %% 12
  full.year.slice  <- index(series)[1:full.year.T]
  full.year.series <- series[full.year.slice]
  first            <- start(full.year.series)
  last             <- end(full.year.series)
  actual.last      <- end(original.series %>% na.omit(.))
  
  # Perform adjustment, depending on options. Error handling in case of complete failure
  #    this tends to occur if a series is e.g. **only** seasonal as with regulated prices
  SA    <- tryCatch(seas(ts(full.year.series, start=c(year(first),month(first)), frequency=12), 
                         transform.function = tform, outlier = NULL),
                    error = function(e) e)
  
  # Extract the SA adjusted series if found, and run the process again if selected
  if(all(class(SA) == 'seas')){
    
    # Extract the SA series if the seas function worked
    temp.series        <- SA %>% final(.) %>% as.xts(.) %>% merge(., dummy)
    names(temp.series) <- names(series)
    
    # Run again if selected & seasonals were found the first time, otherwise store
    if(double == TRUE & !all(temp.series == series, na.rm = TRUE)){
      cat('\nNote: Going for second round\n')
      sa.series <- temp.series %>% rigorous.sa(., double = FALSE, transform = tform, second.round = TRUE)
    }else{
      sa.series <- temp.series
    }
    names(sa.series) <- names(series)
    
    if(all(temp.series == series, na.rm = TRUE)){
      cat('\nNote: No seasonal component detected, original series\n')
    }else{
      cat('\nNote: returning seasonally-adjusted series\n')
    }
    
    if(second.round == FALSE){
      
      # Roll seasonal factor forward for any observations not used in estimation
      seasonals.forward      <- repeat.last.season.m(series,sa.series)
      date.slice             <- paste(as.Date(last) + months(1),actual.last,sep='/')
      sa.series[date.slice]  <- series[date.slice] - seasonals.forward[date.slice]
    }
    
    
  }else{ # Otherwise just return the original
    cat('\nNote: X13 failed, returning original series\n')
    sa.series <- original.series
  }
  
  # Return
  return(sa.series)
}

