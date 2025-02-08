
# Chart Settings
sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 20, base_family = "Roboto Condensed") +
            theme(
              plot.subtitle = element_text(size = rel(1.0),
                                           margin=margin(4,0,0,0)),
              text = element_text(family = "Roboto Condensed", size = 30), # size=30 for pdf
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(linewidth = .5),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "dodgerblue"),
              legend.position = "bottom"              ))
showtext_auto()

# STATS: growth, annualised_3m
growth <- function(series, lags=1) {
  return(series / lag(series, lags)-1 )
}
# calculate 3m annualised rate; applied to SA-index
annualise_3m <- function(series) {
  pc_3m3m = rollapplyr(series, 3, mean, fill = NA) / lag(rollapplyr(series, 3, mean, fill = NA), 3)-1
  ann_3m  = ((1+pc_3m3m)^4) - 1
  ann_3m
}

# Summarise columns
summ_cols <- function(data, ...) {
  data |> 
    select_if(is.numeric(...)) |> 
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") |> 
    group_by("var") |> 
    summarize(mean   = mean(val, na.rm = TRUE),
              sd     = sd(val,   na.rm = TRUE),
              min    = min(val,  na.rm = TRUE),
              max    = max(val,  na.rm = TRUE))
}

# DATA
build.mb.dataset <- function(seriesList,labels = NULL,not.found.fill = NULL,freq='Monthly'){
  # Returns xts table containing all the data for the codes in 'seriesList' 
  # with column names in 'labels'
  
  if(is.null(labels)){
    labels <- seriesList
  }
  
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
  idx <- zoo::index(seriesRequest.xts)
  
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

repeat.last.season.m <- function(series,series.sa){
  
  # Get seasonal adjustment factor
  sa     <- round(series - series.sa,10) %>% as.xts(.)
  sa.fwd <- copy(sa)
  
  # Roll it forward by one cycle
  zoo::index(sa.fwd) <- as.Date(zoo::index(sa)) + years(1)
  
  # Merge together & replace missing values with the forward series
  temp        <- merge(sa,sa.fwd)
  names(temp) <- c('sa','sa.fwd')
  temp[is.na(temp[,'sa']),'sa'] <- temp[is.na(temp[,'sa']),'sa.fwd']
  
  return(temp[,'sa'])
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
  dummy  <- xts(,order.by=as.Date(zoo::index(series)))
  tform  <- transform
  original.series <- series
  series <- na.omit(series)
  
  # Identify slice to use for full-year adjustment
  series.T         <- length(zoo::index(series))
  full.year.T      <- series.T - series.T %% 12
  full.year.slice  <- zoo::index(series)[1:full.year.T]
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

