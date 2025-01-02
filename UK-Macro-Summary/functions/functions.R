
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
  idx <- zoo:::index(seriesRequest.xts)
  
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
  zoo:::index(sa.fwd) <- as.Date(zoo:::index(sa)) + years(1)
  
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
  dummy  <- xts(,order.by=as.Date(zoo:::index(series)))
  tform  <- transform
  original.series <- series
  series <- na.omit(series)
  
  # Identify slice to use for full-year adjustment
  series.T         <- length(zoo:::index(series))
  full.year.T      <- series.T - series.T %% 12
  full.year.slice  <- zoo:::index(series)[1:full.year.T]
  full.year.series <- series[full.year.slice]
  first            <- start(full.year.series)
  last             <- end(full.year.series)
  actual.last      <- end(original.series %>% na.omit(.))
  
  # Perform adjustment, depending on options. Error handling in case of complete failure
  #    this tends to occur is a series is e.g. **only** seasonal as with regulated prices
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

# transformations and Apply Them
transformations  <- list(
  'none'  = function(x) x,
  'fd'    = function(x) diff(x),
  'fd12'  = function(x) diff(x, lag = 12),
  'log'   = function(x) log(x),
  'logD'  = function(x) log(x) - lag(log(x)),
  'logD12'= function(x) log(x) - lag(log(x), 12),
  'logD4' = function(x) log(x) - lag(log(x), 4),
  'cop'   = function(x) 100*(x/lag(x) -1),
  'cop4'  = function(x) 100*(x/lag(x,4) -1),
  'norm1000' = function(x) x/1000
)

# Define the function to apply the transformations via the dictionary/list
transform_dataframe <- function(df, dict_id, transforms) {
  df.Trans <- df
  # Loop through column names in DataFrame
  for (col_name in names(df)) {
    x = which(unlist(names(df)) == col_name)
    # Check transformation function for this column
    if (col_name %in% names(dict_id)) {
      Transform.Codei  <- dict_id[[col_name]]
      # Apply the transformation function to the column
      df.Trans[ , x] <- transforms[[Transform.Codei]](df[ , x])
    }
  }
  return(df.Trans)
}

# ++++++++++++++++++++
# Seasonal Adjustment 
# ++++++++++++++++++++
f.sa <- function(id){
  
  # Need to make sure packages available in parallel clusters
  lapply(c('here','tidyverse','data.table','openxlsx','xts','seasonal','roll',
           'MacrobondAPI','comprehenr','parallel','Rblpapi',
           'tseries','forecast','tictoc'), 
         require, character.only = TRUE)
  
  # Identify & extract series 
  id.inflation = id[[1]]; id.component = id[[2]]
  
  # ...and send it for adjustment
  sa.series <- data.infl[[id.inflation]][['nsa']][,id.component] %>%
    rigorous.sa(., double = sa.twice, transform = 'none')
  
  return(sa.series)
}

f.sa.process <- function(inflation.measure, parallel = FALSE){
  
  # Build ID list for iterating
  ids <- list(); it = 1
  for(id.var in names(data.infl[[inflation.measure]][['nsa']])){
    ids[[it]] = c(inflation.measure,id.var)
    it = it + 1
  }
  
  # Do the adjustments over the iterable
  if(parallel == TRUE){
    
    # Set up clusters for execution, and set them to shut down on exit from the process
    cl <- parallel::makeCluster(detectCores() - 1)
    
    # Export the data to the clusters
    parallel::clusterExport(cl,list('data.infl','rigorous.sa','repeat.last.season.m',
                                    'sa.twice'))
    
    # Execute in parallel
    sa <- parLapply(cl, ids, fun = f.sa)
    
    # Shut down the clusters
    stopCluster(cl)
    
  } else {
    
    # Execute in serial
    sa <- lapply(ids, f.sa) 
    
  }
  
  return(sa %>% do.call(merge, .))
}



select.xvar.arima <- function(y,x,bic.threshold=5){
  # Assumes all series are stationary and seasonally adjusted already
  
  # Store some objects for use later
  possible.xvars <- names(x)
  chosen.xvars   <- NULL
  total.best     <- auto.arima(y,seasonal = FALSE,allowdrift= FALSE)$bic
  
  it = 1
  for(dummy in possible.xvars){
    
    local.best <- total.best
    dump.these <- c()
    
    for(xvar in possible.xvars){
      
      # Estimate optimal ARIMA model and check its fit against the last best
      model <- tryCatch(auto.arima(y, xreg = x[zoo:::index(y),c(chosen.xvars,xvar)], 
                                   seasonal = FALSE, allowdrift = FALSE),
                        error = function(e){FALSE})
      
      # If attempts to estimate the model throw up errors, reject this variable
      if(is.logical(model)){
        fit <- Inf
      }else{
        fit <- model$bic
      }
      
      if(fit < local.best){
        local.best   <- fit
        best.feature <- xvar
        best.model   <- model
      }
      
      # Dump as potentials variables that fail to improve on the total best by 
      #   a threshold as strict as the keep threshold
      if(total.best - fit < bic.threshold/2){
        dump.these <- c(dump.these,xvar)
      }
      
    }
    
    # Exit if the fitness didn't increase enough to matter, 
    #    otherwise add to the regressor set and iterate
    
    if(total.best - local.best < bic.threshold){ 
      return(chosen.xvars)
    }else{
      
      chosen.xvars  <- c(chosen.xvars,best.feature)
      possible.xvars<- possible.xvars[!(possible.xvars %in% c(best.feature,dump.these))]
      total.best    <- local.best
      
    }
    it = it+1
  }
  return(chosen.xvars)
}

forecast.series <- function(series.name,dat.y,dat.x,data.x.map,id.date,con = NULL){
  
  # ----------------------------------------------------------------------------
  # Data preparation
  # 
  # Prepare environment for results
  results <- list(forecast.sa = NULL, forecast.nsa = NULL, residuals = NULL)
  
  # Get series
  series.sa  <- dat.y$sa[ ,series.name]
  series.nsa <- dat.y$nsa[,series.name]
  
  # Make object to help results with correct dimensions
  dummy.xts <- xts(,order.by = seq(from = as.Date(start(series.sa)), 
                                   to   = as.Date(id.date), 
                                   by   = unclass(periodicity(series.sa))$label))
  series.sa  <- series.sa %>% merge(.,dummy.xts)
  series.nsa <- series.nsa %>% merge(.,dummy.xts)
  
  # Check if the target date is already present in the series; if so, just return
  if(!is.na(series.nsa[id.date])){
    
    # Change names
    names(series.nsa)   <- c('forecast.nsa')
    names(series.sa)    <- c('forecast.sa')
    
    # Collect model residuals
    res        <- xts(rep(0,length(series.sa)), order.by = zoo:::index(series.sa))
    names(res) <- c('model.resid')
    
    # Combine & return
    results <- merge(series.sa,res)
    results$forecast.nsa <- series.nsa
    
    cat('\nExiting - value available for ',series.name,'\n')
    return(results[,c('forecast.nsa','forecast.sa','model.resid')])
    
  }
  
  # Roll seasonal adjustment forward to get its forecasts
  series.seas.fcast <- repeat.last.season.m(series.nsa,series.sa)
  
  # Impose constraint if there is one and return
  if(!is.null(con)){
    if(series.name %in% con$code){
      
      constraint <- con[code == series.name]$constraint
      
      # Set constraint in the nsa forecast
      series.nsa[id.date] <- constraint
      names(series.nsa)   <- c('forecast.nsa')
      
      # Make a SA version as well
      series.sa[id.date]  <- constraint - series.seas.fcast[id.date]
      names(series.sa)    <- c('forecast.sa')
      
      # Collect model residuals
      res        <- xts(rep(0,length(series.sa)), order.by = zoo:::index(series.sa))
      names(res) <- c('model.resid')
      
      # Combine & return
      results <- merge(series.sa,res)
      results$forecast.nsa <- series.nsa
      
      cat('\nExiting - constaint imposed on',series.name,'\n')
      return(results[,c('forecast.nsa','forecast.sa','model.resid')])
      
    }
  }
  
  # Identify which of the x variables to allow for consideration. Test is EITHER
  #    match id is a sub-string of series name or vice versa
  x.var.stubs  <- lapply(data.x.map$match, function(x) grepl(x,series.name)) %>% 
    unlist(.) %>% data.x.map$Code[.] %>% gsub(' ','.',.)
  x.var.stubs  <- lapply(data.x.map$match, function(x) grepl(series.name,x)) %>% 
    unlist(.) %>% data.x.map$Code[.] %>% gsub(' ','.',.) %>% c(.,x.var.stubs) %>% unique(.)
  
  # Get the list of all them and their lags
  x.vars <- c()
  for(v in names(dat.x)){
    if(lapply(x.var.stubs, function(x) grepl(x,v)) %>% unlist(.) %>% any(.)){
      x.vars <- c(x.vars,v)
    }
  }
  xDat <- dat.x[,x.vars]
  
  # ----------------------------------------------------------------------------
  # Drop anything without an observation in the target date
  
  # TODO: Could be worth doing lags at this point so can set further ahead horizons
  if(length(xDat) != 0){ xDat <- xDat[,!is.na(xDat[id.date])] }
  
  # ----------------------------------------------------------------------------
  # Model selection
  tic()
  if(length(xDat) != 0){ 
    x.vars.optimal <- select.xvar.arima(series.sa, xDat, bic.threshold = 5)
  } else{
    x.vars.optimal <- NULL
  }
  cat('\n',series.name,' = ',x.vars.optimal,'\n')
  toc()
  # ----------------------------------------------------------------------------
  # Estimate model & forecast
  
  if(is.null(x.vars.optimal)){
    
    model <- auto.arima(series.sa,
                        seasonal = FALSE, allowdrift = FALSE)
    
    h.fcast            <- interval(end(na.omit(series.sa)),
                                   as.Date(id.date)) %/% months(1)
    series.sa[id.date] <- tail(forecast::forecast(model,h=h.fcast)$mean,1)
    names(series.sa)   <- c('forecast.sa')
    
  }else{
    
    model <- auto.arima(series.sa, xreg = xDat[zoo:::index(series.sa),x.vars.optimal], 
                        seasonal = FALSE, allowdrift = FALSE)
    
    series.sa[id.date] <- predict(model, newxreg = xDat[zoo:::index(series.sa),x.vars.optimal])$pred %>%
      as.numeric(.) %>% as.xts(., order.by = zoo:::index(series.sa)) %>% tail(.,1)
    names(series.sa)   <- c('forecast.sa')
  }
  
  # Collect model residuals
  res        <- model$resid
  tail       <- length(series.sa) - length(res)
  res        <- xts(c(rep(NA,tail),res), order.by = zoo:::index(series.sa))
  names(res) <- c('model.resid')
  
  # ----------------------------------------------------------------------------
  # Make & store SA forecast, make NSA & returns
  
  results <- merge(series.sa,res)
  results$forecast.nsa <- series.nsa
  results$forecast.nsa[id.date] <- results$forecast.sa[id.date] + series.seas.fcast[id.date]
  
  return(results[,c('forecast.nsa','forecast.sa','model.resid')])
}

bundle.results   <- function(fcast.data){
  
  results <- list()
  
  for(id.inflation in names(fcast.data)){
    results[[id.inflation]] <- list(sa = list(), nsa = list(), resid = list())
    
    it = 1
    for(id.series in names(fcast.data[[id.inflation]])){
      
      dat.nsa          <- fcast.data[[id.inflation]][[id.series]]$forecast.nsa
      names(dat.nsa)   <- id.series
      results[[id.inflation]][['nsa']][[it]]   <- dat.nsa
      
      dat.sa           <- fcast.data[[id.inflation]][[id.series]]$forecast.sa
      names(dat.sa)    <- id.series
      results[[id.inflation]][['sa']][[it]]    <- dat.sa
      
      dat.resid        <- fcast.data[[id.inflation]][[id.series]]$model.resid
      names(dat.resid) <- id.series
      results[[id.inflation]][['resid']][[it]] <- dat.resid
      
      it = it + 1
    }
    
    # Return results, re-framed as growth rates (from log differences)
    results[[id.inflation]]$nsa   <- do.call(merge,results[[id.inflation]]$nsa)   %>% exp(.) - 1
    results[[id.inflation]]$sa    <- do.call(merge,results[[id.inflation]]$sa)    %>% exp(.) - 1
    results[[id.inflation]]$resid <- do.call(merge,results[[id.inflation]]$resid) %>% exp(.) - 1
    
  }
  
  return(results)
}

aggregate.result <- function(result, weights,
                             label         = 'AGG',
                             use.columns   = NULL,
                             linear.approx = FALSE){
  
  result.mom <- result
  idx        <- zoo:::index(result)
  
  # Now affect the aggregation
  if(linear.approx == FALSE){
    # This leg implements the correct aggregation, taking chaining etc into 
    #    account outlined in Section 10.6 in the ONS Technical Manual
    
    # First make growth rates mom price relatives
    relatives.mom <- 1 + result.mom
    
    # Next make Mar-Dec price relatives to January (Feb already is, and Jan 
    #    needs to be relative to Dec)
    relatives.unchained <- relatives.mom
    
    for(m in 3:12){
      
      temp     <- roll_prod(relatives.mom,m-1) # i.e. for March roll the Feb and Mar realtives together
      temp.idx <- zoo:::index(relatives.unchained)[month(zoo:::index(relatives.unchained)) == m]
      
      for(id.var in names(relatives.unchained)){
        relatives.unchained[temp.idx,id.var] <- temp[temp.idx,id.var]
      }
    }
    
    # Find the weighted average of these price relatives
    if(is.null(use.columns)){
      agg.unchained <- xts(rowSums(relatives.unchained * weights[idx]) / rowSums(weights[idx]), 
                           order.by = idx)
    }else{
      agg.unchained <- xts(rowSums(relatives.unchained[,use.columns] * weights[idx,use.columns]) / rowSums(weights[zoo:::index(result),use.columns]), 
                           order.by = idx)
    }
    
    # Turn into monthly growth rates
    agg <- agg.unchained
    for(m in 1:12){
      if(m %in% c(1,2)){
        agg[month(zoo:::index(agg)) == m] <- agg.unchained[month(zoo:::index(agg.unchained)) == m] - 1
      }else{
        agg[month(zoo:::index(agg)) == m] <- growth(agg.unchained) %>% .[month(zoo:::index(agg.unchained)) == m]
      }
    }
    
  }else{
    # This leg just aggregates growth rates using a weighted sum
    if(is.null(use.columns)){
      agg <- xts(rowSums(result * weights[idx]) / rowSums(weights[idx]), 
                 order.by = idx)
    } else{
      agg <- xts(rowSums(result[,use.columns] * weights[idx,use.columns]) / rowSums(weights[zoo:::index(result),use.columns]), 
                 order.by = idx)
    }
  }
  
  names(agg) <- label
  return(agg)
}

chained.contributions <- function(series, result, weights,
                                  label         = 'AGG',
                                  use.columns   = NULL,
                                  annual        = FALSE){
  # This function produces contributions to **annual** aggregate growth from sub-aggregates
  #    based on the old contributions formula, from before 2017 when the ONS started 
  #    using different weights between January and February (Macrobond doesn't 
  #    publish the different weights). See the example calculation in 
  #    S9.7.1 in the 2014 CPI Technical Manual. Note if 
  
  #    The function assumes you have passed monthly growth rates, not indices. And
  #    affects all the unchaining etc under this assumption
  
  # Check the contribution series you want isn't excluded by column choices
  if(!is.null(use.columns)){
    if(!(series %in% use.columns)){
      cat("\nWARNING: you've asked for contribution from a variable that's not included in the aggregation you've selected\n")
      return(NULL)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Prepare the variables for the formula
  
  # First calculate the aggregate & make it into a faux index
  agg    <- aggregate.result(result, weights, use.columns = use.columns) %>% na.omit(.)
  agg    <- cumprod(1 + agg[,label])
  
  # Next make a faux index of the series
  disagg <- cumprod(1 + na.omit(result[,series]))
  
  # And now unchain both aggregate and disaggreagate series
  vars   <- merge(unchain(agg),
                  unchain(disagg))
  names(vars) <- c('agg.mj','agg.jd','agg.djlag',
                   'ser.mj','ser.jd','ser.djlag')
  
  # Identify which rows are January
  januaries <- (month(zoo:::index(vars))==1)
  
  # And collect weights
  vars$wgt <- weights[,series]
  vars$j.wgt <- NA
  vars[(month(zoo:::index(vars))==1),]$j.wgt <- vars[(month(zoo:::index(vars))==1),]$wgt
  vars$j.wgt <- vars$j.wgt %>% na.locf(.) 
  vars <- na.omit(vars)
  
  # ----------------------------------------------------------------------------
  # Implement the formula
  
  if(annual == TRUE){
    first  <- 100         * (lag(vars$wgt,12)/100) * (vars$ser.djlag - lag(vars$ser.mj,12))
    second <- vars$agg.dj * (vars$j.wgt/100)       * (vars$ser.jd    - 100)
    third  <- vars$agg.dj * (vars$wgt/100)         * (vars$ser.mj    - 100) * (vars$agg.jd/100)
    cont   <- (first + second + third) / lag(vars$agg.mj,12)
  } else{
    cont.mj<- vars$wgt   * diff(vars$ser.mj) / lag(vars$agg.mj,1)
    cont.j <- vars$j.wgt * diff(vars$ser.jd) / 100
    cont   <- (month(zoo:::index(vars))!=1) * cont.mj + (month(zoo:::index(vars))==1) * cont.j
  }
  
  return(cont)
  
}

unchain <- function(series){
  # Takes a series of chained index and returns two columns: 
  #   1. the unchained index relative to January
  #   2. each year's January index relative to the prior December
  
  growth <- series / lag(series) - 1
  result <- xts(,order.by = zoo:::index(series))
  
  # Make the unchained index with base January
  result$Ijan <- NA
  result[month(zoo:::index(result))==1,]$Ijan <- series[month(zoo:::index(series))==1,] %>% as.numeric(.)
  result$Ijan <- result$Ijan %>% na.locf(.) 
  result$Ijan <- 100 * series / result$Ijan
  
  # Make the January index unchained to December
  result$Ijdec <- NA
  result[month(zoo:::index(result))==1,]$Ijdec <- (1 + growth[month(zoo:::index(growth))==1,]) %>% as.numeric(.)
  result$Ijdec <- 100 * result$Ijdec %>% na.locf(.)
  
  # Grab last year's Dec base Jan index
  result$Idjan.lag <- NA
  result$Idjan.lag[month(zoo:::index(result))==12,] <- result$Ijan[month(zoo:::index(result))==12,] %>% as.numeric(.)
  result$Idjan.lag <- result$Idjan.lag %>% na.locf(.)
  result$Idjan.lag[month(zoo:::index(result))==12,] <- NA
  result$Idjan.lag <- result$Idjan.lag %>% na.locf(.)
  
  return(result)
}

unchain.new <- function(series){
  # Takes a series of growth rates and returns an unchained index relative to 
  #    appropriate base month
  
  # Make a faux index from the growth rates
  index.dat        <- (1 + na.omit(series)) %>% cumprod(.)
  names(zoo:::index.dat) <- 'chain'
  
  # Identify the prior base.month value for each observation
  index.dat$chain.jan <- index.dat$chain
  index.dat$chain.jan[month(index(index.dat)) != 1] <- NA
  index.dat$chain.jan <- index.dat$chain.jan %>% na.locf(.)
  index.dat$unchain.jan <- index.dat$chain / index.dat$chain.jan
  
  index.dat$chain.dec <- index.dat$chain
  index.dat$chain.dec[month(index(index.dat)) != 12] <- NA
  index.dat$chain.dec <- index.dat$chain.dec %>% na.locf(.)
  index.dat$unhcain.dec <- index.dat$chain / index.dat$chain.dec
    
  # Now unchain based on month
  index.unchain <- index.dat$unchain.jan
  index.unchain[month(index(index.unchain)) == 1] <- index.dat[month(index(index.dat)) == 1]$unhcain.dec
  
  names(index.unchain) <- names(series)
  
  return(index.unchain)
}

make.chain.aggregate <- function(data, weight, columns = NULL, label = 'AGG'){
  
  # Aggregate all passed column unless directed to do otherwise
  if(is.null(columns)){ columns <- names(data) }
  
  # Unchain
  unchained <- lapply(data[,columns],unchain.new) %>% do.call(merge,.)
  
  # Aggregate
  agg <- rowSums(unchained * weight[index(unchained),columns]) / rowSums(weight[index(unchained),columns]) %>%
    xts(.,order.by = index(unchained))
  names(agg) <- label
  
  # Convert to growth rates
  growth <- agg / lag(agg) - 1
  growth[month(index(agg)) == 1] <- agg[month(index(agg)) == 1] - 1
  growth[month(index(agg)) == 2] <- agg[month(index(agg)) == 2] - 1
  
  # Return growth rate
  return(growth)
  
}

unchain.rpi <- function(series){
  # Takes a series in monthly growth rates; returns an unchained index normalised
  #    to the previous January
  
  # Make a faux index from the growth rates
  index.chained <- (1 + na.omit(series)) %>% cumprod(.)
  names(index.chained) <- 'chain'
  
  # Identify the prior January's value for each observation
  index.chained$last.jan <- NA
  index.chained[month(index(index.chained))==1,]$last.jan <- index.chained[month(index(index.chained))==1,]$chain
  index.chained$last.jan <- index.chained$last.jan %>% na.locf(.)
  
  # And for Januaries, use the January of last year
  index.chained[month(index(index.chained))==1,]$last.jan <- NA
  index.chained$last.jan <- index.chained$last.jan %>% na.locf(.)
  
  # Unchain and return
  index.unchained        <- index.chained$chain / index.chained$last.jan
  names(index.unchained) <- names(series)
  
  return(index.unchained)
}

aggregate.rpi <- function(data, weights,
                          label         = 'AGG',
                          use.columns   = NULL,
                          linear.approx = FALSE){
  
  
  if(is.null(use.columns)){
    result.mom <- data
    weights    <- lag(weights) # Note need to lag weights so January is weighted with prior year weights
  } else{
    result.mom <- data[,use.columns]
    weights    <- lag(weights[,use.columns])
  }
  
  # Now affect the aggregation
  if(linear.approx == FALSE){
    # This leg implements the correct aggregation, taking chaining etc into 
    #    account outlined in Section 12.7.3 in the 2019 ONS Technical Manual
    
    # First turn the mom growth rates into unchained indices
    f.process           <- function(series.name){return(unchain.rpi(result.mom[,series.name]))}
    relatives.unchained <- lapply(names(result.mom), f.process) %>% do.call(merge,.) %>% na.omit(.)
    idx                 <- index(relatives.unchained)
    
    # Find the weighted average of these price relatives to get the aggregate unchained index
    agg.unchained <- xts(rowSums(relatives.unchained[idx] * weights[idx]) / rowSums(weights[idx]), 
                         order.by = idx)
    
    # Turn into monthly growth rates
    agg <- copy(agg.unchained)
    for(m in 1:12){
      if(m == 2){
        agg[month(index(agg)) == m] <- agg.unchained[month(index(agg.unchained)) == m] - 1
      }else{
        agg[month(index(agg)) == m] <- growth(agg.unchained) %>% .[month(index(agg.unchained)) == m]
      }
    }
    
  }else{
    # This leg just aggregates growth rates using a weighted sum -> not correct!
    idx <- index(result.mom)
    agg <- xts(rowSums(result.mom * weights[idx]) / rowSums(weights[idx]), 
               order.by = idx)
  }
  
  names(agg) <- label
  return(agg)
  
}

get.variance.contributions <- function(data,weights,agg.name){
  # Returns contributions of components to the variance in an aggregate that is
  #   defined by their weighted sum. Method uses the fact that when an aggregate
  #   is defined 
  #         agg = sum_j(component_j * weight_j)
  #   then the variance of the aggregate is 
  #         var(agg) = sum(var(w*components)) + 2*sum(cov(w_i*components_i,w_j*components_j))
  #   so a single component i's contribution to the aggregate is
  #         cont_i = sum_j (cov(w_i * component_i, w_j * component_j)) / var(sum_j(w_j * component_j))
  #   which is the OLS formula for the coefficient B in the regression 
  #         w_i*component_i = B_i * sum_j(component_j * weight_j) + u
  
  # Define aggregate and contributions to same
  df.contributions <- data * weights
  df.agg           <- xts(rowSums(df.contributions),
                          order.by = index(df.contributions))
  names(df.agg)    <- c(agg.name)
  df.contributions <- merge(df.agg,df.contributions)
  
  # Find contributions to aggregate by OLS
  cont    <- c()
  sig     <- c()
  normal  <- copy(df.contributions)
  wgts    <- c()
  corr    <- c()
  for(var in names(data)){
    mod          <- lm(data = df.contributions, as.formula(paste(var,'~ 0 + ',agg.name,sep='')))
    cont         <- c(cont,mod$coefficients[[agg.name]])
    
    corr          <- c(corr,cor(df.agg,data[,var],use='pairwise.complete.obs'))
    wgts         <- c(wgts,tail(weights[,var],1))
    sig          <- c(sig,sd(data[,var],na.rm = TRUE))
    normal[,var] <- (df.contributions[,var]/df.contributions[,agg.name])/mod$coefficients[[agg.name]] 
  }
  var.cont <- data.frame(list(Label   = names(data), 
                              weight  = wgts,
                              stdev   = sig,
                              correlation= corr,
                              variance.contribution = 100*cont)) %>% arrange(desc(variance.contribution))
  
  
  return(list(variance.contribution           = var.cont,
              contribution.relative.to.normal = normal))
  
}

growth <- function(series,lags=1){
  return(series / lag(series,lags) - 1)
}

# ----------------------------------------------------------------
# GARBAGE
  
# remove.subcomponent <- function(agg,agg_w,sub,sub_w){
#   # Removes influence of a sub-component from an aggregate series that is a simple
#   # weighted average.
#   # 
#   # Returns the new aggregated series ex- the appropriate sub-component, as well 
#   # as its new weight
#   
#   w_ex   = agg_w - sub_w
#   agg_ex = (agg * agg_w - sub * sub_w) / w_ex
#   
#   return(list(new.series = agg_ex, 
#               new.weight = w_ex))
# }
# 
# 
# 
# get.ancestor <- function(generation,descendent){
#   # Finds the 'youngest' ancestor in the generation for a particular descendent, 
#   #    using name structure grandparent_parent_child
#   
#   bestGuess <- ''
#   for(f in generation){
#     if(grepl(f,descendent) & nchar(f) > nchar(bestGuess)){
#       bestGuess <- f
#     }
#   }
#   if(nchar(bestGuess)==0){
#     return(descendent)
#   }else{
#     return(bestGuess)
#   }
# }
# 
# my.weighted.sum <- function(data,weights){
#   # Weighted average that works for tx & xts objects as well as others
#   return(rowSums(data * weights) / rowSums(weights))
# }
# 
# add.xts.column <- function(data.xts,new.col,new.col.name){
#   new.xts        <- xts(new.col,index(data.xts))
#   names(new.xts) <- new.col.name
#   return(merge(data.xts,new.xts))
#   
# }
# 
# drop.redundant.children <- function(vector){
#   # For a vector of items that are hierarchically related, makes sure that 
#   # there are no redundant elements i.e. children whose ancestors are already in 
#   # the vector. Assumed ancestry is contained in the element string
#   # e.g. 'x' is an ancestor to 'xx', and 'xx' is therefore redundant.
# 
#   clean.vector <- vector
#   for(v in vector){
#     
#     # Check if any of the other items are substrings of v
#     for(p in vector[!vector == v]){
#       
#       # Drop v if they are -> it's redundant
#       if(grepl(p,v)){
#         clean.vector <- clean.vector[!clean.vector == v]
#         next
#       }
#     }
#   }
#   return(clean.vector)
# }
# 
# group.vars <- function(group.dt,label){
#   return(group.dt[eval(parse(text=label))==TRUE,CPI.code])
# }
# 
# get.adhoc.set <- function(data,weights,target,map,exclude=NULL,include=NULL,combine=NULL){
#   # Selects which series in a full hierarchy to use in a hamronised dataset.
#   # 
#   # Variables:
#   # - 'data' (xts object): contains raw data for analysi
#   # - 'weights' (xts object): contains weights for aggregation of above, must
#   #       have identical dim to 'data'
#   # - 'level' (int): identifies the level of aggregation mainly interested in
#   # - 'exclude' (list of str): must name variables at same or lower level as 'level'
#   # - 'include' (list of str): must name variables at lower level than 'level'
#   #
#   # Process:
#   #    - If just 'level' is selected, then the function drops any observations in
#   #       'data' not at that level and makes sure the equivalent weights add to 1.
#   #    - If 'exclude' is specified, these variables are dropped IF at the level 
#   #        specified, or influence removed if lower
#   #    - If 'include' is specificed, these variables at lower levels are promoted
#   #    Weights are adjusted as appropriate, depending on the exclude & include options chosen
#   
#   #-----------------------------------------------------------------------------
#   # Checks
#   
#   # Remove potential double counting in the 'exclude' choices
#   exclude <- drop.redundant.children(exclude)
#   
#   # Ensure none of the 'include' variables are under 'excluded' ones
#   for(x in exclude){
#     for(n in include){
#       if(grepl(x,n)){
#         print(paste('Exclude: ',x,', Include: ',n,sep=''))
#         stop("ERROR: get.set() has been instructed to 'include' a component that is subordinate to an 'excluded' one")
#       }
#     }
#   }
#   
#   #-----------------------------------------------------------------------------
#   # Find the descendents of 'target' & identify the level of aggregation we're working at
#   
#   relevant.vars <- map$Label[sapply(map$Label,grepl,pattern=target)]
#   relevant.vars <- relevant.vars[!relevant.vars == target]
#   
#   level         <- map[Label == target]$Level + 1
#   levelMap      <- map[Label %in% relevant.vars,c('Label','Level')]
#   
#   #-----------------------------------------------------------------------------
#   # Remove branches of the tree that stem from excluded variables at the level 
#   # we're interested in
#   
#   # Find top-level exclusions and their descendents, remove from the map
#   exclude.top <- levelMap[(Level == level & Label %in% exclude)]$Label
#   exclude.low <- c()
#   for(v in exclude.top){
#     exclude.these.too <- relevant.vars[sapply(relevant.vars,grepl,pattern=v)]
#     exclude.low       <- c(exclude.low,exclude.these.too)
#   }
#   levelMap <- levelMap[!(Label %in% c(exclude.top,exclude.low))]
#   
#   # Keep only remaining variables and their weights
#   df       <- data[   ,names(data   ) %in% levelMap$Label]
#   wt       <- weights[,names(weights) %in% levelMap$Label]
#   
#   # Trim the exclusion list to include only variables left
#   exclude.trimmed  <- exclude[(exclude %in% levelMap$Label)]
#   
#   # NOTE: from this point, remaining exclusions and inclusions are descendents of 
#   #   the remaining components at 'level'
#   
#   #-----------------------------------------------------------------------------  
#   # Remove the contributions of low level excluded and included variables from 
#   #    their ancestors
#   
#   # Identify the ancestors at 'level' and remove their contribution
#   for(sub in c(include,exclude.trimmed)){
#     agg      <- get.ancestor(levelMap[Level==level]$Label,sub)
#     agg_ex   <- remove.subcomponent(df[,agg],wt[,agg],df[,sub],wt[,sub])
#     df[,agg] <- agg_ex$new.series
#     wt[,agg] <- agg_ex$new.weight
#   }
#   
#   # ...and elevate included variables to 'level' in the map
#   levelMap[Label %in% include]$Level <- level
#   
#   #-----------------------------------------------------------------------------  
#   # Keep only variables at 'level'
#   
#   chosen <- levelMap[Level == level]$Label
#   df     <- df[,chosen]
#   wt     <- wt[,chosen]
#   
#   #-----------------------------------------------------------------------------  
#   # Do combinations
#   
#   all.combines <- to_vec(for(cc in combine) cc)
#   
#   # Make sure there are no conflicts i.e. double-counting
#   if(any(duplicated(all.combines))){
#     print(full.list[duplicated(all.combines)])
#     stop('ERROR: combine list is trying to double count the above variable.')
#   }
#   
#   # Make sure all combine variables are at the right level
#   if(!all(all.combines %in% chosen)){
#     print('Offending combine: ')
#     print(com)
#     stop('ERROR: instruction to combine at least one variable that is either excluded, or wrong level of aggregation')
#   }
#   
#   for(cc in names(combine)){
# 
#     vars    <- combine[[cc]]
# 
#     # Make combination of data and weights
#     df      <- add.xts.column(df,my.weighted.sum(df[,vars],wt[,vars]),cc)
#     wt      <- add.xts.column(wt,rowSums(wt[,vars]),cc)
#     
#     # Drop old series
#     keep    <- names(df)
#     keep    <- keep[!(keep %in% vars)]
#     df      <- df[,keep]
#     wt      <- wt[,keep]
# 
#   }
#   
#   return(list(data            = df, 
#               weight          = wt, 
#               excluded.data   = data[,exclude],
#               excluded.weight = weights[,exclude]))
#   
# }
# 
# bottom.up.forecast <- function(data,weight,horizon,agg.name='AGG'){
#   # Assumes monthly data. Produces forecast for each column of 'data' and then 
#   # aggregates with weight, returning data, forecasts at top & low level, and contributions
#   
#   # Initialise some useful things
#   year.start  <- year(start(data))
#   month.start <- month(start(data))
#   
#   fcasts <- list()
#   wts    <- list()
#   for(idx in names(data)){
#     
#     # Presently, each series is forecast in the same way - an optimally chosen,
#     # potentially seasonally adjusted ARIMA model. In future, this step could push
#     # to a series-dependent script that performs different forecasting approach to
#     # each depending on what we know
#     fcast.temp   <- auto.arima(data[,idx],seasonal = TRUE) %>% forecast(., h = horizon)
#     fcasts[[idx]]<- c(fcast.temp$x, fcast.temp$mean)
#     
#   }
#   forecasts <- data.frame(fcasts) %>% 
#     ts(., start=c(year.start,month.start), frequency=12) %>% 
#     as.xts(.)
#   
#   # Make sure the weight matrix has the same dimensions as the data+forecast
#   if(as.Date(end(weight)) > as.Date(end(forecasts))){
#     forecasts.wt <- weight[index(forecasts)]
#   }else{
#     forecasts.wt <- merge(weight, xts(,order.by = index(forecasts))) %>% na.locf(.)
#   }
#   
#   # Aggregate
#   forecast.cont<- forecasts * forecasts.wt
#   forecast.agg <- as.xts(my.weighted.sum(forecasts, forecasts.wt), 
#                          order.by = index(forecasts))
#   names(forecast.agg) <- agg.name
#   
#   
#   # Return aggregate & contributions, sub-components, and weights
#   return(list(aggregate         = forecast.agg,
#               contributions     = forecast.cont,
#               components        = forecasts,
#               component.weights = forecasts.wt))
# }
# 


