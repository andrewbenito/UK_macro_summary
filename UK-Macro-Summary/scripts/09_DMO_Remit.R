
#graphics.off()
# Load standard libraries 
lapply(c('here','tidyverse','data.table','openxlsx','xts', 'varhandle',
         'Rblpapi', 'sysfonts', 'showtext', 'MacrobondAPI'), 
       require, character.only = TRUE)

here::here()

# xldate<- function(x) {
#   xn <- as.numeric(x)
#   return(as.Date(xn, origin="1899-12-30"))
# }

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


# ==============================================================================
# remit data
# ==============================================================================

remits <- read.xlsx(here('inputs','past-remits-april-2024.xlsx'))

remits[3,is.na(remits[3,])] <- remits[2,is.na(remits[3,])]
colnames(remits) <- tolower(c("year", "event", remits[3,-1:-2]))
remits <- remits[-1:-3,]
remits[rowSums(!is.na(remits[,3:8])) == 0,]$year <- remits[rowSums(!is.na(remits[,3:8])) == 0,]$event
remits <- remits %>% tidyr::fill(year, .direction = "down")
remits <- remits[rowSums(!is.na(remits[,3:8])) != 0,]
for (i in 1:dim(remits)[1]) {
  remits$year[i] <- strsplit(remits$year[[i]], split = "-")[[1]][1]
}
remits <- remits %>% 
  mutate_at(vars(colnames(remits)[3:8]), function(x) as.numeric(as.character(x)))
remits$type <- NA
remits$type[remits$event == "Outturn"] <- "outturn"
for (i in 2:dim(remits)[1]) {
  if(remits$year[i-1] != remits$year[i]) {
    remits$type[i] = "initial"
  }
}
remits$type[1] <- "initial"
remits$type[is.na(remits$type)] <- "revision"


remits <- remits[!is.na(remits$date),c("date", "total", "type")]
remits <- remits[order(as.Date(remits$date)),]
remits$remit_change <- 100 * (remits$total - lag(remits$total)) / lag(remits$total)
remits <- remits[remits$date >= "2000-01-01",]

remits$revision <- as.numeric(remits$type == "revision")
remits$year     <- year(as.Date(remits[,'date']))

remits$final <- 0
for (i in 1:(nrow(remits)-1)) {
  if ((remits$revision[i] == 1) & (remits$revision[i+1] == 0)) {
    remits$final[i] <- 1
  }
}
remits[remits$date=="2020-01-07",]$year <- 2019



#------------------
# Key Chart: UK 
#------------------
dmo.remit <- ggplot() +
  geom_col(remits[remits$revision != 1,], mapping = aes(year, total), width = 0.5, fill = "darkgrey") +
  geom_point(remits[remits$revision == 1,],
             mapping =  aes(year, total),
             color = "blue", size = 4) +
  geom_point(remits[(remits$revision == 1) & (remits$final == 1),],
             mapping =  aes(year, total),
             color = "black", size = 4) +
  geom_point(remits[remits$date == "2022-09-23",],
           mapping =  aes(year, total),
           color = "red", size = 4) +
  geom_label(aes(x = 2022, y = 220, label="Truss")) + 
  geom_point(remits[remits$date == "2024-10-30",],
           mapping =  aes(year, total),
           color = "black", fill = "white", size = 4, stroke = 1, shape = 21) +
  geom_label(aes(x = 2024, y = 325, label="Reeves")) +
  scale_x_continuous(n.breaks = 10, ) +
  scale_y_continuous(n.breaks = 15) +
  labs(title = "UK DMO Remits for annual Gilt Issuance",
       subtitle = "Initial, subsequent and final remits",
       y = "£ bn",
       caption = "Source: Debt Management Office (DMO)") +
  theme(legend.position="bottom",
        axis.title.x=element_blank())

