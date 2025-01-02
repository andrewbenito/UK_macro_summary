# Tracking UK Growth Consensus Forecasts 

# Load standard libraries
lapply(c('here','tidyverse','data.table', 'readxl', 'openxlsx', 'broom', 'janitor', 'stringi',
         'MacrobondAPI','Rblpapi','ggridges', 'ggsci', 'parallel','glue', 'stringr', 'paletteer',
         'tseries', 'seasonal', 'forecast', 'tsibble', 'xts', 'zoo', 'gtsummary', 'gtExtras',
         'latex2exp', 'stargazer', 'sysfonts', 'showtext', 'patchwork'), 
       require, character.only = TRUE)

blpConnect()

# INPUT TICKERS
#================
fcast.series <- c('ECGQGB')  # UK GDP, %qoq Consensus; Unemployment rate
fcast.dates <- c('Q122', 'Q222', 'Q322', 'Q422', 'Q123', 'Q223', 'Q323', 'Q423', 'Q124', 'Q224', 'Q324', 'Q424') # try adding 2022
start.date <- as.Date('2022-01-01')
ovrd_p <- c("RELEASE_STAGE_OVERRIDE"="P")
ovrd_f <- c("RELEASE_STAGE_OVERRIDE"="F")

# Out-turns
out <- c("UKGRABIQ Index")

# Create combinations
comb     <- expand.grid(fcast.series, fcast.dates)
fcast.codes <- apply(comb, 1, function(x) paste(x[1], " ", x[2], " Index", sep = ""))

# Download from BBG
fcast.data <- bdh(fcast.codes, c("PX_LAST"), start.date=start.date)
fcast.data[[1]]
fcast <- fcast.data[names(fcast.data)==fcast.codes[1]][[1]]
for (i in 2:length(fcast.data)) {
  fcast <- merge(fcast,fcast.data[names(fcast.data)==fcast.codes[i]][[1]], by="date", all=T)
}
colnames(fcast) <- c('date', 'Q1:22', 'Q2:22', 'Q3:22', 'Q4:22', 'Q1:23', 'Q2:23', 'Q3:23', 'Q4:23', 'Q1:24', 'Q2:24', 'Q3:24', 'Q4:24')
fcast

# BBG Download Out-turns
outturns_p <- bdh(out, c("ECO_RELEASE_DT", "ACTUAL_RELEASE"), start.date = start.date, overrides = ovrd_p)
outturns_f <- bdh(out, c("ECO_RELEASE_DT", "ACTUAL_RELEASE"), start.date = start.date, overrides = ovrd_f)

# Assign quarters
outturns_p$period <- format(as.yearqtr(outturns_p$date), format = "Q%q:%y")
outturns_f$period <- format(as.yearqtr(outturns_f$date), format = "Q%q:%y")

# Filter forecasts
for (col in 2:dim(fcast)[2]) {
  if (sum(outturns_p$period==colnames(fcast)[col]) > 0) {
    period_end <- outturns_p[outturns_p$period==colnames(fcast)[col],]$ECO_RELEASE_DT
    period_start <- lubridate::floor_date(outturns_p[outturns_p$period==colnames(fcast)[col],]$date, unit = "quarter")
    fcast[!((fcast[,c(1,col)]$date >= period_start) & (fcast[,c(1,col)]$date <= period_end)),col] <- NA
  } else {
    period_start <- lubridate::floor_date(Sys.Date(), unit = "quarter")
    fcast[!(fcast[,c(1,col)]$date >= period_start),col] <- NA
  }
}

# Drop rows where no outturns yet
outturns_p <- outturns_p %>%
  filter(date >= as.Date("2024-01-01") | rowSums(is.na(select(., everything()))) == 0)
outturns_f <- outturns_f %>%
  filter(date >= as.Date("2024-01-01") | rowSums(is.na(select(., everything()))) == 0)

# Shift lines to make chart readable
fcast_jitter <- fcast[,-dim(fcast)[2]]
for (i in 2:(dim(fcast_jitter)[2]-1)) {
  bool <- fcast_jitter[,i] == fcast_jitter[,i+1]
  bool[is.na(bool)] <- FALSE
  fcast_jitter[bool,i] <- fcast_jitter[bool,i] - 0.013
  fcast_jitter[bool,i+1] <- fcast_jitter[bool,i+1] + 0.013
}

tail(fcast_jitter)

# plot
fcast_trans <- melt(fcast_jitter, id = "date") 
track.cons.plot <- ggplot() + 
  geom_line(fcast_trans, mapping=aes(x=date, y=value, color=variable), size = 3) +
  geom_point(data=outturns_p, aes(x=ECO_RELEASE_DT, y=ACTUAL_RELEASE, color=period), size=5, shape=4, stroke = 3) +
  geom_point(data=outturns_f, aes(x=ECO_RELEASE_DT, y=ACTUAL_RELEASE, color=period), size=5, shape=16, stroke = 3) +
  geom_hline(yintercept = 0.0, lty=4) +
  scale_color_paletteer_d("ggthemes::calc") +
  scale_x_date(breaks = "3 months",
               date_labels=("%Y-%m"),
               limits = as.Date(c("2022-01-01", Sys.Date()))) +
  guides(color = "none") +
  labs(title = "UK GDP Growth (%qoq) - Evolution of Consensus Forecasts", 
       subtitle = "Consensus forecasts, preliminary and final prints",
       y = "%qoq GDP growth",
       caption = "Source: Bloomberg",
       color = NULL)

