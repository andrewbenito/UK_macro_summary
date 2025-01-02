# Plot GBP Asset Prices


blpConnect()
# SETTINGS
date.start.bbg       <- '2020-01-01'

tickers <- c('BPSWS10 BGN Curncy', 'ASX Index', 'SPX Index', 'GBPUSD Curncy', 'USGG10YR Index', 'VIX Index')
labels <- c('UK_10y', 'UK_Equity', 'US_Equity', 'GBPUSD', 'US_10y', 'VIX')


df     <- bdh(tickers, "PX_LAST", start.date = as.Date(date.start.bbg))
bbg.raw<- data.table::rbindlist(df, idcol=T, use.names=FALSE)   # row binding resulting list 
bbg.df <- data.table::dcast(bbg.raw, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  janitor::clean_names() 
# Tidy
df <- as.data.frame(bbg.df)  
df <- df[complete.cases(df), ] # No NA's
df <- pivot_longer(df, !date,names_to = "variable", values_to = "value")

# Plot
assets.plot <- ggplot(subset(df, date>='2024-01-01'), aes(x=date, y=value, color=factor(variable))) + 
  geom_line() + 
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y") 


