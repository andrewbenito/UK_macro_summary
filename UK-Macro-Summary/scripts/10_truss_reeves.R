# Plot GBP Asset Prices ; 5 days around Trss mini-Budget and Reeves


# SETTINGS
date.start.truss     <- '2022-09-22' # 2022-09-23
date.start.reeves    <- '2025-01-06' # 2024-10-30

tickers <- c('BPSWS2 BGN Curncy', 'GTGBP10YR Corp', # 'GTGBP30YR Corp',
             'GBPUSD Curncy' ,  'UKGGBE05 Index'
             )
labels <-  c('2y swap', '10y Gilt', #'30y Gilt', 
             'GBPUSD', '5y breakeven Infl')

df.truss  <- bdh(tickers, "PX_LAST", start.date = as.Date(date.start.truss), end.date = as.Date(date.start.truss) + 15) # 15day window
df.reeves <- bdh(tickers, "PX_LAST", start.date = as.Date(date.start.reeves), end.date = as.Date(date.start.reeves) + 15)
bbg.raw.truss   <- data.table::rbindlist(df.truss, idcol=T, use.names=FALSE)   # row binding resulting list 
df.truss <- data.table::dcast(bbg.raw.truss, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  as.data.frame() |> 
  janitor::clean_names() 
bbg.raw.reeves   <- data.table::rbindlist(df.reeves, idcol=T, use.names=FALSE)   # row binding resulting list 
df.reeves <- data.table::dcast(bbg.raw.reeves, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  as.data.frame() |> 
  janitor::clean_names()

df <- list(truss = df.truss,
           reeves=df.reeves)

# Tidy
df <- lapply(df, function(df) {
  pivot_longer(df, 
               cols = -date,
               names_to = "variable", 
               values_to = "value")
}
  )
  
# Plot
a.plot <- ggplot(
  df$truss, aes(x=date, y=value, color=factor(variable))) + 
  geom_line(linewidth=1.5) + 
  geom_point(size=1.5) +
  geom_vline(xintercept = as.Date('2022-09-23'), lty=4, size=2, color='red') +
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Truss, mini-Budget (2022.09.23)")

b.plot <- ggplot(
  df$reeves, aes(x=date, y=value, color=factor(variable))) + 
  geom_line(linewidth=1.5) + 
  geom_point(size=1.5) +
  geom_vline(xintercept = as.Date('2025-01-07'), lty=4, size=2, color = 'red') +
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y") + 
  labs(title = "January episode, (2025.01.07)")

a.plot / b.plot

