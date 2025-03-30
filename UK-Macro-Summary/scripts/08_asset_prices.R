# Plot GBP Asset Prices


blpConnect()
# SETTINGS
date.start.bbg       <- '2020-01-01'

tickers <- c('BPSWS10 BGN Curncy', 'ASX Index', 'SPX Index', 'GBPUSD Curncy', 'USGG10YR Index', 'VIX Index', 'GTGBP10YR Corp')
labels <- c('UK_10y', 'UK_Equity', 'US_Equity', 'GBPUSD', 'US_10y', 'VIX', 'Gilt_10y')


df     <- bdh(tickers, "PX_LAST", start.date = as.Date(date.start.bbg))
bbg.raw<- data.table::rbindlist(df, idcol=T, use.names=FALSE)   # row binding resulting list 
bbg.df <- data.table::dcast(bbg.raw, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  janitor::clean_names() 
# Tidy
df <- as.data.frame(bbg.df)  
df.wide <- df[complete.cases(df), ] # No NA's

# Rolling correlations
df <- df.wide |> 
  mutate(yieldgap10 = gtgbp10yr_corp - usgg10yr_index) |> 
  mutate(corr_weekly = zoo::rollapply( # weekly Correl GBPUSD
    data = cbind(gbpusd_curncy, yieldgap10),
    width = 10,
    FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    fill = NA, align = "right"
    ),
    corr_monthly = zoo::rollapply( # monthly Correl GBPUSD
    data = cbind(gbpusd_curncy, yieldgap10),
    width = 22,
    FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    fill = NA, align = "right"
    ) )

# Plot GBPUSD Correlations, weekly and monthly 
dat.correl <- df |> 
  dplyr::select(c(contains("corr"), date)) |> 
  pivot_longer(!date, names_to = "variable", values_to = "value")

correl.plot <- ggplot(subset(dat.correl, date>="2022-06-01"),
                      aes(x=date, y=value, color=factor(variable))) +
  geom_line(linewidth=1.75) +
  geom_vline(xintercept = as.Date("2022-09-23"), lty=4, size=1.75) + # mini_Budget
  geom_vline(xintercept = as.Date("2024-10-30"), lty=4, size=1.75) + # Reeves Budget
  geom_vline(xintercept = as.Date("2025-01-07"), lty=4, size=1.75) + # January volatility
  geom_vline(xintercept = as.Date("2025-03-26"), lty=4, size=1.75) + # Fiscal Statement 
  annotate("text", x = as.Date("2022-09-23"), y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
           label = "mini-Budget", hjust = -0.1, vjust = 0, angle = 0, size = 13, fontface = "bold") +
  annotate("text", x = as.Date("2024-10-30"), y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
           label = "Reeves \nBudget", hjust = 1.1, vjust = 0, angle = 0, size = 13, fontface = "bold") +
  annotate("text", x = as.Date("2025-01-07"), y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
           label = "Jan. \n vol.", hjust = 0.0, vjust = 0, angle = 0, size = 13, fontface = "bold") +
  annotate("text", x = as.Date("2025-03-26"), y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
           label = "March \n FiscStat.", hjust = 0.0, vjust = 0, angle = 0, size = 13, fontface = "bold") +
  scale_color_manual(
    values = c("red", "dodgerblue"),  # Custom colors for each series
    labels = c("1-month (rolling)", "2-week (rolling)")  # Custom legend labels
  ) +
  geom_hline(yintercept = 0.0, lty=4) + 
  labs(y="correlation",
       title = "Correlation of GBPUSD and GB/US 10y Yield Gap",
       caption = "Source: Bloomberg",
       color=NULL) +
  theme(legend.position = "right")
correl.plot

df <- df |> 
  select(-c(contains("corr"), yieldgap10, gtgbp10yr_corp)) |> 
  pivot_longer(!date,names_to = "variable", values_to = "value")

# Plot
assets.plot <- ggplot(subset(df, date>='2022-06-01'), aes(x=date, y=value, color=factor(variable))) + 
  geom_line() + 
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y") 

