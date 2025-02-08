# Analysis of BoE Forecast Errors

library(glue)
library(rdbnomics)
library(ggfan)

# INPUT: Most recent MPR ----
year  <- 2024
month <- 'november'
startYear <- 2000

finalReport <- year + case_when(month == 'february' ~ 0.00,
                                month == 'may' ~ 0.25,
                                month == 'august' ~ 0.50,
                                month == 'november' ~ 0.75) 

# Get Data ---- 
fileno <- c(5,7,8,9) # Based on list in zip folder
sheetname <- c('CPI Forecast', 'GDP Forecast', 
               'Unemployment Forecast', 'GDP')

url = glue("https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/{year}/{month}/mpr-{month}-{year}-chart-slides-and-data.zip")

# Download
td <- tempdir() 
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf, mode = 'wb')

# Extract and Read Files into Data Frames ----
dataframes <- map2(fileno, sheetname, ~ {
  fname <- unzip(tf, list = TRUE)$Name[.x]
  message("Processing: ", fname)
  read_xlsx(unzip(tf, files = fname, exdir = td), sheet = .y)
})

# Assign Data Frames to Variables ----
names(dataframes) <- paste0("df", seq_along(dataframes))
list2env(dataframes, envir = .GlobalEnv) 

# Tidy----
# Inflation [df1]
df1 <- df1 %>%
  set_names(dplyr::slice(.,4)) %>% 
  tail(-4) %>%
  type.convert()
colnames(df1)[1:4] <- c('reportDateY', 'reportDateM', 'conditioning', 'summary')
df1 <- df1 %>%
  mutate(reportNext = ifelse(summary=="Market Mode", 1, 0),
         reportID = cumsum(reportNext),
         rateAssump = case_when(
           str_sub(summary, 1, 6) == "Market" ~ "market",
           str_sub(summary, 1, 6) != "Market" ~ "constant" ),
         mm = str_sub(reportDateM, 1, 3),
         reportmm = match(mm, month.abb),
         summary = recode(summary, `Market Mode` = "Mode",
                          `Market Mean` = "Mean", `Market Median` = "Median" )) %>%
  dplyr::group_by(reportID) %>%
  fill(mm, reportmm, reportDateY, conditioning, .direction = "downup") %>%
  dplyr::ungroup() %>%
  fill(reportDateY) %>%
  filter(reportDateY>=2005) %>%
  mutate(reportDate = lubridate::ymd( paste(reportDateY, reportmm, "1", sep = "-")),
         reportyq = ifelse(month(reportDate)==1, year(reportDate),
                           year(reportDate) + (month(reportDate)-2)/12)) %>%
  select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  select(where( ~!all(is.na(.x))) ) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date") %>%
  pivot_wider(names_from = summary) %>%
  mutate(dateY = as.numeric(str_sub(date,1,4)),
         dateQ = as.numeric(str_sub(date,-1)),
         dateyq = dateY + (dateQ-1)/4) %>%
  group_by(reportID, dateyq) %>%
  fill(Uncertainty, Skew, .direction = "downup") %>%
  filter(rateAssump == "market" & !is.na(Mode))

# CPI inflation, back-data 
startYear <- 2000
cpi <- rdb(ids = "ONS/MM23/D7G7.Q") %>%
  select(period, value, series_code) %>%
  filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         Mode = value, Median = value, Mean = value) 
# append CPI with forecast CPI, for each Report; 
data1 <- bind_rows(cpi, df1) 

# Tidy Unemployment Rate forecasts [only avbl from 2014]
df3 <- df3 %>%
  set_names(dplyr::slice(.,4)) %>% 
  tail(-4) %>%
  type.convert() 
colnames(df3)[1:4] <- c('reportDateY', 'reportDateM', 'conditioning', 'summary')
df3 <- df3 %>%
  dplyr::filter(substr(summary,1,4) != "Prob") %>%
  type.convert() 
df3 <- df3 %>%
  mutate(reportNext = ifelse(summary=="Market Mode", 1, 0),
         reportID = cumsum(reportNext),
         rateAssump = case_when(
           str_sub(summary, 1, 6) == "Market" ~ "market",
           str_sub(summary, 1, 6) != "Market" ~ "constant" ),
         mm = str_sub(reportDateM, 1, 3),
         reportmm = match(mm, month.abb),
         summary = recode(summary, `Market Mode` = "Mode",
                          `Market Mean` = "Mean", `Market Median` = "Median" )) %>%
  dplyr::group_by(reportID) %>%
  fill(mm, reportmm, reportDateY, conditioning, .direction = "downup") %>%
  dplyr::ungroup() %>%
  fill(reportDateY) %>%
  filter(reportDateY>=2014) %>%
  select(!c('2013Q3','2013Q4')) %>%
  mutate(reportDate = lubridate::ymd( paste(reportDateY, reportmm, "1", sep = "-")),
         reportyq = ifelse(month(reportDate)==1, year(reportDate),
                           year(reportDate) + (month(reportDate)-2)/12)) %>%
  select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  select(where( ~!all(is.na(.x))) ) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date") %>%
  pivot_wider(names_from = summary) %>%
  mutate(dateY = as.numeric(str_sub(date,1,4)),
         dateQ = as.numeric(str_sub(date,-1)),
         dateyq = dateY + (dateQ-1)/4) %>%
  group_by(reportID, dateyq) %>%
  fill('Standard deviation', 'Skewness', .direction = "downup") %>%
  filter(rateAssump == "market" & !is.na(Mode))

# Unemployment rate back-data [ Forecasts from 2014]
startYear <- 2000
unemp <- rdb(ids = "ONS/LMS/MGSX.Q") %>%
  select(period, value, series_code) %>%
  filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         Mode = value, Median = value, Mean = value) 
# append back-data with forecast Unemployment rate, for each Report; 
data3 <- bind_rows(unemp, df3) 

# Tidy GDP Growth data
df2 <- df2 %>%
  set_names(dplyr::slice(.,4)) %>% 
  tail(-4) %>%
  type.convert()
colnames(df2)[1:4] <- c('reportDateY', 'reportDateM', 'conditioning', 'summary')
df2 <- df2 %>%
  mutate(reportNext = ifelse(summary=="Market Mode", 1, 0),
         reportID = cumsum(reportNext),
         rateAssump = case_when(
           str_sub(summary, 1, 6) == "Market" ~ "market",
           str_sub(summary, 1, 6) != "Market" ~ "constant" ),
         mm = str_sub(reportDateM, 1, 3),
         reportmm = match(mm, month.abb),
         summary = recode(summary, `Market Mode` = "Mode",
                          `Market Mean` = "Mean", `Market Median` = "Median" )) %>%
  dplyr::group_by(reportID) %>%
  fill(mm, reportmm, reportDateY, conditioning, .direction = "downup") %>%
  dplyr::ungroup() %>%
  fill(reportDateY) %>%
  filter(reportDateY>=2005) %>%
  mutate(reportDate = lubridate::ymd( paste(reportDateY, reportmm, "1", sep = "-")),
         reportyq = ifelse(month(reportDate)==1, year(reportDate),
                           year(reportDate) + (month(reportDate)-2)/12)) %>%
  select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  select(where( ~!all(is.na(.x))) ) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date") %>%
  pivot_wider(names_from = summary) %>%
  mutate(dateY = as.numeric(str_sub(date,1,4)),
         dateQ = as.numeric(str_sub(date,-1)),
         dateyq = dateY + (dateQ-1)/4) %>%
  group_by(reportID, dateyq) %>%
  fill(Uncertainty, Skew, .direction = "downup") %>%
  filter(rateAssump == "market" & !is.na(Mode)) %>%
  filter(reportyq<2020.5 | reportyq> 2021.25)

# GDP back-data 
startYear <- 2000
gdp <- rdb(ids = "ONS/UKEA/YBEZ.Q") %>%
  select(period, value, series_code) %>%
  filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         value = 100*(value/lag(value,4)-1),
         Mode = value, Median = value, Mean = value) 
# append back-data with forecast annual GDP growth, for each Report; 
data2 <- bind_rows(gdp, df2) 

# merge Actual Data U and Pi
cpiAdj <- cpi %>%
  select(!c(Median, Mode, value, series_code)) %>%
  rename(cpi = Mean)
unempAdj <- unemp %>%
  select(!c(Median, Mode, value, series_code)) %>%
  rename(unemp = Mean)
data4 <- left_join(cpiAdj, unempAdj) %>%
  mutate(year = year(period))

# Simulate---- CPI
time <- 1:13
N_sims <- 10000
dateyq <- subset(df1$dateyq, df1$reportyq==finalReport)
mu <- subset(df1$Mean, df1$reportyq==finalReport) 
sigma <- subset(df1$Uncertainty, df1$reportyq==finalReport) 

simul_data <- sapply(time, function(i) rnorm(N_sims, mu[i], sigma[i]))
# gather into long-form 
simul_df <- data.frame(x=time, t(simul_data)) %>% gather(key=Sim, value=y, -x)
#head(simul_df)

# Append historic data to CPI fan chart----
dataFanChart <- bind_rows(cpi, simul_df) %>%
  mutate(dateJoin = ifelse(!is.na(dateyq), dateyq, finalReport + (x-1)/4))

p1 <- ggplot(dataFanChart) +
  geom_line(aes(x = dateJoin, y = value), color = 'black') +
  geom_fan(aes(x = dateJoin, y = y), intervals = c(30,60,90)/100) +
  scale_fill_gradient(low = "red", high = "pink") + 
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme(legend.position = "none") +
  labs(x = 'Date', y = 'CPI Inflation (%yoy)') + 
  geom_hline(yintercept = 2.0, linetype="dotted") 

# Previous CPI Fan Chart
dateyq <- subset(df1$dateyq, df1$reportyq==(finalReport-0.25))
mu <- subset(df1$Mean, df1$reportyq==(finalReport-0.25)) 
sigma <- subset(df1$Uncertainty, df1$reportyq==(finalReport-0.25)) 

simul_data <- sapply(time, function(i) rnorm(N_sims, mu[i], sigma[i]))

# gather into long-form 
simul_df <- data.frame(x=time, t(simul_data)) %>% gather(key=Sim, value=y, -x)
#head(simul_df)

# Append historic data to simulated CPI fan chart----
dataFanChartPrevious <- bind_rows(cpi, simul_df) %>%
  mutate(dateJoin = ifelse(!is.na(dateyq), dateyq, finalReport + (x-2)/4))

p2 <- ggplot(dataFanChartPrevious) +
  geom_line(aes(x = dateJoin, y = value), color = 'black') +
  geom_fan(aes(x = dateJoin, y = y), intervals = c(30,60,90)/100) + 
  scale_fill_gradient(low = "red", high = "pink") + 
  theme(legend.position = "none") +
  labs(x = 'Date', y = 'CPI Inflation (%yoy)') + 
  geom_hline(yintercept = 2.0, linetype="dotted") 
#grid.arrange(p1, p2, ncol=2)

# Macro news: scale, persistence and source of news justifies policy decisions

# FIG: Forecast News INFL
# Plot Successive forecasts
infl.forecasts <- ggplot(data1, aes(x=dateyq, y=`Mean`, group=reportDate, color = reportDate)) + 
  geom_line() + 
  geom_hline(yintercept = 2.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "CPI Inflation",
       x="Date", y="BoE CPI Inflation forecast, %yoy")

# Latest set of Inflation Forecasts (latest 5 Reports)
infl.forecasts.latest <- ggplot(subset(data1, reportDate>=last(reportDate) %m-% months(5*3))) + 
  geom_line(aes(x=dateyq, y=`Mean`, group=reportDate, color = reportDate)) +
  geom_point(
    data = subset(data1, reportDate == last(reportDate)),  # Filter points for latest reportDate
    aes(x = dateyq, y = `Mean`),
    color = "blue",
    size = 3) +
  geom_hline(yintercept = 2.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "CPI Inflation",
       x="Date", y="BoE CPI Inflation forecast, %yoy")


# FIG: Forecast News UNEMPLOYMENT
unemp.forecasts <- ggplot(data3, aes(x=dateyq, y=Mean, group=reportDate, color = reportDate)) + 
  geom_line() + 
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "Unemployment rate",
       x="Date", y="BoE Unemployment rate forecast, %")

# FIG: Forecast News GDP Growth
# Plot: GDP
gdp.forecasts <- ggplot(data2, aes(x=dateyq, y=Mean, group=reportDate, color = reportDate)) + 
  geom_line() + 
  geom_hline(yintercept = 0.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "GDP growth",
       x="Date", y="BoE GDP growth forecasts %yoy")
gdp.forecasts

# INFL: Actual and Predicted
cpiT <- cpi %>%
  select(dateyq, value) 
CPIactualPredicted <- left_join(df1, cpiT) %>%
  rename(cpi = value) %>%
  mutate(h = dateyq - reportyq,
         FE = cpi - Mean) %>%
  select(!c('Uncertainty', 'Skew'))
colnames(CPIactualPredicted)[7:9] <- paste("CPI_", colnames(CPIactualPredicted[,c(7:9)]), sep = "") 

# UNEMPLOYMENT Actual and Predicted 
unempT <- unemp %>%
  select(dateyq, value) 
UnempactualPredicted <- left_join(df3, unempT) %>%
  rename(unemp = value) %>%
  mutate(h = dateyq - reportyq,
         FE = unemp - Mean) %>%
  select(!c('Standard deviation', 'Skewness'))
colnames(UnempactualPredicted)[7:9] <- paste("Unemp_", colnames(UnempactualPredicted[,c(7:9)]), sep = "")

# GDP Actual and Predicted 
gdpT <- gdp %>%
  select(dateyq, value) 
gdpactualPredicted <- left_join(df2, gdpT) %>%
  rename(gdp = value) %>%
  mutate(h = dateyq - reportyq,
         FE = gdp - Mean) %>%
  select(!c('Uncertainty', 'Skew'))
colnames(gdpactualPredicted)[7:9] <- paste("gdp_", colnames(gdpactualPredicted[,c(7:9)]), sep = "")



# FORECAST ERRORS
#=================
# Histograms
# CPI
ggplot(CPIactualPredicted, aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# subset h>=1y
p3 <- ggplot(subset(CPIactualPredicted, h>=1&year>=2014), aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp") + 
  geom_vline(xintercept = 0.0, lty=4, color = 'black', size = 1.25) +
  labs(subtitle = "CPI Inflation")
            
# Histograms
# Unemployment
ggplot(UnempactualPredicted, aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp") + 
  labs(subtitle = "Unemployment rate")

# subset h>=1y
p4 <- ggplot(subset(UnempactualPredicted, h>=1), aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp") + 
  geom_vline(xintercept = 0.0, lty=4, color = 'black', size = 1.25) +
  labs(subtitle = "Unemployment rate")

# GDP growth
ggplot(gdpactualPredicted, aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Forecast error, pp") + 
  labs(subtitle = "GDP growth")

# subset h>=1y
p5 <- ggplot(subset(gdpactualPredicted, h>=1&year>=2014), aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp annual") + 
  geom_vline(xintercept = 0.0, lty=4, color = 'black', size = 1.25) +
  labs(subtitle = "GDP growth")

# p3 + p4 + p5 + plot_annotation(
#   caption = "Eisler Capital"
#   )

