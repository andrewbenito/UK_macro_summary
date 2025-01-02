# GBP OIS Forward Curves and How they have Evolved
# Download BoE data from url, Tidy and Plot

# Packages ----
lapply(c('here', 'tidyverse', 'readxl', 'lubridate', 'xts', 'MacrobondAPI', 'showtext'),
       require, character.only = TRUE)
here::here()
source(here('functions','functions.R'))


# Data: Download and Tidy  ----
url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/oisddata.zip"
td <- tempdir() 
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf)
fname1 <- unzip(tf, list=TRUE)$Name[1]
fname2 <- unzip(tf, list=TRUE)$Name[2]
df1 <- read_xlsx(unzip(tf, files = fname1, exdir = td),
                 sheet = "1. fwd curve") 
df2 <- read_xlsx(unzip(tf, files = fname2, exdir = td),
                 sheet = "1. fwds, short end") 

# Latest OIS data ----
url3 <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
tf3  <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url3, tf3, mode = "wb")
fname3 <- unzip(tf3, list = TRUE)$Name[4]
df3    <- read_xlsx(unzip(tf3, files = fname3, exdir = td),
                    sheet = "1. fwds, short end")

# Tidy Historic Forward Curve data ----
cleanOIS <- function(df) {
  # Round the months row (excl date column)
  df[2, -1] <- round(df[2, -1], digits = 0)
  
  df <- df %>%
    set_names(slice(., 2)) %>%
    tail(-5) %>%
    type.convert(as.is = TRUE) %>%
    rename(date = 1) %>%
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% # Convert Excel dates
    drop_na() %>%
    column_to_rownames(var = "date")
  
  return(df)
}

# Clean + bind the 3 downloaded dataframes
for (dfn in c("df1", "df2", "df3")) {
  assign(dfn, cleanOIS(get(dfn)))
}
df <- bind_rows(df1, df2, df3) # Daily OIS data for inst fwds 1-60m

# Convert Daily Data to Monthly; pivot----
dfxts <- as.xts(df) 
df_m  <- as.data.frame(apply.monthly(dfxts, mean)) 
df_m$date = as_date(rownames(df_m))
# Dates from maturities
fwcv <- pivot_longer(df_m, !date, 
                     names_to = "tau", values_to = "yield") %>%
  mutate(month = month(date),
         tau   = as.numeric(tau),
         date2 = ymd(as.Date(date) %m+% months(tau)-1),
         date2 = ceiling_date(date2, unit='month') - days(1))

# Merge Bank Rate [uses MacroBond API]
bankr <- build.mb.dataset('gbrate0001', 'bankrate') |> as.data.frame() |> 
  rownames_to_column(var = "date") |>  
  mutate(date = as.Date(date),
         date2 = ceiling_date(date, unit='month') - days(1)) |> 
  filter(date>=as.Date('1997-01-01'))

fwcv <- left_join(fwcv, bankr, by='date2') |> 
  select(-date.y) |> rename(date = date.x)

# Figure: Evolving Forwards----
ois1 <- ggplot(fwcv, aes(x=date2, y = yield, group = date)) +
  geom_line(aes(colour = as.factor(date))) + 
  geom_line(aes(y=bankrate)) +
  geom_hline(yintercept = 0.0, lty=4) +
  theme(legend.position = "none") + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(title = "Bank Rate and GBP OIS Curves", 
       subtitle = "monthly averages of end-of-day daily data",
       x = "date", y = "rate %",
       caption = "Source: Bank of England data")
ggsave(here::here('plots', '1.GBP-OIS.png'), plot = ois1, width = 9, height = 5, dpi = 90, units = "in", device='png')

# Recent data
comp.date <- last(fwcv$date) %m-% months(12)

ois2 <- ggplot(subset(fwcv, date>=as.Date(comp.date)), aes(x=date2, y = yield, group = date)) +
  geom_line(aes(colour = as.factor(date)), linewidth=1.4) + 
  geom_line(aes(y=bankrate), linewidth=1.25) +
  theme(legend.position = "none") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "Bank Rate and GBP OIS Curves: The past 12 months", 
       subtitle = "monthly averages of end-of-day daily data",
       x = "date", y = "rate %",
       caption = "Source: Bank of England data")
ggsave(here::here('plots', '2.GBP-OIS_12m.png'), plot = ois2, width = 9, height = 5, dpi = 90, units = "in", device='png')




