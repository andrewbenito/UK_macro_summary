# Reactions at MPC days
# Dovish MPC Comms becoming less credible?

# SETTINGS
blpConnect()
date.start.bbg       <- '1997-01-01'

# DATA
tickers <- c('BPSWS1 Curncy', 'BPSWS2 Curncy', 'GTGBP5YR Corp', 'GTGBP10YR Corp', 'GTGBP20YR Corp', 'GTGBP30YR Corp',    
             'UKGGBE05 Index', 'FWISBP55 Index', 'BPSWIT5 BGN Curncy')

# CREATE DATA
df     <- bdh(tickers, "PX_LAST", start.date = as.Date(date.start.bbg))
bbg.raw<- data.table::rbindlist(df, idcol=T, use.names=FALSE)   # row binding resulting list 
bbg.df <- data.table::dcast(bbg.raw, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  janitor::clean_names() 
# Tidy
df <- as.data.frame(bbg.df)  
df.wide <- df[complete.cases(df), ] # No NA's

# Transform Data to Changes
df.wide <- df.wide |> arrange(date) |> 
  mutate(across(where(is.numeric), ~ . - lag(.)))

# MPC dates: from Mianda-Agrapino
dates <- read_excel(here::here('inputs', 'measuring-monetary-policy-in-the-uk-the-ukmpesd.xlsx'),
                            sheet = "Surprises") |>
  mutate(date = as.Date(Datetime)) |> filter(isMPC==TRUE) |> pull(date)

# Add latest MPC dates
#======================
extra.dates <- as.Date(c("2024-02-01", "2024-03-21", "2024-05-09", "2024-06-20", "2024-08-01", "2024-09-19", 
                 "2024-11-07", "2024-12-19", "2025-02-06", "2025-03-20", "2025-05-08", "2025-06-19",
                 "2025-08-07", "2025-09-18", "2025-11-06", "2025-12-18", "2026-02-05"))
dates <- c(dates, extra.dates)

#===========================
# Filter DATA to MPC DATES
#===========================
mpc <- df.wide |> 
  dplyr::filter(date %in% dates) |> 
  dplyr::filter(bpsws1_curncy>-0.4)

mpc <-mpc |> 
  mutate(dategrp5 = case_when(date>=as.Date("2005-01-01") & date<as.Date("2010-01-01") ~ "2005-09",
                              date>=as.Date("2010-01-01") & date<as.Date("2015-01-01") ~ "2010-14",
                              date>=as.Date("2015-01-01") & date<as.Date("2020-01-01") ~ "2015-19",
                              date>=as.Date("2020-01-01")  ~ "2020-"   ),
         preminibudget = case_when(date< as.Date('2022-09-23') ~ "pre mini-Budget",
                                   date>=as.Date('2022-09-23') ~ "post mini-Budget"))

# 1: Increased Market volatility around Announcements? [since mini-Budget]

mpcsummary <- mpc |>  
  dplyr::select(date, dategrp5, bpsws1_curncy, gtgbp10yr_corp, fwisbp55_index) |> 
  group_by(dategrp5) |>  
  summarise(
    mean_OIS1y   = mean(bpsws1_curncy, na.rm=TRUE),
    sd_OIS1y     = sd(bpsws1_curncy, na.rm=TRUE),
    mean_10y     = mean(gtgbp10yr_corp, na.rm=TRUE),
    sd_10y       = sd(gtgbp10yr_corp, na.rm=TRUE),
    mean_infl5y5 = mean(fwisbp55_index, na.rm=TRUE),
    sd_infl5y5   = sd(fwisbp55_index, na.rm=TRUE)
    )
mpcsummary

# 2: Dovish Front-end reactions less anchoring (in Gilts and BE's) [since mini-Budget]
# 10y Gilt yields
mpc.10y <- ggplot(mpc, aes(x = bpsws1_curncy, y = gtgbp10yr_corp)) + 
  geom_point(aes(color = ifelse(date >= as.Date("2022-09-23"), "post mini-Budget", "pre mini-Budget"),
                 size = ifelse(date >= as.Date("2022-09-23"), 2.5, 1.5))) + 
  geom_text_repel(data = subset(mpc, date >= as.Date("2022-09-23") & bpsws1_curncy < 0 & gtgbp10yr_corp > 0), 
                  aes(label = format(date, "%Y-%m-%d")), 
                  size = 6, color = "blue", max.overlaps = 15) +
  geom_smooth(method = "rlm", method.args = list(method = "M"), se = TRUE) + 
  geom_vline(xintercept = 0.0, lty = 4) + 
  geom_hline(yintercept = 0.0, lty = 4) + 
  xlim(-0.2,0.2) + ylim(-0.2,0.2) +
  labs(title = "10y Gilt Yield reaction",
       subtitle = "market reactions on MPC days",
       x = "Ch in 1y OIS", y = "Ch in 10y Gilt yield",
       caption = "Source: Bloomberg") +
  scale_color_manual(values = c("post mini-Budget" = "red", "pre mini-Budget" = "black"),
                     name = "") +
  guides(size = "none")  
#mpc.10y

# 20y Gilt yields
mpc.20y <- ggplot(mpc, aes(x = bpsws1_curncy, y = gtgbp20yr_corp)) + 
  geom_point(aes(color = ifelse(date >= as.Date("2022-09-23"), "post mini-Budget", "pre mini-Budget"),
                 size = ifelse(date >= as.Date("2022-09-23"), 2.5, 1.5))) + 
  geom_text_repel(data = subset(mpc, date >= as.Date("2022-09-23") & bpsws1_curncy < 0 & gtgbp20yr_corp > 0), 
                  aes(label = format(date, "%Y-%m-%d")), 
                  size = 6, color = "blue", max.overlaps = 15) +
  geom_smooth(method = "rlm", method.args = list(method = "M"), se = TRUE) + 
  geom_vline(xintercept = 0.0, lty = 4) + 
  geom_hline(yintercept = 0.0, lty = 4) + 
  xlim(-0.2,0.2) + ylim(-0.2,0.2) +
  labs(title = "20y Gilt Yield reaction",
       subtitle = "market reactions on MPC decision days",
       x = "Ch in 1y OIS (pp)", y = "Ch in 20y Gilt yield (pp)",
       caption = "Source: Bloomberg") +
  scale_color_manual(values = c("post mini-Budget" = "red", "pre mini-Budget" = "black"),
                     name = "") +
  guides(size = "none")  
#mpc.20y


#30y
mpc.30y <- ggplot(mpc, aes(x = bpsws1_curncy, y = gtgbp30yr_corp)) + 
  geom_point(aes(color = ifelse(date >= as.Date("2022-09-23"), "post mini-Budget", "pre mini-Budget"),
                 size = ifelse(date >= as.Date("2022-09-23"), 2.5, 1.5))) + 
  geom_text_repel(data = subset(mpc, date >= as.Date("2022-09-23") & bpsws1_curncy < 0 & gtgbp30yr_corp > 0), 
                  aes(label = format(date, "%Y-%m-%d")), 
                  size = 6, color = "blue", max.overlaps = 15) +
  geom_smooth(method = "rlm", method.args = list(method = "M"), se = TRUE) + 
  geom_vline(xintercept = 0.0, lty = 4) + 
  geom_hline(yintercept = 0.0, lty = 4) + 
  xlim(-0.2,0.2) + ylim(-0.2,0.2) +
  labs(title = "30y Gilt Yield reaction",
       subtitle = "market reactions on MPC days",
       x = "Ch in 1y OIS", y = "Ch in 30y Gilt yield",
       caption = "Source: Bloomberg") +
  scale_color_manual(values = c("post mini-Budget" = "red", "pre mini-Budget" = "black"),
                     name = "") +
  guides(size = "none")  
#mpc.30y

#5y Infl swap
mpc.infl5 <- ggplot(mpc, aes(x = bpsws1_curncy, y = bpswit5_bgn_curncy)) + 
  geom_point(aes(color = ifelse(date >= as.Date("2022-09-23"), "post mini-Budget", "pre mini-Budget"),
                 size = ifelse(date >= as.Date("2022-09-23"), 2.25, 2))) + 
  geom_text_repel(data = subset(mpc, date >= as.Date("2022-09-23") & bpsws1_curncy < 0 & bpswit5_bgn_curncy > 0), 
                  aes(label = format(date, "%Y-%m-%d")), 
                  size = 6, color = "blue", max.overlaps = 15) +
  #  geom_smooth(method = "rlm", method.args = list(method = "M"), se = TRUE) + 
  geom_vline(xintercept = 0.0, lty = 4) + 
  geom_hline(yintercept = 0.0, lty = 4) + 
  xlim(-0.15,0.2) + ylim(-0.2,0.1) +
  labs(title = "5y Infl swap reaction",
       subtitle = "market reactions on MPC decision days",
       x = "Ch in 1y OIS (pp)", y = "Ch in 5y inflation (pp)",
       caption = "Source: Bloomberg") +
  scale_color_manual(values = c("post mini-Budget" = "red", "pre mini-Budget" = "black"),
                     name = "") +
  guides(size = "none")  
mpc.infl5




