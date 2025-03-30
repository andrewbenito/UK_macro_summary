# BoE Balance sheet, QT, Losses and All That

lapply(c('here','tidyverse','readxl', 'MacrobondAPI', 'gt', 'Rblpapi', 'ggsci',
         'ggrepel', 'xts', 'zoo'), 
       require, character.only = TRUE)
here::here()

# functions 
date.range       <- '20080101/'    # data start 

# BoE Policy Settings and
# Gilts Maturity Profile Data 
#----------------------------
# Policy Settings: Bank Rate and QE1-QE5
tickers <- c("gbrate0001", "gbgpfi1013")
labels  <- c("Bank Rate", "Gilt holdings")
dat <- build.mb.dataset(tickers, labels) %>% .[date.range] %>% as.data.frame() %>%
  janitor::clean_names() 

dat <- dat |> 
  mutate(gilt_holdings = gilt_holdings / 1e9, # Bn
         date = as.Date(row.names(dat)),
         qe_type = case_when(
           date >= as.Date("2009-03-01") & date < as.Date("2011-10-01")  ~ "QE1",
           date >= as.Date("2011-10-01") & date < as.Date("2012-07-01")  ~ "QE2",
           date >= as.Date("2012-07-01") & date < as.Date("2016-08-01")  ~ "QE3",
           date >= as.Date("2016-08-01") & date < as.Date("2020-03-01")  ~ "QE4",
           date >= as.Date("2020-03-01")   ~ "QE5" ))

# Plot Bank Rate and QE
coeff <- 100
p1 <- ggplot(dat, aes(x = date)) + 
  geom_col(aes(y = gilt_holdings / 100, fill = qe_type),) + 
  geom_line(aes(y = bank_rate), color = "red", size = 1.2) +
  scale_fill_manual(
    values = c("QE1" = "#0073C299", "QE2" = "#EFC000FF", "QE3" = "#86868699",
               "QE4" = "#CD534C99", "QE5" = "#7AA6DC99"),
    name = NULL
  ) +
  scale_y_continuous(
    name = "Bank Rate (%)",
    sec.axis = sec_axis(~ .*100, name = "Gilts (£bn)")
  ) + 
  labs(title = "BoE policy",
       subtitle = "Bank Rate and QE-related Gilt Holdings",
       caption = "Source: BoE") +
  theme(legend.position = "bottom")

#=======================================================
# BoE Orphanides-style Charts: Nominal and Real rates
#=======================================================
# blpConnect()
# 
# df <- bdh(c("BPSWS2 Curncy","UKGGBE02 Index", "BPSWIT2 Curncy", "BPSWIT3 Curncy",
#             "UKBRBASE Index"), 
#           "PX_LAST", start.date = as.Date("2020-01-01"))
# bbg   <- data.table::rbindlist(df, idcol=T, use.names=FALSE)   # row binding resulting list 
# bbg.wide <- data.table::dcast(bbg, date ~ .id, value.var="PX_LAST") |> 
#   janitor::clean_names() |> 
#   mutate(rreal_2yswap = bpsws2_curncy - bpswit2_curncy,
#          rreal_2ybe   = bpsws2_curncy - ukggbe02_index)
# bbg.long <- bbg.wide |> 
#   select(date, bpsws2_curncy, bpswit2_curncy, ukbrbase_index, rreal_2yswap) |> 
#   pivot_longer(!date, names_to = "category", values_to = "value")
# 
# # Plot2: Expected inflation and the real interest rate
# df.keep <- bbg.long |> 
#   filter(category %in% c("rreal_2yswap", "bpswit2_curncy", "ukbrbase_index"))  # drop realr
# 
# orph.plot <- ggplot(df.keep, aes(x=date, y = value, color = category)) + 
#   geom_line() + 
#   geom_hline(yintercept = 0.0, lty=4) +
#   labs(x = "Date", y = "%",
#        title = "Expected inflation and the real interest rate",
#        color = NULL,
#        caption = "Sources: Bloomberg, Eisler Capital") + 
#   scale_color_manual(values = c("ukbrbase_index" = "darkblue", "bpswit2_curncy" = "darkgreen", 
#                                 "rreal_2yswap" = "red"),
#                      labels = c("2y RPI Infl", "2y real rate", "BoE Bank Rate"  )) + 
#   theme(legend.position = "bottom")
# orph.plot
