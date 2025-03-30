# Plot GBP Asset Prices ; 15d before and 20d after Truss mini-Budget and January 2025


# SETTINGS
date.truss <- as.Date('2022-09-23')
date.jan   <- as.Date('2025-01-07')

tickers <- c('BPSWS1 BGN Curncy', 'BPSWS2 BGN Curncy',  'GTGBP30YR Corp', 'GBPUSD Curncy')
labels <-  c('1y OIS', '2y OIS', '10y Gilt', 'GBPUSD')

df.truss  <- bdh(tickers, "PX_LAST", start.date = as.Date(date.truss - days(15)), end.date = as.Date(date.truss) + days(20)) # +/-20day window
df.jan    <- bdh(tickers, "PX_LAST", start.date = as.Date(date.jan - days(15)),   end.date = as.Date(date.jan) + days(20))

# Truss dataframe
bbg.raw.truss   <- data.table::rbindlist(df.truss, idcol=T, use.names=FALSE)   # row binding resulting list 
df.truss <- data.table::dcast(bbg.raw.truss, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  as.data.frame() |> 
  janitor::clean_names() |> 
  mutate(across(-date, ~ na.approx(.x, na.rm=FALSE))) |>  # interpolate
  mutate(across(-c(date, gbpusd_curncy), ~ (. - first(.)) * 100) ) |>  # Normalize to deviations from first date
  mutate(across(c(gbpusd_curncy), ~ (. / first(.)-1) *100))

# January 2025 dataframe
bbg.raw.jan   <- data.table::rbindlist(df.jan, idcol=T, use.names=FALSE)   # row binding resulting list 
df.jan <- data.table::dcast(bbg.raw.jan, date ~ .id, value.var="PX_LAST") |> # Daily data, some NAs
  as.data.frame() |> 
  janitor::clean_names() |> 
  mutate(across(-date, ~ na.approx(.x, na.rm=FALSE))) |>  # interpolate
  mutate(across(-c(date, gbpusd_curncy),  ~ (. - first(.)) * 100) ) |> # normalise to deviations from first date
  mutate(across(c(gbpusd_curncy), ~ (. / first(.)-1) *100))


# list 
df <- list(truss = df.truss,
           jan   = df.jan)

# Tidy - Pivot-longer
#------------------------
df <- lapply(df, function(df) {
  pivot_longer(df, 
               cols = -date,
               names_to = "variable", 
               values_to = "value")
}
  )
  
# Plot
# Define a scaling factor for the secondary axis transformation
scale_factor <- 20

a.plot <- ggplot(df$truss, aes(x = date)) + 
  # Primary Y-axis (Interest Rate Changes)
  geom_line(data = df$truss %>% filter(variable != "gbpusd_curncy"), 
            aes(y = value, color = factor(variable)), linewidth = 1.5) + 
  geom_point(data = df$truss %>% filter(variable != "gbpusd_curncy"), 
             aes(y = value, color = factor(variable)), size = 1.5) +
  # Secondary Y-axis (GBPUSD scaled)
  geom_col(data = df$truss %>% filter(variable == "gbpusd_curncy"), 
           aes(y = value * scale_factor, fill = variable), 
           color = 'black', alpha = 0.6) +  
  # Reference lines
  geom_hline(yintercept = 0.0, lty = 4) +
  geom_vline(xintercept = as.Date('2022-09-23'), lty = 4, size = 2, color = 'red') +
  geom_vline(xintercept = as.Date('2022-09-28'), lty = 4, size = 2, color = 'black') +
  # Color and Fill Scales
  scale_color_jco(labels = c("1y OIS", "2y OIS", "10y Gilt")) +
  scale_fill_manual(values = c("gbpusd_curncy" = "deeppink4"), 
                    labels = c("gbpusd_curncy" = "GBPUSD")) +  # Correctly label GBPUSD
  # Primary and Secondary Y-Axis
  scale_y_continuous(
    name = "bp, cumulative change",
    sec.axis = sec_axis(~ . / scale_factor, name = "GBPUSD % change")
  ) +
  # Labels & Legends
  labs(title = "Truss, mini-Budget (2022.09.23)", 
       color = "", 
       fill = "")  

# PLOT: /january 2025 episode
scale_factor <- 10

b.plot <- ggplot(df$jan, aes(x = date)) + 
  # Primary Y-axis (Interest Rate Changes)
  geom_line(data = df$jan %>% filter(variable != "gbpusd_curncy"), 
            aes(y = value, color = factor(variable)), linewidth = 1.5) + 
  geom_point(data = df$jan %>% filter(variable != "gbpusd_curncy"), 
             aes(y = value, color = factor(variable)), size = 1.5) +
  # Secondary Y-axis (GBPUSD scaled)
  geom_col(data = df$jan %>% filter(variable == "gbpusd_curncy"), 
           aes(y = value * scale_factor, fill = variable), 
           color = 'black', alpha = 0.6) +  
  # Reference lines
  geom_hline(yintercept = 0.0, lty = 4) +
  geom_vline(xintercept = as.Date('2025-01-07'), lty = 4, size = 2, color = 'red') +
  # Color and Fill Scales
  scale_color_jco(labels = c("1y OIS", "2y OIS", "10y Gilt")) +
  scale_fill_manual(values = c("gbpusd_curncy" = "deeppink4"), 
                    labels = c("gbpusd_curncy" = "GBPUSD")) +  # Correctly label GBPUSD
  # Primary and Secondary Y-Axis
  scale_y_continuous(
    name = "bp, cumulative change",
    sec.axis = sec_axis(~ . / scale_factor, name = "GBPUSD % change")
  ) +
  # Labels & Legends
  labs(title = "January episode (2025.01.07)", 
       color = "", 
       fill = "")  







