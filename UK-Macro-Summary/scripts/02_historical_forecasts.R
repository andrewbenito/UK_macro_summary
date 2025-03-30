# PLOT: Historical forecasts and their Evolution: 
# Debt Interest, Productivity growth, Primary Balance

# Options
#==========
hist.obr      <- "Historical_official_forecasts_database_October_2024.xlsx"
vlineyr <- 2025
official_forecast <- ""

# Choose variables
fcastvar <- c("PSDebtint", "Outputgap", "Prod", "Netdebtint", "£PSNB", "NGDP")

read_sheets <- function(sheet_name) {
  readxl::read_excel(here('inputs', hist.obr), 
                     sheet = sheet_name, skip = 3) %>% data.table(.) %>% head(-3) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::remove_empty(which = "rows") 
}
forecasts <- set_names(map(fcastvar, read_sheets), fcastvar) 
# Clean each df then apply to list
forecasts <- lapply(forecasts, function(df) {
  df |> 
    mutate(across(where(is.numeric), ~ round(., 1))) %>%    # round the list of dataframes
    rename(official_forecast = 1) %>%
    mutate(official_forecast = as.Date(paste(official_forecast, "01"), format = "%B %Y %d"))
})

# Evolution of Debt interest costs incl Forecasts
#--------------------------------------------------
interest.dat <- forecasts[['Netdebtint']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "value") |> 
  mutate(year = as.integer(substr(name, 1,4)))  
# Plot
interest.plot <- ggplot(interest.dat, 
                        aes(x=year, y = value, group=official_forecast)) + 
  geom_line(color='gray70') + 
  geom_line(data=filter(interest.dat, official_forecast %in% "2024-10-01"), color="navy", size=1.25) + 
  geom_vline(xintercept = vlineyr, lty = 4) +
  labs(title = "Debt Interest Costs",
       subtitle = "Successive OBR forecasts",
       y = "£bn",
       caption = "Source: OBR")
interest.plot


# Evolution of Productivity Growth forecasts
#---------------------------------------------
prody.dat <- forecasts[['Prod']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "value") |> 
  mutate(year = as.integer(substr(name, 1,4)))  
# Final point
final_point <- prody.dat %>%
  filter(official_forecast == last(official_forecast)) %>%
  slice_tail(n = 1)  # Select the last row

# Plot
prody.plot <- ggplot(prody.dat, 
                        aes(x=year, y = value, group=official_forecast)) + 
  geom_line(color='gray70') + 
  geom_line(data=filter(prody.dat, official_forecast == "2024-10-01"), color="navy", size=1.25) + 
  geom_point(data = final_point, aes(x = year, y = value), color = "navy", size = 3) + 
#  Add a label next to the point
  geom_text(data = final_point, aes(x = year, y = value, label = paste0(round(value, 1), "%")),
            hjust = 0.4, vjust = -0.75, color = "navy", size = 8) +
  geom_vline(xintercept = vlineyr, lty = 4) +
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "Labour Productivity forecasts",
       subtitle = "Successive OBR forecasts",
       y = "%yoy",
       caption = "Source: OBR")
prody.plot

# Evolution of Output Gap forecasts
#---------------------------------------------
og.dat <- forecasts[['Outputgap']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "value") |> 
  mutate(year = as.integer(substr(name, 1,4)))  
# Plot
og.plot <- ggplot(og.dat, 
                     aes(x=year, y = value, group=official_forecast)) + 
  geom_line(color='gray70') + 
  geom_line(data=filter(og.dat, official_forecast %in% "2024-10-01"), color="navy", size=1.25) + 
  geom_vline(xintercept = vlineyr, lty = 4) +
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "Output Gap forecasts",
       subtitle = "Successive OBR forecasts",
       y = "%",
       caption = "Source: OBR")
og.plot

# Primary Balance 
#------------------
pb.dat.part1 <- forecasts[['£PSNB']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "psnb") |> 
  mutate(year = as.integer(substr(name, 1,4)))  |> 
  filter(year(official_forecast)>=2010)
pb.dat.part2 <- forecasts[['Netdebtint']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "debtint") |> 
  mutate(year = as.integer(substr(name, 1,4)))
pb.dat.part3 <- forecasts[['NGDP']] |> 
  pivot_longer(!official_forecast, names_prefix = "x", values_to = "ngdp") |> 
  mutate(year = as.integer(substr(name, 1,4)))
# merge components
pb.dat <- left_join(pb.dat.part1, pb.dat.part2)
pb.dat <- left_join(pb.dat, pb.dat.part3)
# Prim Bal
pb.dat <- pb.dat |> 
  mutate(pb = -100*(psnb - debtint)/ngdp  ) |> 
  mutate(name=as.factor(name)) |> 
  filter(!is.na(pb)) 

# Calc t+4 Forecast errors in PB
#=================================
df_recent <- pb.dat |> 
  arrange(year, desc(official_forecast)) |> 
  group_by(year) |> 
  slice(1) |> 
  ungroup() |>  
  filter(year<=year(official_forecast)) |> 
  dplyr::select(year, pb) 
colnames(df_recent) <- c("year", "pb_actual")
# Calc year+4 forecasts 
df_t4 <- pb.dat |> 
  arrange(official_forecast, year) |> 
  filter(year(official_forecast)==year-4) |> 
  group_by(year) |> 
  slice(1) |> # take earliest value when 2 events per year
  ungroup() |> 
  dplyr::select(official_forecast, year, pb)
colnames(df_t4) <- c("official_forecast",  "year", "pb_t4")
# Merge actual and t+4 fcast to original
pb.dat <- pb.dat %>%
  left_join(df_recent, by = "year") |> 
  left_join(df_t4, by = c("year", "official_forecast"))
# Calc t+4 Forecast errors
pb.dat <- pb.dat |> 
  mutate(fe_4y = pb_actual - pb_t4)

pb.dat |> 
  filter(year!=2020) |> 
  summarise(mean = mean(fe_4y, na.rm=TRUE)) 

#==================================

#------------ PLOTS --------------

#==================================
# PB Plot
pb.plot <- pb.dat |> 
  ggplot(aes(x=year, y = pb, group=official_forecast)) + 
  geom_line(color='gray70') + 
  geom_line(data=filter(pb.dat, official_forecast %in% "2024-10-01"), color="navy", size=1.25) + 
  geom_vline(xintercept = 2024, lty = 4) +
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "Primary Balance forecasts",
       subtitle = "Successive OBR forecasts",
       y = "% GDP",
       caption = "Source: OBR")
#pb.plot

# plot: pb latest 
pb.plot.latest <- pb.dat |> 
  filter(official_forecast == last(official_forecast)) |> 
  ggplot(aes(x = year, y = pb)) + 
  geom_col(fill = 'darkblue') + 
  geom_text(aes(label = round(pb, 1), 
                vjust = ifelse(pb >= 0, -0.5, 1.5),
                color = ifelse(year==max(year), "red", "black")), 
            size = 10, 
            na.rm = TRUE) + 
  scale_y_continuous(
    limits = c(-1, 2)  
  ) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(title = "Primary Balance, % GDP",
       subtitle = "Latest OBR forecast",
       y = "Primary Balance % GDP",
       caption = "Source: OBR") + 
  scale_color_identity()
#pb.plot.latest


# Calc for 4y-ahead average Forecast Error
pb.dat |> 
  dplyr::filter(year!=2020) |> 
  summarise(mean = mean(fe_4y, na.rm=TRUE)) 


  