# PLOT: Historical forecasts and their Evolution: 
# Debt Interest and Productivity growth


hist.obr      <- "Historical_official_forecasts_database_October_2024.xlsx"

# Choose variables
fcastvar <- c("PSDebtint", "Outputgap", "Prod", "Netdebtint")

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
  geom_vline(xintercept = 2024, lty = 4) +
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
# Plot
prody.plot <- ggplot(prody.dat, 
                        aes(x=year, y = value, group=official_forecast)) + 
  geom_line(color='gray70') + 
  geom_line(data=filter(prody.dat, official_forecast %in% "2024-10-01"), color="navy", size=1.25) + 
  geom_vline(xintercept = 2024, lty = 4) +
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "Labour Productivity forecasts",
       subtitle = "Successive OBR forecasts",
       y = "%yoy",
       caption = "Source: OBR")
prody.plot

