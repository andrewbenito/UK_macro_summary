# UK MaPS Analysis 
# Andrew Benito
# Uses BoE Market Participants Survey

# ==============================================================================
#### SECTION 1: Preparing Data files ############
set.seed(132)
dates.maps <- c(
  "february-2025",
  "december-2024", "november-2024", "september-2024", "february-2024", "august-2023", "february-2023", 
  "august-2022",   "february-2022"
)

# Convert to named list
names_list <- c("latest", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8") # Corresponding names for the maps list

# Function to create a list structure
create_map_list <- function(dates, names) {
  # Select specific elements for the final list
  selected_dates <- dates[c(1, 2, 3, 4, 5, 6, 7, 8, 9)] # Modify indices as needed
  
  # Map names to the selected dates
  map2(names, selected_dates, ~ list(map = .y)) %>%
    set_names(names)
}

# Create the maps list
maps <- create_map_list(dates.maps, names_list)

# Function to determine correct urlstem based on year
# Function to determine the correct urlstem based on the year in the map name
get_urlstem <- function(map_name) {
  # Extract the year from the map_name (e.g., "december-2024" -> "2024")
  year <- sub(".*-(\\d{4})$", "\\1", map_name)
  
  # Construct urlstem 
  glue("https://www.bankofengland.co.uk/-/media/boe/files/markets/market-intelligence/survey/{year}/")
}  

# Function to download from url
process_file <- function(map_name) {
  # Determine appropriate url stem
  urlstem <- get_urlstem(map_name)
  # Construct filename and url
  name <- glue("market-participants-survey-results-{map_name}.xlsx")
  url  <- paste0(urlstem, name)
  destfile <- tempfile(fileext = ".xlsx")
  # Download
  download.file(url, 
                destfile = destfile, mode = "wb")
  # Read all sheets into list
  sheet_names <- excel_sheets(destfile)
  dat  <- lapply(sheet_names, function(sheet) {
    read_excel(destfile, sheet)
  })
  names(dat) <- sheet_names
  # Clean data
  dat <- map(dat, ~ .x |> 
               janitor::clean_names() |> 
               janitor::remove_empty(which = "cols") |> 
               janitor::remove_empty(which = "rows")  )
  return(dat)
}
# Process all
dat.maps <- map(maps, ~ process_file(.x$map))

# Clean to make numeric format where possible
convert_to_numeric <- function(df) {
  df |> 
    mutate(across(everything(), ~suppressWarnings(as.numeric(.))))
}
dat.maps <- map(dat.maps, ~map(.x, convert_to_numeric))

# Set labels
dat.maps$latest$Q1b$source <- "Feb-2025"
dat.maps$p4$Q1e$source     <- "Feb-2024"
dat.maps$p6$Q1e$source     <- "Feb-2023"

neutral <- dplyr::bind_rows(dat.maps$latest$Q1b, dat.maps$p4$Q1e, dat.maps$p6$Q1e)

# Pivot and plot
dat.neutral <- neutral %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "percentile",
    values_to = "value"
  ) |> 
  filter(!is.na(value)) |> 
  mutate(source = factor(source, levels = c("Feb-2023", "Feb-2024", "Feb-2025")))

# Plot
maps.r.plot <- ggplot(dat.neutral, aes(x = source, y = value, group = source)) +
  geom_boxplot(size=1.75) + 
  labs(x = "Survey", y = "neutral rate, %",
       title = "R*: survey-based estimate",
       subtitle = "medians and IQRs",
       caption = "Source: BoE Market Participants Survey")
maps.r.plot  

# Simulate Indiv Responses for Neutral Rates from
# 3 MAPS surveys
dat.neutral.wide <-dat.neutral |> 
  pivot_wider(names_from = "percentile", values_from = "value") |> 
  rename(median = contains("50th"),
         q1 = contains("25th"), q3 = contains("75th"))

# Function to estimate mean and standard deviation
simulate_responses <- function(median, q1, q3, n = 1000) {
  sd_est <- (q3 - q1) / 1.35  # Approximate SD using IQR
  rnorm(n, mean = median, sd = sd_est)  # Generate normal distribution
}

# Simulate responses for each date
df_simul <- dat.neutral.wide %>%
  group_by(source) %>%
  rowwise() %>%
  mutate(estimate = list(simulate_responses(median, q1, q3, n = number_of_responses))) %>%
  unnest_longer(estimate)  # Expand list into long format

# PLOT withSimulated Individual respnses
boxplot <- ggplot(df_simul, aes(x = factor(source), y = estimate)) +
  geom_boxplot(outlier.shape = NA, fill = "dodgerblue", alpha = 0.5) +  # Boxplot without outliers
  geom_beeswarm(color = "deeppink4", alpha = 0.5, size=2.8) +  # Beeswarm plot for individual points
  labs(title = "R*: survey-based estimate",
       subtitle = "median, IQR and simulated individual estimates",
       x = "Date",
       y = "%",
       caption = "Source: BoE Market Participants Survey"
       ) 
boxplot
