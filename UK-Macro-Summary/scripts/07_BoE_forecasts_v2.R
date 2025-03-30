# Analysis of BoE Forecast Errors
# ABenito, March 2025
#=================================
lapply(c('rdbnomics', 'ggfan'),require, character.only = TRUE)

# INPUT: Most recent MPR ----
year  <- 2025
month <- 'february'
startYear <- 2000

finalReport <- year + case_when(month == 'february' ~ 0.00,
                                month == 'may' ~ 0.25,
                                month == 'august' ~ 0.50,
                                month == 'november' ~ 0.75) 
# Get Data ---- 
sheetnames <- c('CPI Forecast', 'GDP Forecast', 
                'Unemployment Forecast', 'GDP')
url = glue("https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/{year}/{month}/mpr-{month}-{year}-chart-slides-and-data.zip")

# Download
td <- tempdir() 
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf, mode = 'wb')
# Extract filenames from zip
zip_files <- unzip(tf, list = TRUE)$Name
excel_files <- zip_files[grepl("\\.xlsx$", zip_files)]

# function to find right file for each sheet----
#-----------------------------------------------
find_and_read_sheet <- function(sheet_name, excel_files, zip_file, temp_dir) {
  # extract all Excel files to temp directory
  unzip(zip_file, files = excel_files, exdir = temp_dir)
  
  # try find the sheet in each Excel
  for (file in excel_files) {
    file_path <- file.path(temp_dir, file)
    sheet_names <- excel_sheets(file_path)
    
    if (sheet_name %in% sheet_names) {
      message("Found sheet '", sheet_name, "' in file: ", file)
      return(read_xlsx(file_path, sheet = sheet_name))
    }
  }
  warning("Sheet '", sheet_name, "' not found in any file")
  return(NULL)
}

# Extract and Read Files into Data Frames ----
dataframes <- map(sheetnames, ~ find_and_read_sheet(.x, excel_files, tf, td))
# name dataframes by sheet names
names(dataframes) <- sheetnames
# Clean temp files
unlink(tf)
file.remove(list.files(td, full.names = TRUE, pattern = "\\.xlsx$"))

# Tidy----
# Inflation [df1]
dataframes[['CPI Forecast']] <- dataframes[['CPI Forecast']] %>%
  # Set column names from the 4th row and remove the first 4 rows
  {
    col_names <- as.character(unlist(slice(., 4)))
    col_names[is.na(col_names) | col_names == ""] <- paste0("col_", seq_along(col_names))
    setNames(tail(., -4), col_names)
  } %>%
  type.convert(as.is = TRUE) %>%
  # Rename the first four columns
  rename(reportDateY = 1, reportDateM = 2, conditioning = 3, summary = 4) %>%
  # Mutate and recode columns as needed
  mutate(
    summary = as.character(summary),
    reportNext = ifelse(summary == "Market Mode", 1, 0),
    reportID = cumsum(reportNext),
    rateAssump = ifelse(grepl("^Market", summary), "market", "constant"),
    mm = substr(reportDateM, 1, 3),
    reportmm = match(mm, month.abb),
    summary = recode(summary, `Market Mode` = "Mode", `Market Mean` = "Mean", `Market Median` = "Median")
  ) %>%
  group_by(reportID) %>%
  fill(mm, reportmm, reportDateY, conditioning, .direction = "downup") %>%
  ungroup() %>%
  fill(reportDateY, .direction = "downup") %>%
  dplyr::filter(as.numeric(reportDateY) >= 2005) %>%
  mutate(
    reportDate = ymd(paste(reportDateY, reportmm, "1", sep = "-")),
    reportyq = ifelse(month(reportDate) == 1, year(reportDate), year(reportDate) + (month(reportDate) - 2) / 12)
  ) %>%
  dplyr::select(-c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  # Remove columns that are entirely NA
  dplyr::select_if(~!all(is.na(.))) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"), names_to = "date", values_to = "value") %>%
  pivot_wider(names_from = summary, values_from = value) %>%
  mutate(
    dateY = as.numeric(substr(date, 1, 4)),
    dateQ = as.numeric(sub(".*Q(\\d)$", "\\1", date)),
    dateyq = dateY + (dateQ - 1) / 4
  ) %>%
  group_by(reportID, dateyq) %>%
  fill(Uncertainty, Skew, .direction = "downup") %>%
  dplyr::filter(rateAssump == "market" & !is.na(Mode)) %>%
  ungroup()


# CPI inflation, back-data 
cpi <- rdb(ids = "ONS/MM23/D7G7.Q") %>%
  dplyr::select(period, value, series_code) %>%
  dplyr::filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         Mode = value, Median = value, Mean = value) 
# append CPI with forecast CPI, for each Report; 
data.cpi <- left_join(dataframes[['CPI Forecast']], cpi, by="dateyq")
#data.cpi <- bind_rows(cpi, dataframes[['CPI Forecast']]) 

# Tidy Unemployment Rate forecasts [only avbl from 2014]
dataframes[['Unemployment Forecast']] <- dataframes[['Unemployment Forecast']] %>%
  set_names(dplyr::slice(.,4)) %>% 
  tail(-4) %>%
  type.convert() 
colnames(dataframes[['Unemployment Forecast']])[1:4] <- c('reportDateY', 'reportDateM', 'conditioning', 'summary')
dataframes[['Unemployment Forecast']] <- dataframes[['Unemployment Forecast']] %>%
  dplyr::filter(substr(summary,1,4) != "Prob") %>%
  type.convert() 
dataframes[['Unemployment Forecast']] <- dataframes[['Unemployment Forecast']] %>%
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
  dplyr::filter(reportDateY>=2014) %>%
  dplyr::select(!c('2013Q3','2013Q4')) %>%
  mutate(reportDate = lubridate::ymd( paste(reportDateY, reportmm, "1", sep = "-")),
         reportyq = ifelse(month(reportDate)==1, year(reportDate),
                           year(reportDate) + (month(reportDate)-2)/12)) %>%
  dplyr::select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  dplyr::select(where( ~!all(is.na(.x))) ) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date") %>%
  pivot_wider(names_from = summary) %>%
  mutate(dateY = as.numeric(str_sub(date,1,4)),
         dateQ = as.numeric(str_sub(date,-1)),
         dateyq = dateY + (dateQ-1)/4) %>%
  group_by(reportID, dateyq) %>%
  fill('Standard deviation', 'Skewness', .direction = "downup") %>%
  dplyr::filter(rateAssump == "market" & !is.na(Mode))

#  Claude Code:

process_forecast_data <- function(df, 
                                  min_year = 2005,
                                  cols_to_rename = c('reportDateY', 'reportDateM', 'conditioning', 'summary'),
                                  filter_prob = TRUE,
                                  exclude_columns = NULL,
                                  uncertainty_cols = c('Uncertainty', 'Skew')) {
  
  # Extract column names from the 4th row and remove the first 4 rows
  df <- df %>%
    {
      col_names <- as.character(unlist(slice(., 4)))
      col_names[is.na(col_names) | col_names == ""] <- paste0("col_", which(is.na(col_names) | col_names == ""))
      setNames(tail(., -4), col_names)
    } %>%
    type.convert(as.is = TRUE)
  
  # Rename the first four columns
  names(df)[1:4] <- cols_to_rename
  
  # Filter out probability rows if needed
  if(filter_prob) {
    df <- df %>% dplyr::filter(substr(summary, 1, 4) != "Prob")
  }
  
  # Main transformations
  df <- df %>%
    mutate(
      summary = as.character(summary),
      reportNext = ifelse(summary == "Market Mode", 1, 0),
      reportID = cumsum(reportNext),
      rateAssump = if_else(grepl("^Market", summary), "market", "constant"),
      mm = substr(reportDateM, 1, 3),
      reportmm = match(mm, month.abb),
      summary = recode(summary, 
                       `Market Mode` = "Mode", 
                       `Market Mean` = "Mean", 
                       `Market Median` = "Median")
    ) %>%
    group_by(reportID) %>%
    fill(mm, reportmm, reportDateY, conditioning, .direction = "downup") %>%
    ungroup() %>%
    fill(reportDateY, .direction = "downup") %>%
    dplyr::filter(as.numeric(reportDateY) >= min_year)
  
  # Exclude specific columns if needed
  if(!is.null(exclude_columns)) {
    df <- df %>% dplyr::select(!all_of(exclude_columns))
  }
  
  # Continue processing
  df <- df %>%
    mutate(
      reportDate = ymd(paste(reportDateY, reportmm, "1", sep = "-")),
      reportyq = year(reportDate) + (month(reportDate) %% 12 - 1) %/% 3 / 4
    ) %>%
    dplyr::select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    relocate(reportID, rateAssump, .before = conditioning) %>%
    pivot_longer(cols = starts_with("20"), names_to = "date", values_to = "value") %>%
    pivot_wider(names_from = summary, values_from = value) %>%
    mutate(
      dateY = as.numeric(substr(date, 1, 4)),
      dateQ = as.numeric(sub(".*Q(\\d)$", "\\1", date)),
      dateyq = dateY + (dateQ - 1) / 4
    )
  
  # Handle uncertainty columns with dynamic column names
  if(all(uncertainty_cols %in% names(df))) {
    df <- df %>%
      group_by(reportID, dateyq) %>%
      fill(all_of(uncertainty_cols), .direction = "downup")
  } else if("Standard deviation" %in% names(df) && "Skewness" %in% names(df)) {
    df <- df %>%
      group_by(reportID, dateyq) %>%
      fill(`Standard deviation`, Skewness, .direction = "downup")
  }
  
  # Final filter
  df <- df %>%
    dplyr::filter(rateAssump == "market" & !is.na(Mode)) %>%
    ungroup()
  
  return(df)
}

# how you'd apply it to CPI Forecast
dataframes[['CPI Forecast']] <- process_forecast_data(
  dataframes[['CPI Forecast']],
  min_year = 2005,
  uncertainty_cols = c('Uncertainty', 'Skew')
)

# Apply to Unemployment Forecast
dataframes[['Unemployment Forecast']] <- process_forecast_data(
  dataframes[['Unemployment Forecast']],
  min_year = 2014,
  exclude_columns = c('2013Q3', '2013Q4'),
  uncertainty_cols = c('Standard deviation', 'Skewness')
)

# GDP growth
dataframes[['GDP Forecast']] <- process_forecast_data(
  dataframes[['GDP Forecast']],
  min_year = 2005,
  uncertainty_cols = c('Uncertainty', 'Skew')
)





# Unemployment rate back-data [ Forecasts from 2014]
unemp <- rdb(ids = "ONS/LMS/MGSX.Q") %>%
  dplyr::select(period, value, series_code) %>%
  dplyr::filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         Mode = value, Median = value, Mean = value) 
# append back-data with forecast Unemployment rate, for each Report; 
data.unemp <- left_join(dataframes[['Unemployment Forecast']], unemp, by="dateyq")

# Tidy GDP Growth data
dataframes[['GDP Forecast']] <- dataframes[['GDP Forecast']] %>%
  set_names(dplyr::slice(.,4)) %>% 
  tail(-4) %>%
  type.convert()
colnames(dataframes[['GDP Forecast']])[1:4] <- c('reportDateY', 'reportDateM', 'conditioning', 'summary')
dataframes[['GDP Forecast']] <- dataframes[['GDP Forecast']] %>%
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
  dplyr::filter(reportDateY>=2005) %>%
  mutate(reportDate = lubridate::ymd( paste(reportDateY, reportmm, "1", sep = "-")),
         reportyq = ifelse(month(reportDate)==1, year(reportDate),
                           year(reportDate) + (month(reportDate)-2)/12)) %>%
  dplyr::select(!c(mm, reportmm, reportDateY, reportDateM, reportNext)) %>%
  dplyr::select(where( ~!all(is.na(.x))) ) %>%
  relocate(reportID, rateAssump, .before = conditioning) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date") %>%
  pivot_wider(names_from = summary) %>%
  mutate(dateY = as.numeric(str_sub(date,1,4)),
         dateQ = as.numeric(str_sub(date,-1)),
         dateyq = dateY + (dateQ-1)/4) %>%
  group_by(reportID, dateyq) %>%
  fill(Uncertainty, Skew, .direction = "downup") %>%
  dplyr::filter(rateAssump == "market" & !is.na(Mode)) %>%
  dplyr::filter(reportyq<2020.5 | reportyq> 2021.25)

# GDP back-data 
gdp <- rdb(ids = "ONS/UKEA/YBEZ.Q") %>%
  dplyr::select(period, value, series_code) %>%
  dplyr::filter(year(period)>=startYear) %>%
  mutate(dateyq = year(period) + (month(period)-1)/12,
         value = 100*(value/lag(value,4)-1),
         Mode = value, Median = value, Mean = value) 
# append back-data with forecast annual GDP growth, for each Report; 
data.gdp <- left_join(dataframes[['GDP Forecast']], gdp, by="dateyq")

# merge Actual Data U and Pi
cpiAdj <- cpi %>%
  dplyr::select(!c(Median, Mode, value, series_code)) %>%
  rename(cpi = Mean)
unempAdj <- unemp %>%
  dplyr::select(!c(Median, Mode, value, series_code)) %>%
  rename(unemp = Mean)
data4 <- left_join(cpiAdj, unempAdj) %>%
  mutate(year = year(period))

# Macro news: scale, persistence and source of news justifies policy decisions

# FIG: Forecast News INFL
# Plot Successive forecasts
infl.forecasts <- ggplot(data.cpi, aes(x=dateyq, y=`Mean.x`, group=reportDate, color = reportDate)) + 
  geom_line() + 
  geom_hline(yintercept = 2.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "CPI Inflation",
       x="Date", y="BoE CPI Inflation forecast, %yoy")
infl.forecasts

# Latest set of Inflation Forecasts (latest 5 Reports)
infl.forecasts.latest <- ggplot(subset(data.cpi, reportDate>=last(reportDate) %m-% months(5*3))) + 
  geom_line(aes(x=dateyq, y=`Mean.x`, group=reportDate, color = reportDate)) +
  geom_point(
    data = subset(data.cpi, reportDate == last(reportDate)),  # Filter points for latest reportDate
    aes(x = dateyq, y = `Mean.x`),
    color = "blue",
    size = 3) +
  geom_hline(yintercept = 2.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "CPI Inflation",
       x="Date", y="BoE CPI Inflation forecast, %yoy")
infl.forecasts.latest

# FIG: Forecast News UNEMPLOYMENT
unemp.forecasts <- ggplot(data.unemp, aes(x=dateyq, y=Mean.x, group=reportDate, color = reportDate)) + 
  geom_line() + 
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "Unemployment rate",
       x="Date", y="BoE Unemployment rate forecast, %")
unemp.forecasts

# FIG: Forecast News GDP Growth
# Plot: GDP
gdp.forecasts <- ggplot(data.gdp, aes(x=dateyq, y=Mean.x, group=reportDate, color = reportDate)) + 
  geom_line() + 
  geom_hline(yintercept = 0.0, lty=4) +
  theme(legend.position = "none") +
  labs(color=NULL, 
       subtitle = "GDP growth",
       x="Date", y="BoE GDP growth forecasts %yoy")
gdp.forecasts

# Define a function to process the actual and predicted values for CPI, U 
# INFL: Actual and Predicted
cpiT <- cpi %>%
  dplyr::select(dateyq, value) 
CPIactualPredicted <- left_join(data.cpi, cpiT) %>%
  rename(cpi = value) %>%
  mutate(h = dateyq - reportyq,
         FE = cpi - Mean.x) %>%
  dplyr::select(!c('Uncertainty', 'Skew'))
colnames(CPIactualPredicted)[7:9] <- paste("CPI_", colnames(CPIactualPredicted[,c(7:9)]), sep = "") 

# UNEMPLOYMENT Actual and Predicted 
unempT <- unemp %>%
  dplyr::select(dateyq, value) 
UnempactualPredicted <- left_join(data.unemp, unempT) %>%
  rename(unemp = value) %>%
  mutate(h = dateyq - reportyq,
         FE = unemp - Mean.x) %>%
  dplyr::select(!c('Standard deviation', 'Skewness'))
colnames(UnempactualPredicted)[7:9] <- paste("Unemp_", colnames(UnempactualPredicted[,c(7:9)]), sep = "")

# GDP Actual and Predicted 
gdpT <- gdp %>%
  dplyr::select(dateyq, value) 
gdpactualPredicted <- left_join(data.gdp, gdpT) %>%
  rename(gdp = value) %>%
  mutate(h = dateyq - reportyq,
         FE = gdp - Mean.x) %>%
  dplyr::select(!c('Uncertainty', 'Skew'))
colnames(gdpactualPredicted)[7:9] <- paste("gdp_", colnames(gdpactualPredicted[,c(7:9)]), sep = "")


# Define a function to process each data set
#=============================================
process_data <- function(forecast_data, actual_data, variable_name, 
                         cols_to_remove = c('Uncertainty', 'Skew')) {
  
  # Prepare actual data
  actual_T <- actual_data %>%
    dplyr::select(dateyq, value)
  
  # Join and process
  result <- left_join(forecast_data, actual_T) %>%
    rename_with(~ variable_name, "value") %>%
    mutate(h = dateyq - reportyq,
           FE = !!sym(variable_name) - Mean.x) %>%
    dplyr::select(!all_of(cols_to_remove))
  
  # Rename columns with variable prefix
  col_indices <- 7:9
  colnames(result)[col_indices] <- paste0(variable_name, "_", colnames(result)[col_indices])
  
  return(result)
}

# Process each dataset
CPIactualPredicted <- process_data(data.cpi, cpi, "cpi", c('Uncertainty', 'Skew'))
UnempactualPredicted <- process_data(data.unemp, unemp, "unemp", c('Standard deviation', 'Skewness'))
gdpactualPredicted <- process_data(data.gdp, gdp, "gdp", c('Uncertainty', 'Skew'))















#=================
# FORECAST ERRORS
#=================
# Histograms
# CPI
# subset h>=1y
latest_date <- max(CPIactualPredicted$dateyq[!is.na(CPIactualPredicted$FE)], na.rm = TRUE)

# Filter the data for the latest date and h >= 1
latest_data <- CPIactualPredicted %>%
  filter(dateyq == latest_date & h >= 1)

# Create the histogram with the indicator
p3 <- ggplot(subset(CPIactualPredicted, h >= 1 & year >= 2014), aes(x = FE)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") + 
  xlab("Forecast error, pp") + 
  geom_vline(xintercept = 0.0, lty = 4, color = 'black', size = 1.25) +
  labs(subtitle = "CPI Inflation") +
  geom_point(data = latest_data, aes(x = FE, y = 0, color = "Latest quarter"), size = 3) +
  scale_color_manual(name = "", values = c("Latest quarter" = "blue")) +
  theme(legend.position = "bottom")
#p3

#--------------
# Unemployment
#--------------
# subset h>=1y
latest_date <- max(UnempactualPredicted$dateyq[!is.na(UnempactualPredicted$FE)], na.rm = TRUE)
# Filter the data for the latest date and h >= 1
latest_data <- UnempactualPredicted %>%
  filter(dateyq == latest_date & h >= 1)

p4 <- ggplot(subset(UnempactualPredicted, h>=1), aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp") + 
  geom_vline(xintercept = 0.0, lty=4, color = 'black', size = 1.25) +
  labs(subtitle = "Unemployment rate") + 
  geom_point(data = latest_data, aes(x = FE, y = 0, color = "Latest quarter"), size = 3) +
  scale_color_manual(name = "", values = c("Latest quarter" = "blue")) +
  theme(legend.position = "bottom")
#p4

#------------
# GDP growth
#------------
# subset h>=1y
latest_date <- max(gdpactualPredicted$dateyq[!is.na(gdpactualPredicted$FE)], na.rm = TRUE)

# Filter the data for the latest date and h >= 1
latest_data <- gdpactualPredicted %>%
  filter(dateyq == latest_date & h >= 1)

p5 <- ggplot(subset(gdpactualPredicted, h>=1&year>=2014), aes(x=FE)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  xlab("Forecast error, pp annual") + 
  geom_vline(xintercept = 0.0, lty=4, color = 'black', size = 1.25) +
  labs(subtitle = "GDP growth") + 
  geom_point(data = latest_data, aes(x = FE, y = 0, color = "Latest quarter"), size = 3) +
  scale_color_manual(name = "", values = c("Latest quarter" = "blue")) +
  theme(legend.position = "bottom")
#p5

#p3 + p4 + p5 

