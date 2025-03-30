# UK Fiscal Analysis 
# Andrew Benito
# Uses OBR's Historical Public Finances DB
# creates: (i) fiscal.efo -- EFO Forecast dataframe
#          (ii) combined Historical Fiscal dataset, list of df's for taxes, spending and fiscal aggregates (% GDP)    
#         (iii) discretionary fiscal measures dataset

#rm(list = ls())
# ==============================================================================
#### SECTION 1: Preparing Data files ############
# Manual Inputs
this.efo      <- "Oct2024"  # PSF_aggregates_databank
previous.efo  <- "May-2024"  # PSF_aggregates_databank
hist.db.name  <- "Historical-public-finances-database_July_2023.xlsx"
policy.measures <- "Policy_measures_database_October_2024.xlsx"
hist.obr      <- "Historical_official_forecasts_database_October_2024.xlsx"
# Input Sheet Names [For Historical data from historical database]
taxes.sheet   <- "Receipts (per cent of GDP)"
spending.sheet<- "Spending (per cent of GDP)"
agg.sheet     <- "Aggregates (per cent of GDP)"
#match labels on fiscal events
Budget.match  <- "matching_measures_obr_forecast_history.xlsx"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Specify Data-files [download latest from OBR site. to inputs folder]
# Budgets
#+++++++++
Budget.match <- read_excel(here('inputs', Budget.match),
                           sheet = "Sheet1", skip = 1) %>% data.table(.) %>% dplyr::select(1:5) %>%
  mutate(official_forecast = as.Date(paste(official_forecast, "01"), format = "%B %Y %d"),
         fiscal_event_date = as.Date(fiscal_event_date) )
Budget.dates <- Budget.match |> dplyr::select(fiscal_event_date)

# Load OBR Datasets: Databank (incl Forecasts) 
#++++++++++++++++++++++++++++++++++++++++++++++
fiscal.efo  <- read_excel(here('inputs',glue('PSF_aggregates_databank_{this.efo}.xlsx')),
                          sheet = 3, skip = 3) %>% data.table(.) # sheet: Aggregates: % GDP
efo.previous <- read_excel(here('inputs',glue('PSF_aggregates_databank_{previous.efo}.xlsx')), 
                           sheet = 3, skip = 3) %>% data.table(.) # sheet: Aggregates: % GDP

# Historical DB; 
#++++++++++++++++
taxes.db <- read_excel(here('inputs', hist.db.name),
                       sheet = taxes.sheet, range = "A4:Z329") %>% data.table(.) # sheet: Receipts: % GDP
  
spending.db <- read_excel(here('inputs', hist.db.name),
                                      sheet = spending.sheet, skip = 3 ) %>% data.table(.) # sheet: Spending: % GDP

agg.db <- read_excel(here('inputs', hist.db.name),
                     sheet = agg.sheet, skip = 5) %>% data.table(.) # sheet: Aggregates: % GDP

# Policy Measures db
#+++++++++++++++++++++
measures <- read_excel(here('inputs', policy.measures),
                       sheet = "Borrowing Summary", skip = 1) %>% data.table(.) # sheet: Aggregates: % GDP

# Historical Macro/Fiscal fcasts 
#++++++++++++++++++++++++++++++++
#+ Forecast variables
fcastvar <- c("PSNB", "NGDP", "PSND", "UKGDP", "Outputgap", "Prod", "Unemplrate")
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

# merge Budget dates into each forecast sheet
merge_Budgets <- function(df) {
  df %>%
    left_join(Budget.match, # as_tibble(Budget.match)
              by = c('official_forecast' = 'official_forecast'),
              relationship="many-to-many")
  }
forecasts <- map(forecasts, merge_Budgets)

# separate out these sheets / macro variables
ngdp <- read_excel(here('inputs', hist.obr),
                   sheet = "NGDP", skip = 3) %>% data.table(.) %>% head(-3)

psnb <- read_excel(here('inputs', hist.obr),
                   sheet = "PSNB", skip = 3) %>% data.table(.) %>% head(-3)


#=====================================================================================
# Clean Data
clean.data <- function(data, yearcol, nrowtopdelete=0, nrowbottomdel=0, ndropcol=0) {
  data <- data |> 
    janitor::remove_empty(which = "cols") |> 
    janitor::remove_empty(which = "rows") |> 
    clean_names() |> 
    rename(year = {{yearcol}}) |> 
    mutate(year = as.numeric(substr(stringi::stri_enc_toutf8(year), 1,4))) |> 
    filter(!is.na(year)) |> 
    replace_with_na_all(condition =  ~.x =="-")
# drop rows, bottom and top  
  if (nrowbottomdel >0) {
    data <- head(data, -nrowbottomdel) 
  }
  if (nrowtopdelete >0) {
    data <- tail(data, -nrowtopdelete)
  }
  if (ndropcol >0) {
    data <- dplyr::select(data, -ndropcol)
  }
  data <- data |> type_convert()
  data <- data.frame(lapply(data, function(x) if(is.numeric(x)) round(x,3) else x))
  colnames(data) <- gsub("[0-9]", "", colnames(data))
  return(data)
  }

# EFO file incl OBR Forecasts
fiscal.efo <- clean.data(fiscal.efo, yearcol = 1, nrowtopdelete = 0, nrowbottomdel = 6)
taxes.db   <- clean.data(taxes.db,   yearcol = 1, nrowtopdelete = 2, nrowbottomdel = 0)
spending.db<- clean.data(spending.db,yearcol = 2, nrowtopdelete = 2, nrowbottomdel = 2) 
agg.db     <- clean.data(agg.db,     yearcol = 2, nrowtopdelete = 2, nrowbottomdel = 0)  

# Clean Historical Taxes data (better groups) NB typo [sic]: central_govennment
taxes.db <- taxes.db |> 
  dplyr::select(year, total_business_taxes, total_personal_taxes, total_consumption_taxes, total_central_govennment_receipts) |> 
  mutate(other = total_central_govennment_receipts - rowSums(across(c(total_business_taxes, total_personal_taxes, total_consumption_taxes)))) |> 
  rename(business = total_business_taxes,
         personal = total_personal_taxes,
         consumption = total_consumption_taxes)

# Clean Historical Public spending (better aggregates and components)
spending.db <- spending.db |> 
  dplyr::select(-contains("pension")) |> 
  mutate(
    total_public_sector_spending = ifelse(is.na(total_public_sector_spending), total_central_government_spending, total_public_sector_spending),
    other = total_public_sector_spending - rowSums(across(c(defence, health, social_security,
                                                                                   education, debt_interest)), na.rm = TRUE)
  )

# # Combined OBR Historic db
fiscal.hist.db <- list(taxes = taxes.db,
                       spending = spending.db,
                       agg = agg.db)  

# NGDP Forecasts [extend to loop through ALL sheets]
ngdp <- ngdp |> 
  clean_names() |> 
  rename(fiscal_event = 1) |> 
  rename_with(~ ifelse(seq_along(.) >=2, 
                       substr(., 1, 5), .), everything()) |> type_convert() 

ngdp <- ngdp |> # NGDP - value
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year", names_prefix = "x", values_to = "ngdp")  |> 
  mutate(year   = as.numeric(year))

ngdp.yoy <- ngdp |> group_by(fiscal_event) |> # NGDP %yoy
  mutate(ngdp = c(NA, 100*diff(log(ngdp) )))

# NGDP list: values and %yoy
ngdp.long = list(ngdp = ngdp, 
                 ngdp.yoy = ngdp.yoy)

# fiscal.efo
fiscal.efo <- fiscal.efo %>% 
  mutate(ngdp.yoy = 100*(nominal_gdp_billion / lag(nominal_gdp_billion,1)-1))
  
#+++++++++++++++++++++++++++++
# Policy Measures db: measures
#+++++++++++++++++++++++++++++
# Clean 'measures', pivot longer 
measures <- measures |> clean_names() |> 
  fill(fiscal_event) |> 
  rename_with(~ ifelse(seq_along(.) >2, 
                        substr(., 1, 5), .), everything()) |> type_convert() 

measures.long <- measures |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year", names_prefix = "x", values_to = "effect")  |> 
  mutate(year   = as.numeric(year),
         n      = row_number() ) |> 
  group_by(fiscal_event) |> 
  mutate(
    event_id   = min(n),  
    yr.min     = min(if_else(!is.na(effect), year, NA), na.rm = TRUE),  # problem here
    year.start = if_else(!is.na(effect), yr.min, NA),
    greyed     = year>= (year.start + 5),
    stat_id    = if_else(fiscal_event == "Autumn 2016", event_id, NA) ) |> 
  ungroup() |>  
  tidyr::fill(stat_id) |> 
  group_by(fiscal_event) |> 
  mutate(greyed = if_else(event_id >= stat_id, 
                         (year>=year.start + 6), greyed)) |>   #from Autumn 2016 onwards +6, not +5   
  ungroup() |> 
  # recode greyed to NA
  mutate(effect = if_else(greyed==TRUE, NA, effect))

# merge and scale measures by nominal GDP
measures.long <- left_join(measures.long, ngdp, by = "year", relationship = "many-to-many") |> 
  dplyr::select(-c(stat_id, fiscal_event.y))

# 2. Into Budget Measures data, [measures] merge Budget event data
measures <- left_join(measures, Budget.match, 
                      by = c('fiscal_event' = 'fiscal_event'), relationship="many-to-many") 

