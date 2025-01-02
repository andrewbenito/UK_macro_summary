# Analysis of UK HH Real Income Growth
# July, 2024

# 06_create_data.R
lapply(c('here', 'tidyverse', 'janitor','data.table', 'openxlsx',
         'MacrobondAPI', 'scales', 'patchwork', 'zoo',
         'gt', 'gtExtras', 'stargazer', 'ggsci'),
       require, character.only = TRUE)

# Settings ----
here::here()
source(here('functions','functions.R'))

macro.start  <- '19960101/'

#===============
# 1. IMPORT DATA
#===============
# A. create Quarterly macro data
map.var  <- read.xlsx(here('inputs','uk-hh-input-codes.xlsx')) %>% data.table(.)
vars.idx <- map.var[['MBCode']]
labels   <- map.var[['Label']]
hh.data <- build.mb.dataset(vars.idx, labels, freq = "Quarterly") %>% .[macro.start] %>% as.data.frame() %>%
  clean_names()
hh.data <- hh.data |> 
  mutate(date = as.Date(row.names(hh.data)),
         other = resources_primary - gross_mixed_income - compensation_of_employees - interest_received,
         social_con_ben_tran = net_social_contributions + social_benefits + other_current_transfers_resources, # Resources
         total_resources     = gross_disposable_income  + net_equity_adj,
         hhsr = 100*(gross_saving / (total_resources )),
         nfa_y= nfa / gross_disposable_income,
         fa_y = fa  / gross_disposable_income,
         depo_y = currency_deposits / gross_disposable_income)
# cleaner income groups
hh.data <- hh.data |> 
  mutate(comp = compensation_of_employees,
         gmi  = gross_mixed_income,
         intr = interest_received,
         intp = interest_paid,
         nettrans=social_con_ben_tran - uses_social_contributions - social_benefits_ex_social_transfers - other_current_transfers_uses,
         taxes = current_taxes_on_incomes,
         defl = deflator,
         rdi=real_disposable_income_gbp,
         gpi  = comp + gmi + intr + other - intp,
         gdi = comp + gmi + intr + other - intp + nettrans - taxes) |> 
  dplyr::select(comp:gdi, other, resources_primary, balance_of_gross_primary_incomes, total_resources, hhsr, nfa_y, fa_y, unempr,
         rlend, final_consumption, gross_saving, currency_deposits, depo_y)
# Annual growth rates
yoy.hh.data <- hh.data |> 
  mutate(across(where(is.numeric), ~ c(rep(NA,4), 100*diff(log(.x),4))))
# share of income
share.gdi<- hh.data |> # shares of Gross dispos. income
  mutate(across(everything(), ~ .x / gdi))
share.rdi <- hh.data |> # shares of Real dispos. income
  mutate(across(everything(), ~ .x / rdi))
share.totalres <- hh.data |> # shares of Total resources 
  mutate(across(everything(), ~ .x / total_resources))
# contributions
contributions.gdi <- hh.data %>%
  mutate(across(everything(), ~ c(rep(NA, 4), diff(log(.), 4)) * lag(share.gdi[[cur_column()]], 4)))
contributions.rdi <- hh.data %>%
  mutate(across(c(where(is.numeric), -defl), ~ c(rep(NA, 4), diff(log(.), 4)) * lag(share.rdi[[cur_column()]], 4)),
         defl = c(rep(NA,4), diff(log(defl), 4)))
contributions.totalres <- hh.data %>%
  mutate(across(everything(), ~ c(rep(NA, 4), diff(log(.), 4)) * lag(share.totalres[[cur_column()]], 4)))

#++++++++++++++++++
# Create data: dat
#++++++++++++++++++
dat <- list(base = hh.data,
            yoy  = yoy.hh.data,
            share.gdi = share.gdi,
            share.rdi = share.rdi,
            contributions.gdi = contributions.gdi,
            contributions.rdi = contributions.rdi,
            contributions.totalres = contributions.totalres)
# Add date to each dataframe in list
add.date <- function(df) {
  df %>% mutate(date = as.Date(row.names(df)))
}
dat <- lapply(dat, add.date)

# Plot: Dispos income - nominal and real 
ggplot() + 
  geom_col(aes(x = dat[['yoy']]$date, y = dat[['yoy']]$rdi), fill = '#EFC000FF') +
  geom_line(aes(x = dat[['yoy']]$date, y = dat[['yoy']]$gdi), color = '#0073C2FF') + 
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "UK Household Disposable Income",
       subtitle = "Annual growth in Gross Disposable Income and Real Disposable Income",    
       x = "Date", y = "%yoy")

# Plot dTotal resources = dC + dS
contributions.totalres <- dat[['contributions.totalres']] |> 
  dplyr::select(date, total_resources, final_consumption, gross_saving) |> 
  pivot_longer(cols = -c(date, total_resources), names_to = "contribution", values_to = "pp")  

ggplot(contributions.totalres, aes(fill = contribution, y=pp, x=date)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(aes(x=date, y = total_resources), size = 0.75) +
  scale_fill_jco(
    labels = c("final_consumption" = "Consumption",
               "gross_saving"  = "Saving")
  ) +
  labs(title = "UK Household Income: Consumption and Saving",
       subtitle = "Household income using Total resources definition",
       x = "Date", y = "%yoy and contributions", 
       fill = "") 


# Income Contributions [gdi]
#+++++++++++++++++++++++++++
# gdi = comp + gmi + intr - intp + nettrans + other - taxes
# pivot_longer
contributions.gdi <- dat[['contributions.gdi']] |> 
  select(date, comp:taxes, other) |> 
  mutate(intp = -1*intp, taxes = -1*taxes) |> 
  pivot_longer(!date, names_to = "contribution", values_to = "pp")  |> 
  group_by(date) |> 
  mutate(gdi = sum(pp))
contributions.rdi <- dat[['contributions.rdi']] |> 
  select(date, comp:taxes, other, defl) |> 
  mutate(intp = -1*intp, taxes = -1*taxes, defl = -1*defl) |>   # make negative contribution
  pivot_longer(!date, names_to = "contribution", values_to = "pp")  |> 
  group_by(date) |> 
  mutate(rdi = sum(pp))
# Plot
# GDI
ggplot(contributions.gdi, aes(fill=contribution, y=pp, x=date)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(aes(x=date, y = gdi), size = 1.1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") + 
  scale_fill_jco(
    labels = c("comp" = "Compensation",
               "gmi"  = "Gross mixed income",
               "intp" = "Interest paid",
               "intr" = "Interest received",
               "nettrans"="Net transfers",
               "other" = "Other",
               "taxes" = "Taxes")
  ) +
  labs(title = "Household Nominal Disposable Income",
       subtitle = "Annual growth with contributions",
       x = "Date", y = "%yoy and contributions", 
       fill = "") 

# since 2022
ggplot(subset(contributions.gdi, date>=as.Date("2022-01-01")), aes(fill=contribution, y=pp, x=date)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(aes(x=date, y = gdi), size = 1.1) +
  scale_fill_jco(
    labels = c("comp" = "Compensation",
               "gmi"  = "Gross mixed income",
               "intp" = "Interest paid",
               "intr" = "Interest received",
               "nettrans"="Net transfers",
               "other" = "Other",
               "taxes" = "Taxes")
  ) +
  labs(title = "Household Nominal Disposable Income",
       subtitle = "Annual growth with contributions",
       x = "Date", y = "%yoy and contributions", 
       fill = "") 


# RDI = comp + gmi + intr - intp + nettrans + other - taxes - deflator 
rdi.plot <- ggplot(contributions.rdi, aes(fill=contribution, y=pp, x=date)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(aes(x=date, y = rdi), size = 1.1) +
  scale_fill_jco(
    labels = c("comp" = "Compensation",
               "gmi"  = "Gross mixed income",
               "intp" = "Interest paid",
               "intr" = "Interest received",
               "nettrans"="Net transfers",
               "other" = "Other",
               "taxes" = "Taxes",
               "defl" = "Inflation")
  ) +
  labs(title = "UK Household Real Disposable Income",
       subtitle = "Annual growth with contributions",
       fill = "",
       x = "Date", y = "Real Disposable Income, %yoy and contributions")
#rdi.plot

# since 2021
ggplot(subset(contributions.rdi, date>=as.Date("2021-01-01")) , aes(fill=contribution, y=pp, x=date)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(aes(x=date, y = rdi), size = 1.1) +
  scale_fill_jco(
    labels = c("comp" = "Compensation",
               "gmi"  = "Gross mixed income",
               "intp" = "Interest paid",
               "intr" = "Interest received",
               "nettrans"="Net transfers",
               "other" = "Other",
               "taxes" = "Taxes",
               "defl" = "Inflation")
  ) +
  labs(title = "UK Household Real Disposable Income",
       subtitle = "Annual growth with contributions",
       fill = "",
       x = "Date", y = "Real Disposable Income, %yoy and contributions")

# HH Saving ratio
ggplot() + 
  geom_line(aes(x= dat[['base']]$date, y = dat[['base']]$hhsr)) + 
  geom_point() + 
  geom_hline(yintercept = 0.0, lty=4) +
  labs(title = "Household Saving ratio",
       x = "Date", y = "% total resources",
       caption = "Source: ONS")

# ++++++++++++

# ESTIMATE

#++++++++++++
# savr: unemp(t) + real disp income per head(t+1) + real overdraft rate (t) + wealth/income ratio (t-1)

# model1 <- lm(
#   hhsr ~ unempr + rdi + rlend + log(nfa_y),
#   data = dat[['base']])
# model2 <- lm(
#   hhsr ~ unempr + rdi + rlend + log(nfa_y),
#   data = dat[['base']])
# 
# # subset, pre-covid given forced saving
# model1 <- lm(
#   hhsr ~ unempr + rdi + rlend + log(nfa_y),
#   data = subset(dat[['base']], date<=as.Date("2019-10-01")))
# model2 <- lm(
#   hhsr ~ unempr + rdi + rlend + log(nfa_y),
#   data = subset(dat[['base']], date<=as.Date("2019-10-01")))
#   
# summary(model1)
# summary(model2)

# stargazer





