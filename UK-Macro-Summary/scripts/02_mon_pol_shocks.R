# Mon Pol Shocks using Braun et al data

rm(list = ls())

lapply(c('knitr', 'here', 'tidyverse', 'openxlsx', 'tsbox', 'Rblpapi', 'data.table', 'ggtext',
         'MacrobondAPI', 'ggsci', 'patchwork', 'ggbeeswarm', 'sysfonts', 'readxl', 'ggExtra',
         'kableExtra', 'showtext', 'hrbrthemes', 'gt', 'gtExtras', 'gridExtra', 'janitor',
         'hablar', 'Rblpapi'),
       require, character.only = TRUE)
here::here()

# Source Functions
source(here('functions','chartFormat.R'))


# Mon pol factors
# DATA: Braun et al 2024
event.factors <- read_excel(here::here('inputs', 'measuring-monetary-policy-in-the-uk-the-ukmpesd.xlsx'),
                            sheet = "Factors") |>
  rename(date = Datetime) |>
  pivot_longer(!date, names_to = "monetary_factor", values_to = "effect" ) 

event.factors <- event.factors |> 
  filter(!monetary_factor=="isMPC")

event.factors.wide <- read_excel(here::here('inputs', 'measuring-monetary-policy-in-the-uk-the-ukmpesd.xlsx'),
                                 sheet = "Factors") |> rename(date = Datetime)

MPC.factors <- read_excel(here::here('inputs', 'measuring-monetary-policy-in-the-uk-the-ukmpesd.xlsx'),
                          sheet = "Factors") |>
  rename(date = Datetime) |>
  pivot_longer(!date, names_to = "monetary_factor", values_to = "effect" )

event.surprises <- read_excel(here::here('inputs', 'measuring-monetary-policy-in-the-uk-the-ukmpesd.xlsx'),
                              sheet = "Surprises")


# Plot Monetary Factors
ordered <- c("Target", "Path", "QE")
event.factors$monetary_factor <- factor(event.factors$monetary_factor, levels = ordered)
  

p1 <- ggplot(event.factors) +
  geom_col(aes(x=date, y =effect, color = monetary_factor)) +
  facet_wrap(~monetary_factor) +
  labs(y = "effect, pp",
       title = "Evolving BoE Monetary Policy Factors",
       subtitle = "Monetary policy factors, at each MPC announcement event",
       caption = "Source: UK Monetary Policy Event Study Database (UKMPD)") +
  theme(legend.position = "none")
