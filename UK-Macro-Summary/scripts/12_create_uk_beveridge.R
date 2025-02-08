# UK Application of Bernanke Blanchard
# 01 - create Data
# ABenito, May 2024 
#++++++++++++++++++++++
lapply(c('here', 'tidyverse', 'openxlsx', 'data.table', 'ggrepel',
         'BVAR', 'MacrobondAPI', 'zoo', 'seasonal',
         'patchwork', 'showtext', 'ggsci'),
       require, character.only = TRUE)

# Settings ----
here::here()
options(warn = -1)
sa.twice         <- FALSE          # If TRUE, SA process is run twice

# Date SETTINGS----
date.range <- '19970101/'

# Functions +++++++++
source(here('functions','functions.R'))
source(here('functions','chartFormat.R'))

#===============
# GET DATA
#===============
# BBG data
map.var  <- read.xlsx(here::here('inputs', 'uk-uv-input-codes.xlsx')) %>% data.table(.)
tickers  <- map.var[MonthlySA ==0][['Ticker']]
labels   <- map.var[MonthlySA ==0][['Label']]
vars.m   <- map.var[MonthlySA==1][['Ticker']]
labels.m <- map.var[MonthlySA==1][['Label']]
id.rec   <- map.var[MonthlySA==2][['Ticker']]
label    <- map.var[MonthlySA==2][['Label']]

### Build Raw dataset: MB (quarterly data) 
#================================================================
data.M  <- build.mb.dataset(vars.m, labels = labels.m, freq = "Monthly") %>% .[date.range] %>% as.xts()
data    <- build.mb.dataset(tickers, labels = labels,  freq = "Quarterly") %>% .[date.range] 
data.rec.m<- build.mb.dataset(id.rec,  labels = label,   freq = "Monthly") %>% .[date.range]

df <- list(raw = data.frame(), clean = data.frame())
# SA: p, pe, pf; merge as quarterly
#data.M  <- lapply(data.M, rigorous.sa)  # SA'd CPI series creates list
#data.M  <- do.call(cbind, data.M)
data.Q  <- apply.quarterly(data.M, mean)
time(data.Q) <- as.Date(as.yearqtr(time(data.Q)))
df[['raw']]    <- merge(data.Q, data) |> as.data.frame() |> 
  mutate(date = zoo:::index(data.Q)) |> arrange(date)

# Transforms
df[['clean']] <- df[['raw']] |> 
  mutate(y           = c(rep(NA, 1), 400*diff(log(gdp), 1)), # annsd GDP growth
         pty         = c(rep(NA, 1), 400*diff(log(pty),1)),
         pty         = c(rep(NA, 7), rollmeanr(pty, 8)), # 8 Q moving avg
         p           = c(rep(NA, 1), 400*diff(log(cpi),1)), # annsd CPI infl 
         wg          = c(rep(NA, 1), 400*diff(log(w),1)),   # annsd wage growth
         vr          = 100*v/(e/3),  
         v_u         = 100*(3*(v/e) / u),
         catchup     = c(rep(NA, 3), rollmeanr(p, 4)) - lag(iesr,4) 
         ) |> 
  dplyr::select(date, y, pty, p, wg, v_u, ur, vr, short, iesr, iesr_citi, ielr_citi, catchup )

# save data
write.csv(df[['clean']],  here::here('inputs', 'df.clean.csv'), row.names = FALSE)

# 
df[['uv']] <- df[['clean']] |>
  mutate(dateLabel = format(date, "%Y-%m"),
         dateLabelShort = as.factor(case_when(dateLabel <= "2008-12" ~ "pre-2009",
                                              dateLabel >= "2009-01" & dateLabel <= "2017-12" ~ "2009-17",
                                              dateLabel >= "2018-01" & dateLabel <= "2020-03" ~ "2018-2020[Mar]",
                                              dateLabel >= "2020-04" & dateLabel <= "2022-12" ~ "2020[Apr]-2022",
                                              dateLabel >= "2023-01" ~ "2023+")))
# Ordering
df[['uv']]$dateLabelShort <- factor(df[['uv']]$dateLabelShort, 
                                    levels = c("pre-2009", "2009-17", "2018-2020[Mar]", "2020[Apr]-2022", "2023+" ))

# change in V/U from end-2019 
df[['dv_u']] <- df[['uv']] |> 
  filter(date>='2019-10-01') |> 
  dplyr::select(date, v_u, vr, ur) |> 
  mutate(dv_u = v_u - v_u[1],
         v.cont = (vr - vr[1]) / ur[1],
         u.cont = -1*(vr[1] * (ur - ur[1]) / (ur[1] * ur)))

df[['dv_u']] <- df[['dv_u']] |> 
  dplyr::select(date, dv_u, v.cont, u.cont) |> 
  pivot_longer(-c(date, dv_u), names_to = "contribution", values_to = "pp")

# UK Beveridge curve
#++++++++++++++++++++
beveridge.plot <- ggplot(df[['uv']], 
       aes(x=ur, y=vr, color = dateLabelShort, shape=dateLabelShort))  +
  geom_point(size=1.75) +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", linewidth=1.65, se = F) +
  geom_text_repel(aes(label=dateLabel, size=1.75)) +
  scale_color_jco() +
  labs(x = 'Unemployment rate, %', y = 'Vacancy rate per person employed',
       title = "UK Beveridge curve",
       color = NULL, shape = NULL, size=NULL) 
  
# Change in V/U ratio since 2019Q4
dv_u <- ggplot(df[['dv_u']], aes(x=date)) + 
  geom_line(aes(y = dv_u))  + 
  geom_bar(aes( y = pp, fill = contribution),
           stat = "identity", position = "stack", alpha = 0.5) +
  geom_hline(yintercept = 0.0, lty = 4) + 
  scale_fill_jco(
    labels = c("v.cont" = "Vacancies",
               "u.cont" = "Unemployed")
  ) + 
  labs(title = "Change in UK V/U ratio" ,
       subtitle = "Change since 2019Q4 and contributions",
       x = "Date", y = "Change in V/U ratio, pp",
       color = NULL)

# REC: D and Supply v Ch in Unemp
data.rec <- as.data.frame(data.rec.m) 
data.rec <- data.rec |> 
  mutate(date = as.Date(row.names(data.rec)),
         s_d = rec_avb - rec_d,
         ur_d=ur - lag(ur,12)) |> 
  dplyr::select(date, s_d, ur_d) |> 
  pivot_longer(!c(date, ur_d), 
               names_to = "series", values_to = "value")

# Plot: Ch in Unemp and REC Availability less Demand
rec_ds <- ggplot(data.rec, aes(x=date)) + 
  geom_col(aes( y = ur_d, fill = "Change Unemp rate, 1y"), alpha=0.6) +  
  geom_line(aes(y = lag(value / 25,4)  , color = "REC Labour availability less Demand"), size=0.75) + 
  scale_y_continuous(
    name = "Ch. in Unemp rate, 1y",
    sec.axis = sec_axis(~.*25, name = "Index points")
  ) + 
  scale_fill_manual(values  = c("Change Unemp rate, 1y" = "#EFC000FF")) +
  scale_color_manual(values = c("REC Labour availability less Demand" = '#0073C2FF')) +
  labs(title = "Unemployment rate outlook" ,
       x = "Date", y = "",
       caption = "Sources: REC/KPMG",
       color=NULL, fill=NULL) +
  geom_hline(yintercept = 0.0, lty=4) 
   

