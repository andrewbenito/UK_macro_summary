# Analysis of UK HH Real Income Growth
# July, 2024

# 01_create_data.R
lapply(c('here', 'tidyverse', 'janitor','data.table', 'openxlsx',
         'MacrobondAPI', 'scales', 'patchwork', 'zoo',
         'gt', 'gtExtras', 'stargazer', 'ggsci'),
       require, character.only = TRUE)

# Settings ----
here::here()
source(here('functions', 'functions.R'))
macro.start  <- '19900101/'
#===============
# 1. IMPORT DATA
#===============
# A. create Quarterly macro data
vars   <- c('gbsurv0076', 'gbsurv0049', 'gbfcst1568')
labels <- c('Services_at_fullcapacity', 'Manuf_at_fullcapacity', 'obr_output_gap')

# Build data
dat    <- build.mb.dataset(vars, labels, freq = "Quarterly") %>% .[macro.start] %>% as.data.frame() %>%
  clean_names()
# Mutate
dat <- dat |> 
  mutate(date = as.Date(row.names(dat)),
         full_capacity = 0.8*services_at_fullcapacity + 0.2*manuf_at_fullcapacity,
         full_capacity_mean = mean(full_capacity, na.rm = TRUE),
         og_proxy = full_capacity - full_capacity_mean )

# Plot: 
coeff = 0.2
og.plot <- ggplot(data = dat, aes(x=date)) + 
  geom_line(aes(y=og_proxy, color = "At full capacity"), linewidth=1.2) + 
  geom_line(aes(y=obr_output_gap / coeff, color = "Output gap (OBR)"), linewidth=1.2) + 
  geom_hline(yintercept = 10.0, color = "red", lty = 4, size=1.25) +
  geom_hline(yintercept = 0.0, color = "gray", lty = 4, size=1.25) +
  scale_y_continuous(
    name = "At Full capacity, dev from mean",
    sec.axis = sec_axis(~.*coeff, name = "Output gap (%)" )
  ) + 
  labs(title = "UK Output Gap proxy",
       subtitle = "",
       caption = "Source: BCC, OBR",
       color = NULL) + 
  scale_color_jco()

