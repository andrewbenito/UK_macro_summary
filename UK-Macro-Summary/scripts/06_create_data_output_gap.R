# Analysis of UK HH Real Income Growth
# July, 2024

# 01_create_data_output_gap.R
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
vars   <- c('gbsurv0076', 'gbsurv0049', 'gbfcst1568', 'gbsurv00256')
labels <- c('Services_at_fullcapacity', 'Manuf_at_fullcapacity', 'obr_output_gap', 'agents_capu')

# Build data
dat    <- build.mb.dataset(vars, labels, freq = "Quarterly") %>% .[macro.start] %>% as.data.frame() %>%
  clean_names()
# Mutate
dat <- dat |> 
  mutate(date = as.Date(row.names(dat)),
         bcc_capu = 0.8*services_at_fullcapacity + 0.2*manuf_at_fullcapacity,
         bcc_capu = (bcc_capu - mean(bcc_capu, na.rm=TRUE)) / sd(bcc_capu, na.rm = TRUE),
         agents_capu = (agents_capu - mean(agents_capu, na.rm = TRUE)) / sd(agents_capu, na.rm = TRUE)) 

# Plot with ribbon
og.plot <- ggplot(dat, aes(x = date)) +
  geom_ribbon(aes(
    ymin = pmin(bcc_capu, agents_capu), 
    ymax = pmax(bcc_capu, agents_capu)), alpha = 0.3) +
  geom_line(aes(y = bcc_capu, color = "BCC"), linewidth = 1) +
  geom_point(data = dat[dat$date == max(dat$date[!is.na(dat$bcc_capu)]), ], 
             aes(y = bcc_capu, color = "BCC"), size = 3) +
  geom_line(aes(y = agents_capu, color = "Agents"), linewidth = 1) +
  geom_point(data = dat[dat$date == max(dat$date[!is.na(dat$agents_capu)]), ], 
             aes(y = agents_capu, color = "Agents"), size = 3) +
  geom_line(aes(y=obr_output_gap, color = "Output gap (OBR)"), linewidth=1.2) + 
  geom_hline(yintercept = 0.0, lty=4) +
  scale_y_continuous(
    name = "Capacity Util. st.devs",
    sec.axis = sec_axis(~., name = "Output gap (%)")
  ) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) + 
  labs(title = "The UK Output gap and Capacity Utilisation",
       caption = "Sources: BCC, BoE, OBR",
       color = NULL) + 
  # Add fill scale for the ribbon
  scale_color_jco() +
  # Organize the legend
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))

