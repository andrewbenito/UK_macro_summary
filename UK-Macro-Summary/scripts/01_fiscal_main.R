# MAIN.R
# Prepare Quarterly UK Macro Data for UK Fiscal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load standard libraries
lapply(c('here','tidyverse','data.table', 'readxl', 'openxlsx', 'broom', 'janitor', 'stringi',
         'MacrobondAPI','Rblpapi','ggridges', 'ggsci', 'parallel','glue', 'stringr', 
         'tseries', 'seasonal', 'forecast', 'tsibble', 'xts', 'zoo', 'gtsummary', 'gtExtras',
         'latex2exp', 'stargazer', 'sysfonts', 'showtext', 'patchwork', 'naniar'), 
       require, character.only = TRUE)

# helpers
source(here('functions','functions.R'))
source(here('functions','chartFormat.R'))

# settings
date.range  <- '19800101/'

# ==============================================================================
# PREPARE DATA
source(here('scripts','08b_prepare_fiscal.R'))    

# ==============================================================================
# CREATE PLOTS
source(here('scripts','08c_plot_lt_fiscal.R'))

# ==============================================================================


