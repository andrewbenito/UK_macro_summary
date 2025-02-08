# ==============================================================================
#  Estimating Distributions of UK CPI data
# ==============================================================================

# ==============================================================================
# Initialize environment
# ==============================================================================
# Load standard libraries
libs <- c('here','tidyverse','data.table','openxlsx','xts', 'tseries', 'seasonal', 'gt', 'gtExtras',
          'patchwork',  'sysfonts', 'showtext', 
          'MacrobondAPI','forecast','comprehenr','ggridges', 'scales', 'roll','corrplot')
lapply(libs, require, character.only = TRUE)

# ...and project specific ones
source(here('functions', 'UK CPI - functions.R')) # incl chart formatting

# Don't collect any data before...
drop.before <- '19900101/'

# ==============================================================================
# Import data
# ==============================================================================
# Load the map of the variables - their Labels, & macro-bond codes etc
map.var  <- read.xlsx(here('inputs','UK CPI codes.xlsx'), sheet = 'Inflation data') %>% data.table(.)
agg.var  <- read.xlsx(here('inputs','UK CPI codes.xlsx'), sheet = 'Agg') %>% data.table(.)
inflation.level  <- 3

# Define the variables and labels we're after
vars.idx    <- map.var[Category == 'CPI' & Level == inflation.level & Type == 'Index'][['MB.Code']]
labels      <- map.var[Category == 'CPI' & Level == inflation.level & Type == 'Index'][['Label']]
description <- map.var[Category == 'CPI' & Level == inflation.level & Type == 'Index'][['Description']]
group       <- map.var[Category == 'CPI' & Level == inflation.level & Type == 'Index'][['Group']]

# Make dataset with UK inflation indices in
data.infl     <- list()
agg.infl      <- list()
data.infl[['sector']] <- build.mb.dataset(vars.idx,labels) %>% .[drop.before] # component-level
agg.infl[['agg']]    <- build.mb.dataset(agg.var$MB.Code,agg.var$Label) %>% .[drop.before]  # agg groups, NSA

# ==============================================================================
# Prepare data
# ==============================================================================
# Identify the last print date
last.outturn <- end(data.infl[['sector']])
#print(last.outturn)

# Make growth rates, annual and monthly
data.mom <- list()
data.yoy <- list()
for(sector in names(data.infl)){
  data.mom[[sector]] <- 100 * (data.infl[[sector]]/lag(data.infl[[sector]],1 ) - 1)
  data.yoy[[sector]] <- 100 * (data.infl[[sector]]/lag(data.infl[[sector]],12) - 1)
}
# Aggregates mom, yoy, SA
agg.mom <- list()
agg.yoy <- list()
for(group in names(agg.infl)){
  agg.mom[[group]] <- 100 * (agg.infl[[group]]/lag(agg.infl[[group]],1 ) - 1)
  agg.yoy[[group]] <- 100 * (agg.infl[[group]]/lag(agg.infl[[group]],12) - 1)
}
# Seasonally adjust the monthly series, component-level
for(var in names(data.mom$sector)){
  
  first <- first(zoo::index(data.mom$sector[, var]))
  SA    <- tryCatch(ts(data.mom$sector[,var],start=c(year(first),month(first)),frequency=12) %>% seas(.,outlier=NULL),
                    error = function(e) FALSE)
  if(!is.logical(SA)){
    if(!any(grepl('Series should not be a candidate for seasonal adjustment',SA$err$warning))){
      data.mom$sector[, var] <- SA %>% final(.) %>% as.xts(.) %>% merge(.,xts(,order.by=zoo::index(data.mom$sector[, var])))
    }
  } 
}
# SA agg CPI indices, aggregates, also keep original index 
agg.index.sa <- agg.infl$agg # starts with NSA index
colnames(agg.index.sa) <- paste0(colnames(agg.index.sa), ".index")
agg.mom.sa    <- list()

for(var in names(agg.index.sa)){   # %mom SA
  
  first <- first(zoo::index(agg.index.sa[, var]))
  SA    <- tryCatch(ts(agg.index.sa[,var],start=c(year(first),month(first)),frequency=12) %>% seas(.,outlier=NULL),
                    error = function(e) FALSE)
  if(!is.logical(SA)){
    if(!any(grepl('Series should not be a candidate for seasonal adjustment',SA$err$warning))){
      agg.index.sa[, var] <- SA %>% final(.) %>% as.xts(.) %>% merge(.,xts(,order.by=zoo::index(agg.index.sa[, var])))
    }
  } 
}
# SA %mom's
for(var in names(agg.mom$agg)){   # %mom SA
  
  first <- first(zoo::index(agg.mom$agg[, var]))
  SA    <- tryCatch(ts(agg.mom$agg[,var],start=c(year(first),month(first)),frequency=12) %>% seas(.,outlier=NULL),
                    error = function(e) FALSE)
  if(!is.logical(SA)){
    if(!any(grepl('Series should not be a candidate for seasonal adjustment',SA$err$warning))){
      agg.mom$agg[, var] <- SA %>% final(.) %>% as.xts(.) %>% merge(.,xts(,order.by=zoo::index(agg.mom$agg[, var])))
    }
  } 
}

# Get labels in the right order
description <- c()
for(var in names(data.infl$sector)){
  description <- c(description,map.var[Level == inflation.level & MB.Code == var]$Description)
}

# Get slice of last two observations
last.mom <- tail(data.mom$sector,2) %>% as.data.table(.) %>%
  melt(., id.vars = c('index'))  %>% .[,index := paste('D',as.character(index),sep='.')] %>%
  dcast(.,variable ~ index, value.var = c("value")) %>%
  .[,description := description] %>%
  setnames(.,paste('D',last.outturn,sep='.'),'value') %>%
  setnames(.,paste('D',last.outturn - months(1),sep='.'),'previous.value') %>%
  .[order(value)] 
 
last.yoy <- tail(data.yoy$sector,2) %>% as.data.table(.) %>%
  melt(., id.vars = c('index'))  %>% .[,index := paste('D',as.character(index),sep='.')] %>%
  dcast(.,variable ~ index, value.var = c("value")) %>%
  .[,description := description] %>%
  setnames(.,paste('D',last.outturn,sep='.'),'value') %>%
  setnames(.,paste('D',last.outturn - months(1),sep='.'),'previous.value') %>%
  .[order(value)] 

latest.median.all <- median(last.mom$value)
prev.median.all   <- median(last.mom$previous.value) 

# ==============================================================================
# This vs last month distribution plots
# ==============================================================================
# Kernel density of growth rates last time & time before
p.mom <- last.mom %>%
  ggplot(.) +
  geom_density(aes(x=value,color=paste(format(last.outturn, "%B"))), linewidth=2) +
  geom_density(aes(x=previous.value,color=format(last.outturn-month(1), "%B")), linewidth=2) +
  geom_vline(aes(xintercept=median(value, na.rm = TRUE)),
              color="blue", linetype="dashed") + xlim(-1.5,1.5) +  xlab('mom% SA') +
  labs(title = paste("UK CPI Distributions of Price Changes", format(last.outturn, "%B"), "vs", format(last.outturn-month(1), "%B"), "(%mom SA)"), 
       caption = paste(""),
       color="")
p.mom
ggsave('density.this.last.mom.png', plot=p.mom, path = here('plots'),
       width=14, height=10, dpi=300)


# ==============================================================================
# Ridge plots
# ==============================================================================
# Ridge plot of mom growth densities over time - last 10 months
ridges.mom       <- data.mom$sector %>% na.omit() %>% as.data.table(., keep.rownames = 'date') %>%
  pivot_longer(., !date) %>% data.table(.) %>% .[date > last(date) %m-% months(10)] %>% .[,date := paste(year(date),month(date),sep = '-')]
dates.reverse    <- unique(ridges.mom$date) %>% rev(.)
ridges.mom       <- ridges.mom %>% mutate(date = fct_relevel(date, dates.reverse))

ridge.plot.mom <- ggplot(ridges.mom, aes(x=value, y=date, group=date, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 6, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(name = "%mom SA", option = "C") + xlim(-4,4) + xlab('%mom SA') + ylab('') +
  labs(title = "UK CPI Distribution of Price Changes",
       caption = paste("")) +
  theme(plot.caption = element_text(size = 40, color = 'black'))
ridge.plot.mom

ggsave('ridge.plot.mom.png', plot=ridge.plot.mom, path = here('plots'),
       width=14, height=8, dpi=300)


# ==============================================================================
# Split ridges by group
# ==============================================================================
map.group <- map.var[Category == 'CPI'& Type == 'Index'] %>%
  dplyr::select('MB.Code', 'Label', 'Group') %>%
  rename(name = Label)

#[mom]: Goods and Services
ridges.group.mom  <- data.mom$sector %>% na.omit(.) %>% as.data.table(., keep.rownames = 'date') %>%
  pivot_longer(., !date) %>% data.table(.) %>% .[date > last(date) %m-% months(10)] %>% .[,date := paste(year(date),month(date),sep='-')] %>%
  left_join(map.group) %>% dplyr::filter(Group=='Goods'|Group=='Services')

dates.reverse    <- unique(ridges.group.mom$date) %>% rev(.)
ridges.group.mom <- ridges.group.mom %>% mutate(date = fct_relevel(date, dates.reverse))

ridge.group.plot.mom <- ggplot(ridges.group.mom, aes(x=value, y=date, group=date, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 6, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(name = "%mom SA", option = "C") + xlim(-4,4) + xlab('%mom SA') + ylab('') + 
  facet_wrap(~Group) +
  labs(caption = paste("")) +
  theme(plot.caption = element_text(size = 40, color = 'black'))
ridge.group.plot.mom

ggsave('ridge.group.plot.mom.png', plot=ridge.group.plot.mom, path = here('plots'),
       width=25, height=8, dpi=300)

# ==============================================================================
# Medians table
# ==============================================================================
# Services prices; get medians
medians_all <- ridges.group.mom %>%
  group_by(date) %>%
  summarize(medians = round(median(value),3))
medians_g <- ridges.group.mom %>% 
  dplyr::filter(Group == "Goods" ) %>%
  group_by(date) %>%
  summarize(medians = round(median(value),3))
medians_s <- ridges.group.mom %>% 
  dplyr::filter(Group == "Services" ) %>%
  group_by(date) %>%
  summarize(medians = round(median(value),3))

median_table <- bind_cols(medians_all, medians_g, medians_s)[,c(-3,-5)]
colnames(median_table) <- c('Date', 'All', 'Goods', 'Services')

median_table[,'Date']
median_gt <- median_table %>%
  gt(rowname_col = "Date") %>%
  tab_header(title    = md("**UK CPI component medians**"), 
             subtitle = md("***%mom SA***"))
gtsave(median_gt, 'median-table.png', here('plots'))

latest.median.srv <- medians_s %>%
  arrange(date) %>% slice(1) %>%
  pull(medians)
prev.median.srv <- medians_s %>%
  arrange(date) %>% slice(2) %>%
  pull(medians)
latest.median.gd <- medians_g %>%
  arrange(date) %>% slice(1) %>%
  pull(medians)
prev.median.gd <- medians_g %>%
  arrange(date) %>% slice(2) %>%
  pull(medians)

