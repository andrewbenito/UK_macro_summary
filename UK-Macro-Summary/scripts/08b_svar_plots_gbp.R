# ==============================================================================
# Initializing environment and loading functions
# ==============================================================================

# Load standard libraries
lapply(c('tidyverse','data.table','readxl', 'ggsci', 'here', 'patchwork'),
       require, character.only = TRUE)

source(here::here("functions", "chartFormat.R"))
# ==============================================================================
# Parameters
# ==============================================================================

variables <- c("10y Gilt yield", "FTSE-All", "2y Inflation Swap", "GBP Trade-weighted exch rate")
shocks <- c("Monetary Policy", "Demand", "Supply", "Risk Premium")
yaxis <- c("bp", "%", "bp", "%")

start_date <- "2024-12-20"
end_date <- "2025-01-20"
vline_date <- as.Date("2025-01-07")

label <- "_2003"

# ==============================================================================
# Data preparation
# ==============================================================================

n <- length(variables)
k <- length(shocks)
combos <- list()
for(i in 1:length(variables)){
  for(j in 1:length(shocks)){
    combos[[(i-1)*length(shocks)+j]] <- paste(variables[i],"-",shocks[j],sep="")
  }
}
combos <- array(unlist(combos))

input_data <- read_excel(here::here("inputs", "input_data.xlsx"))
colnames(input_data)[1] <- "date"
input_data <- input_data[c(2:(k+1), 1)]
input_data[1:n] <- input_data[1:n]*100
head(input_data)

hd <- read_excel(here::here("inputs", "HistDecompCTM.xlsx"), col_names = combos)
hd$date <- input_data$date
hd[1:(n*k)] <- hd[1:(n*k)]*100
tail(hd)

# ==============================================================================
# Plots
# ==============================================================================
# i=4 is gbp

#for(i in 1:n){
  i <- 4
  df <- bind_cols(input_data["date"], input_data[i], hd[((i-1)*k+1):((i-1)*k+k)])
  df["Residual"] <- df[2] - rowSums(df[3:(2+k)])
  colnames(df)[3:(2+k)] <- shocks
  df <- df[(df$date>=start_date)&(df$date<=end_date),]
  
  df_plot <- df %>%
    rename(col1=2) %>%
    pivot_longer(!c(1,2))
  df_plot$name <- factor(df_plot$name, levels=append(shocks, "Residual"))
  #df_plot
  
  change_plot <- 
    ggplot(data=df_plot) +
    geom_bar(mapping=aes(x=as.Date(date), y=value, fill=name), 
             position="stack", stat="identity") +
    geom_point(mapping=aes(x=as.Date(date), y=col1), size=1) +
    scale_x_date(date_breaks = "10 days", date_labels = "%b-%d") +
    scale_y_continuous(n.breaks = 10) + 
    labs(title = paste(variables[i], " - decomposition of daily change (", yaxis[i], ")", sep=""),
         subtitle = "SVAR model results with sign restrictions",
         y = yaxis[i],
         x = element_blank()) +
    theme(legend.title = element_blank()) + 
    scale_fill_jco() 
  
  
  df_cum <- df
  df_cum[2:(3+k)] <- cumsum(df_cum[2:(3+k)])
  
  df_plot <- df_cum %>%
    rename(col1=2) %>%
    pivot_longer(!c(1,2))
  df_plot$name <- factor(df_plot$name, levels=append(shocks, "Residual"))
  
  
  plot_gbp <- 
    ggplot(data=df_plot) +
    geom_bar(mapping=aes(x=as.Date(date), y=value, fill=name), 
             position="stack", stat="identity", color="black") +
    geom_line(mapping=aes(x=as.Date(date), y=col1), linewidth=1) +
    geom_vline(xintercept = vline_date, lty=4, color="red", size=2) +
    geom_hline(yintercept = 0) +
    scale_x_date(date_breaks = "10 days", date_labels = "%b-%d") +
    scale_y_continuous(n.breaks = 10) + 
    labs(title = paste(variables[i], " - historical decomposition (", yaxis[i], ")", sep=""),
         subtitle = "SVAR model results with sign restrictions",
         y = yaxis[i],
         x = element_blank()) +
    theme(legend.title = element_blank()) + 
    scale_fill_jco() 

#}






