# Calculate Debt Stabilising Primary Balance for different combinations of r, g

# PB = [(Rt – Gt) / (1 + Gt)] dt-1 + sfat

# Rt: effective nominal interest rate on government debt, 
# Gt is the nominal growth rate, dt is the debt to GDP ratio, and sfat is stock-flow adjustments
#=================================================================================================
# library(tidyverse)
# library(gt)


# Set initial values

sfa <- 0 # ignore 

# set up range for r and g
r   <- seq(0.015, 0.04, 0.005)  
g   <- seq(0.010, 0.040, 0.005)  

grid <- data.frame(expand.grid(
  r = r,
  g = g,
  d = 100
))

# debt stabilising primary balance
grid <- grid |> 
  mutate(pb = ((r - g) / (1-g)) * d)  

# Pivot to create table
dat <- grid |> 
  select(r, g, pb) |> 
  pivot_wider(names_from = r, values_from = pb )

# format dat
gt.dat <- dat |> 
  gt() |> 
  fmt_number(
    decimals = 2
  ) |>
  fmt_number(
    columns = g,
    decimals = 3 # change g column to 3 digits
  ) |> 
  tab_options(
    table.font.size = "small"
  ) |> 
  data_color(
    columns = -g,
    method = "numeric",
    palette = "inferno",
    direction = "column", 
    reverse = TRUE
  ) |> 
  cols_label(
    g = md("**g**")
  ) |> 
  tab_header(
    title = "Required, Debt-stabilising, Primary Balance (% GDP)",       # 
    subtitle = md("**effective nominal interest rate, r**")  
  ) |> 
  tab_footnote(
    footnote = "Note: Calculations assume Debt/GDP at 100% and no stock-flow adjustments, for different combinations of 'r' and 'g'.",
    placement = "left"
  )
#gt.dat




