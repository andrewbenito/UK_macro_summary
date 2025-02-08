# 02 - explore Historical DB data.R
# Explore Annual UK Fiscal Data 

# Quarterly UK data
#============================
source(here('functions','chartFormat.R' ) )


# Historical db: since 1700 
#===========================
# Public Debt and Borrowing since 1700
debt.lt.plot <- fiscal.hist.db$agg  |> 
  dplyr::select(year, contains("public_sector_net")) |> 
  ggplot(aes(x=year)) + 
  geom_col(aes(y = public_sector_net_borrowing_psnb_, fill = "Borrowing")) + 
  geom_line(aes(y = public_sector_net_debt_psnd_ / 10, color = "Debt"), linewidth=1.4) + 
  scale_fill_manual(values = c("Borrowing" = "darkblue")) +
  scale_color_manual(values = c("Debt" = "red")) +
  scale_y_continuous(
    name = "Borrowing (% GDP)",
    sec.axis = sec_axis(transform = ~.*10, name = "Debt (% GDP)")
  ) +
  scale_x_continuous(
    breaks = seq(1700, max(fiscal.hist.db$agg$year, na.rm = TRUE), by = 25),
    name = "Date"
  ) +
  labs(x = "Date", y ="% GDP",
       title = "UK Public Debt and Borrowing since 1700",
       caption = "Source: OBR",
       fill = NULL,
       color = NULL)
#debt.lt.plot

# From 1950
debt.lt.plot1950 <- fiscal.hist.db$agg  |> 
  dplyr::select(year, contains("public_sector_net")) |> 
  dplyr::filter(year>=1950) |> 
  ggplot(aes(x=year)) + 
  geom_col(aes(y = public_sector_net_borrowing_psnb_, fill = "Borrowing")) + 
  geom_line(aes(y = public_sector_net_debt_psnd_ / 10, color = "Debt"), linewidth=1.4) + 
  scale_fill_manual(values = c("Borrowing" = "darkblue")) +
  scale_color_manual(values = c("Debt" = "red")) +
  scale_y_continuous(
    name = "Borrowing (% GDP)",
    sec.axis = sec_axis(transform = ~.*10, name = "Debt (% GDP)")
  ) +
  scale_x_continuous(
    breaks = seq(1700, max(fiscal.hist.db$agg$year, na.rm = TRUE), by = 25),
    name = "Date"
  ) +
  labs(x = "Date", y ="% GDP",
       title = "UK Public Debt and Borrowing since 1950",
       caption = "Source: OBR",
       fill = NULL,
       color = NULL)
#debt.lt.plot1950

# Average Debt/GDP in MPC's 1997-2007
fiscal.hist.db$agg |> 
  dplyr::filter(year>=1950) |> 
  dplyr::filter(year>=1997 & year <=2007) |> 
  summarise(avg = mean(public_sector_net_debt_psnd_, na.rm = TRUE))

fiscal.hist.db$agg |> 
  dplyr::filter(year==last(year)) |> 
  summarise(avg = mean(public_sector_net_debt_psnd_, na.rm = TRUE))



# Public sector spending since 1700
# where is health?
spend.lt.plot <- fiscal.hist.db[['spending']] |> 
  dplyr::select(c(year, defence, health, social_security,
                  education, debt_interest, other)) |> 
  pivot_longer(!year, names_to = "category", values_to = "value") |> 
  mutate(category = str_replace_all(category, "_", " ")) |> 
  mutate(category = factor(category, levels = c("other", "education", "health", "social security", "debt interest", "defence" ))) |> 
  ggplot(aes(x=year, y=value, fill = category)) +
  geom_col() + 
  scale_fill_jco() + 
  scale_x_continuous(
    breaks = seq(1700, max(fiscal.hist.db$spending$year, na.rm = TRUE), by = 50),
    name = "Date"
  ) +
  theme(legend.position = "right") +
  labs(y ="% GDP",
       title = "UK Public Spending since 1700",
       caption = "Source: OBR",
       fill = NULL)
#spend.lt.plot
  
# Gov Receipts since 1700
tax.lt.plot <- fiscal.hist.db[['taxes']] |> 
  dplyr::select(-contains("central")) |> 
  pivot_longer(!year, names_to = "category", values_to = "value") |> 
  mutate(category = str_replace_all(category, "_", " ")) |> 
#  mutate(category = factor(category, levels = c("other public sector spending", "education","social security", "debt interest", "defence" ))) |> 
  ggplot(aes(x=year, y=value, fill = category)) +
  geom_col() + 
  scale_fill_jco() + 
  scale_x_continuous(
    breaks = seq(1700, max(fiscal.hist.db$taxes$year, na.rm = TRUE), by = 50),
    name = "Date"
  ) +
  theme(legend.position = "right") +
  labs(y ="% GDP",
       title = "UK Tax / Receipts since 1700",
       caption = "Source: OBR",
       fill = NULL)






