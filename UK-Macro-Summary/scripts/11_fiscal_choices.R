# Fiscal Reactions
# [1] Forecast News [2] Bohn regression/reaction to Net Debt

highlight <- "Oct-24"
highlight2<- "Mar-25"

#====================================================
# Policy Respoonses to Fiscal news 
#====================================================
input.file <- 'Chapter_3_charts_and_tables_March_2025.xlsx' # C4.F
responses <- read_excel(here('inputs', input.file),
                        sheet = "C3.1", skip = 25) |> 
  janitor::clean_names() |> 
  rename(event = 1)

# recode event label
responses$event <- paste0(str_sub(responses$event, 1, 3), 
                          "-", str_sub(responses$event, -2))

# Plot
responses.scatter <- ggplot(responses, aes(y=policy,x = underlying,
                                           label = event)) + 
  geom_point() + 
  geom_point(data = subset(responses, event==highlight), aes(x=underlying, y=policy),
             color = "red", size=4) +
  geom_point(data = subset(responses, event==highlight2), aes(x=underlying, y=policy),
             color = "darkgreen", size=4) +
  geom_smooth(data=responses, color = "tomato", fill="tomato", method = MASS::rlm) +
  geom_text_repel(size = 11) +
  geom_vline(xintercept = 0.0, lty=4, color='darkred', size=2) + 
  geom_hline(yintercept = 0.0, lty=4, color='darkred', size=2) + 
  labs(title = "Policy Responses to Fiscal Forecast Revisions",
       subtitle = "Robust regression line",
       x = "Pre-measures forecast revision, % GDP",
       y = "Discretionary policy change (% GDP)",
       caption = "Source: OBR")
responses.scatter

# Robust regression 
robust_reg <- MASS::rlm(policy ~ underlying, 
                        data = responses)
tidy(robust_reg)  # summary of coefficients

#====================================================
# primary balance and debt to GDP -- Bohn Regression
#====================================================
tickers <- c("gfsmab_112_s1311b_gpb__z_p", "ons_hf6x_pusf_a", "gbfcst1568")
labels  <- c("primary balance", "Net debt to GDP", "Output gap")
dat <- build.mb.dataset(tickers, labels, freq = "Annual") %>% as.data.frame() %>%
  janitor::clean_names() 
dat$year <- year(as.Date(rownames(dat)))  

dat$primary_balance[dat$year == 2024] <- -1.9 # From OBR EFO March 2025 Table A.9
dat$primary_balance[dat$year == 2023] <- -1.7
dat$net_debt_to_gdp[dat$year == 2024] <- 95.9
dat$net_debt_to_gdp[dat$year == 2023] <- 95.1
dat <-  dplyr::filter(dat, !is.na(primary_balance)) %>%
  mutate(lagged_debt = lag(net_debt_to_gdp)) 

# Two sub-periods by GFC
pre_crisis_dat  <- subset(dat, year<=2007)
post_crisis_dat <- subset(dat, year> 2007)

# Run the Bohn regression: pre-crisis
pre_crisis_model      <- lm(primary_balance ~ lagged_debt + output_gap, 
                            data = pre_crisis_dat)
pre_crisis_dat$fitted <- predict(pre_crisis_model, newdata = pre_crisis_dat)

# Run the Bohn regression: pre-crisis
pre_crisis_model      <- lm(primary_balance ~ lagged_debt + output_gap, data = pre_crisis_dat)
pre_crisis_dat$fitted <- predict(pre_crisis_model, newdata = pre_crisis_dat)

# post-crisis period
post_crisis_model      <- lm(primary_balance ~ lagged_debt + output_gap, data = post_crisis_dat)
post_crisis_dat$fitted <- predict(post_crisis_model, newdata = post_crisis_dat)

# broom::tidy(pre_crisis_model)
# broom::tidy(post_crisis_model)

# Combine fitted data for plotting
fitted_data <- rbind(
  data.frame(year = pre_crisis_dat$year, fitted = pre_crisis_dat$fitted, lag_debt = pre_crisis_dat$lagged_debt, period = "Pre-GFC, fitted"),
  data.frame(year = post_crisis_dat$year, fitted = post_crisis_dat$fitted, lag_debt = post_crisis_dat$lagged_debt, period = "Post-GFC, fitted")
)

# Plot original data and fitted lines
bohn.plot <- ggplot(dat, aes(x = lagged_debt, y = primary_balance)) +
  geom_point(color = "darkblue") +
  geom_text_repel(aes(label = year), color = "black", size = 8) +
  geom_line(data = fitted_data, aes(x = lag_debt, y = fitted, color = period), size = 1.5) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(title = "A Weaker Fiscal Response, amid Higher Debt",
       subtitle = "A positive slope implies debt-stabilising policy",
       x = "Net Debt to GDP, t-1", y = "Primary Balance (% GDP)",
       caption = "Note: The Bohn regressions control for the Output gap") +
  scale_color_manual(values = c("Pre-GFC, fitted" = "red", "Post-GFC, fitted" = "darkgreen")) +
  theme(legend.title = element_blank())
