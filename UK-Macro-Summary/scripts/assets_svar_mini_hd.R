# Plot HD from 2-var Decomposition of GBPUSD and 10y Yield Gap


contributions <- apply(var1, c(1,2), median) %>%   # k x T
  as.data.frame() %>% t() # T x k
T <- dim(contributions)[1]
dates <- tail(df.wide$date,T) %>% as.data.frame() 
contributions <- cbind(contributions, dates) # dataframe
colnames(contributions) <- c(
  "UK Demand",
  "UK risk premium", 
  "date")

# add total daily change [df_]
var1_T <-  tail(df.wide$gbpusd_curncy,T) 
contributions <- cbind(contributions, var1_T)

# pivot_longer
contributions <- contributions |> 
  pivot_longer(!c(date, var1_T), names_to = "contributions", values_to = "value")

# Plot 10y OIS
ggplot(contributions) + 
  geom_bar(aes(x=date, y = value, fill = as.factor(contributions)),
           position = "stack", stat = "identity") +
  geom_point(aes(x=date, y = var1_T)) +
  geom_hline(yintercept = 0.0, lty=4) +
  labs(x = "date", y = "contribution, bp",
       title = "GBPUSD: Historical Decomposition",
       subtitle = "Change (bp). Estimated contributions from SVAR under sign restrictions",
       caption = "Source: Eisler Capital",
       fill = NULL)

