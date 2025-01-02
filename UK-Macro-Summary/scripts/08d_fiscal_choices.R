

input.file <- 'Chapter_3_charts_and_tables_March_2024_update.xlsx' # C4.F
responses <- read_excel(here('inputs', input.file),
                        sheet = "C3.B", skip = 26) |> 
  janitor::clean_names() |> 
  rename(event = 1)

responses.scatter <- ggplot(responses, aes(y=discretionary_package,x = underlying_change_in_ca_psnb,
                                           label = event)) + 
  geom_point() + 
  geom_smooth(data=subset(responses,
                          discretionary_package<1.5),color = "tomato", fill="tomato", method = MASS::rlm) +
  geom_text_repel(size = 11) +
  geom_vline(xintercept = 0.0, lty=4, color='darkred', size=2) + 
  geom_hline(yintercept = 0.0, lty=4, color='darkred', size=2) + 
  labs(title = "Policy Responses to Fiscal Forecast Revisions",
       subtitle = "With Robust regression line, and excl Outlier",
       x = "Structural forecast revision, % GDP",
       y = "Discretaionary fiscal change (% GDP)",
       caption = "Source: OBR")
#responses.scatter

