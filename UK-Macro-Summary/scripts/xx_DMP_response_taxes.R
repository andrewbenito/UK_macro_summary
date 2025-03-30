# Chart of Responses to Budget Payroll taxes

select.month <- "2025-01-01" # input Preferred month

input.file <- 'monthly-dmp-data-for-website-january-2025-1.xlsx' # Download DMP
responses <- read_excel(here('inputs', input.file),
                        sheet = "NICs Reaction", skip = 2) |> 
  janitor::clean_names() |> type_convert() |> 
  rename(date = 1) |> 
  mutate(date = excel_numeric_to_date(as.numeric(date))) |> 
  drop_na() |> 
  pivot_longer(!date, names_to = "response") |> 
  filter(date==select.month)

# clean _ in responses
responses$response <- gsub("_", " ", responses$response)

# Bar-plot
responses.bar <- 
  ggplot(responses, aes(x = reorder(response, value), y = value, label = sprintf("%.1f%%", value))) +  # Format labels with 1 decimal and %
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(hjust = -0.2, size = 10) +  # Add formatted labels at the end of the bars
  labs(title = "Q: Responses to Increased National Insurance costs?",
       x = "Response",
       y = "% Respondents",
       caption = "Source: BoE Decision Maker Panel") + 
  expand_limits(y = max(responses$value) * 1.2) + 
  coord_flip() 
#responses.bar
