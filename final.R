library(tidyverse)

# Import the data as a survey data frame
tidy_survey <- read_csv("國家出口總值排名前30名.csv")

# Remove the last column
tidy_survey <- tidy_survey |> 
  select(-ncol(tidy_survey))

# View the updated data
print(tidy_survey)

# Display the first few rows of the data
print(tidy_survey)

tidy_survey <- tidy_survey %>%
  rename(
    period = 期間,
    rank = 排名,
    country = 國家,
    `total export value` = `出口總值(新台幣千元)`,
    percentage = 百分比
  )

# View the updated column names
colnames(tidy_survey)

library(tidyverse)
library(lubridate)

# Convert Taiwan year to Western year and parse the date
tidy_survey <- tidy_survey %>%
  mutate(
    period = str_replace(period, "^(\\d+)", function(x) as.character(as.numeric(x) + 1911)), # Add 1911 to Taiwan year
    period = ym(period) # Parse the updated period as "YYYY-MM"
  )

# Check the updated period column
tidy_survey %>%
  select(period) %>%
  head()

tidy_survey <- tidy_survey %>%
  mutate(
    period = as_factor(period),
    rank = as_factor(rank),
    country = as_factor(country),
    `total export value` = as_factor(`total export value`),
    percentage = as_factor(percentage)
  )

# Check the updated column classes
tidy_survey %>%
  summarise(across(c(period, rank, country, `total export value`, percentage), class))

# <chr> <int> ----
tidy_survey $ `total export value` >= 10^9
tidy_survey $ `total export value` < 10^9 & tidy_survey $ `total export value` >= 10^8
tidy_survey $ `total export value` < 10^8

# Filtering total export value into three levels
tidy_survey |> 
  filter(`total export value` >= 10^9)

tidy_survey |> 
  filter(`total export value` < 10^9 & `total export value` >= 10^8)

tidy_survey |> 
  filter(`total export value` < 10^8)

# Summarize total export value into three levels
summary_total_export <- tidy_survey |> 
  mutate(
    export_category = case_when(
      `total export value` >= 10^9 ~ ">= 10^9",
      `total export value` < 10^9 & `total export value` >= 10^8 ~ "< 10^9 & >= 10^8",
      `total export value` < 10^8 ~ "< 10^8"
    )
  ) |> 
  count(export_category, name = "count") |> 
  arrange(desc(count))

# View summary
print(summary_total_export)



