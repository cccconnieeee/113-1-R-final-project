library(tidyverse)

# Import the data as a survey data frame
tidy_survey <- read_csv("國家出口總值排名前30名.csv")

# Remove the last column
tidy_survey <- tidy_survey |> 
  select(-ncol(tidy_survey))

# Preserve the data from rows 271 to 300 
tidy_survey <- tidy_survey |> 
  slice(271:300)

# Check the updated dataframe
print(tidy_survey)

library(tidyverse)

# Example dataset
tidy_survey <- tibble(period = c("民國112年1-12月"))

# Convert `民國112年1-12月` to start and end dates
tidy_survey <- tidy_survey |> 
  mutate(
    # Extract Taiwan year and convert to Western year
    western_year = as.numeric(str_extract(period, "\\d+")) + 1911,
    
    # Create start and end dates
    start_date = paste0(western_year, "-01-01"),
    end_date = paste0(western_year, "-12-31")
  ) |> 
  select(start_date, end_date) # Keep only the start and end dates

# View the updated data
print(tidy_survey)


# View the updated data
print(tidy_survey)

# Display the first few rows of the data
print(tidy_survey)

tidy_survey <- tidy_survey %>%
  rename(
    period = 期間,
    rank = 排名,
    country = 國家,
    `total export value` = `出口總值(新臺幣千元)`,
    percentage = 百分比
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



# Ensure `total export value` is numeric for comparisons
tidy_survey <- tidy_survey |> 
  mutate(`total export value` = as.numeric(as.character(`total export value`)))

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

# View the summary
print(summary_total_export)


# View summary
print(summary_total_export)



