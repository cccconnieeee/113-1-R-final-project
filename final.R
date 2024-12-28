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


# View the updated data
print(tidy_survey)

# Display the first few rows of the data
print(tidy_survey)

tidy_survey <- tidy_survey %>%
  rename(
    period = 期間,
    rank = 排名,
    country = 國家,
    `total export value(1000NTD)` = `出口總值(新臺幣千元)`,
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
    `total export value(1000NTD)` = as_factor(`total export value(1000NTD)`),
    percentage = as_factor(percentage)
  )


# Check the updated column classes
tidy_survey %>%
  summarise(across(c(period, rank, country, `total export value(1000NTD)`, percentage), class))


# Ensure `total export value` is numeric for comparisons
tidy_survey <- tidy_survey |> 
  mutate(`total export value(1000NTD)` = as.numeric(as.character(`total export value(1000NTD)`)))

# Filtering total export value into three levels
tidy_survey |> 
  filter(`total export value(1000NTD)` >= 10^9)

tidy_survey |> 
  filter(`total export value(1000NTD)` < 10^9 & `total export value(1000NTD)` >= 10^8)

tidy_survey |> 
  filter(`total export value(1000NTD)` < 10^8)



# View the summary
print(summary_total_export)


tidy_survey <- tidy_survey %>%
  mutate(
    period = "2023"  
  )


# 確保 `total export value(1000NTD)` 是數值型態
tidy_survey <- tidy_survey %>%
  mutate(`total export value(1000NTD)` = as.numeric(`total export value(1000NTD)`))

# 選擇排名前 30 名的國家並按總值排序
top30_data <- tidy_survey %>%
  arrange(desc(`total export value(1000NTD)`)) %>%
  slice_head(n = 30) # 前30名

# 繪製 Bar Chart
ggplot(top30_data, aes(x = reorder(country, `total export value(1000NTD)`), 
                       y = `total export value(1000NTD)`, 
                       fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Top 30 Countries by Total Export Value",
    x = "Country",
    y = "Total Export Value (1000 NTD)"
  ) +
  coord_flip() + # 將 x 和 y 軸翻轉，便於顯示
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8), # 調整字體大小
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )



