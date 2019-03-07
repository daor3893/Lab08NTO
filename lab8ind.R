library(tidyverse)
library(data.table)
data <- read.csv("avocado.csv")

tidy_data <- data %>%
  separate(Date, into = c("year","month","day"), sep = "-") %>%
  mutate(conventional = ifelse(type == "conventional", "1", "0")) %>%
  mutate(organic = ifelse(type == "organic", "1", "0")) %>%
  select(-X, -day)

tidy_data$month <- parse_double(tidy_data$month)
tidy_data$year <- parse_double(tidy_data$year)
tidy_data$conventional <- parse_double(tidy_data$conventional)
tidy_data$organic <- parse_double(tidy_data$organic)

avocado <- tidy_data %>%
  group_by(month) %>%
  summarise(avg_price = mean(AveragePrice),
            avg_volume = mean(`Total.Volume`)
  )

ggplot(data = avocado) +
  geom_point(mapping = aes(x = month, y = avg_price, size = avg_volume)) +
  geom_line(mapping = aes(x = month, y = avg_price)) +
  xlab("Month") +
  ylab("Average Price per Unit") +
  ggtitle("Avocado Prices over Time of Year by Volume Sold")
