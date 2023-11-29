library(readr)
library(ggplot2)
library(dplyr)


my_data <- read.csv("C:/Users/justi/OneDrive/Desktop/country vaccinations/country_vaccinations.csv")
str(my_data)
head(my_data)
summary(my_data)
missing_values <- is.na(my_data)
col_missing <- colSums(missing_values)
col_missing
albania_data <- my_data[my_data$country == "Albania", ]
people_vaccinated_albania <- albania_data$people_vaccinated

print(people_vaccinated_albania)



albania_data$date <- as.Date(albania_data$date)
ggplot(data = albania_data, aes(x = date, y = daily_vaccinations)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Date", y = "Daily Vaccinations", title = "Daily Vaccinations in Albania") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal()

mean_daily_vaccinations <- mean(albania_data$daily_vaccinations, na.rm = TRUE)
median_daily_vaccinations <- median(albania_data$daily_vaccinations, na.rm = TRUE)
std_dev_daily_vaccinations <- sd(albania_data$daily_vaccinations, na.rm = TRUE)

summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation"),
  Value = c(mean_daily_vaccinations, median_daily_vaccinations, std_dev_daily_vaccinations)
  
  print(summary_table)
  
  
  
  albania_montenegro_data <- my_data %>%
    filter(country %in% c("Albania", "Montenegro"))
  ggplot(data = albania_montenegro_data, aes(x = date, y = daily_vaccinations, color = country)) +
    geom_point(size = 3) +  
    labs(x = "Date", y = "Daily Vaccinations", title = "Daily Vaccinations in Albania and Montenegro") +
    scale_color_manual(values = c("Albania" = "blue", "Montenegro" = "red")) +  
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank())
  
 
  write.csv(my_data, "my_data.csv")
  
  
  
  

