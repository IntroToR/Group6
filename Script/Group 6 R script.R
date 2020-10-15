rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
write.table(rainfall, "rainfall_overall_data.txt")
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')
write.table(temperature, "temperature_overall_data.txt")

head(rainfall)
head(temperature)

library(lmerTest)
library(lsmeans)
library(multcomp)
library(multcompView)
library(tidyverse)
library(lawstat)
library(doBy)
library(ggplot2)
library(dplyr)
library(patchwork)
library(olsrr)
library(onewaytests)
library(tidytuesdayR)
library(olsrr)
library(onewaytests)
library(nortest)

rainfall$date <- as.Date(paste(rainfall$year, "-", rainfall$month, "-", rainfall$day, sep = ""))

# Change the capitalization of city_names in temperature to match rainfall city_name case
temperature$city_name <- gsub("BRISBANE","Brisbane", temperature$city_name)
temperature$city_name <- gsub("CANBERRA","Canberra", temperature$city_name)
temperature$city_name <- gsub("MELBOURNE","Melbourne", temperature$city_name)
temperature$city_name <- gsub("PERTH","Perth", temperature$city_name)
temperature$city_name <- gsub("SYDNEY","Sydney", temperature$city_name)
head(temperature)

# Filter data from 1970 to 2018, for cities that have BOTH rainfall and temperature data
rainfall_1 <-  rainfall %>%
  filter(date >= "1970-01-01", date <= "2018-12-31") %>%
  filter(city_name == "Brisbane" | city_name == "Canberra" | city_name == "Melbourne" | city_name == "Perth" | city_name == "Sydney")
head(rainfall_1)

temp_1 <- temperature %>%
  filter(date >= "1970-01-01", date <= "2018-12-31") %>%
  filter(city_name == "Brisbane" | city_name == "Canberra" | city_name == "Melbourne" | city_name == "Perth" | city_name == "Sydney")
head(temp_1)

combined <- merge(rainfall_1, temp_1, by = c("city_name", "date"))
write.table(combined, "overall_data.txt")
head(combined)

summaryBy(temperature ~ temp_type + city_name, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

summaryBy(rainfall ~ city_name, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

summaryBy(rainfall ~ month, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

summaryBy(temperature ~ month + temp_type, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

summaryBy(rainfall ~ year, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

summaryBy(temperature ~ year + temp_type, data = combined,
          FUN = function(x) { c(average = mean(x, na.rm = T), standard_deviation = sd(x, na.rm = T), maximum = max(x, na.rm = T), minimum = min(x, na.rm = T)) } )

combined$date_monthly <- as.Date(paste(combined$year, "-", combined$month, "-", 1, sep = ""))
combined$date_yearly <- as.Date(paste(combined$year, "-", 1, "-", 1, sep = ""))

combined_wider <- na.omit(pivot_wider(data = combined, names_from = temp_type, values_from = temperature))
names(combined_wider)[names(combined_wider) == "max"] <- "max_temperature"
names(combined_wider)[names(combined_wider) == "min"] <- "min_temperature"


#Monthly Average Timeline
combined_avg_rt <- aggregate(. ~city_name + date_monthly, data = combined_wider[, c("rainfall","max_temperature","min_temperature","city_name","date_monthly")], mean)

p1 <- ggplot(data = combined_avg_rt) +
  geom_bar(mapping = aes(y = rainfall, x = date_monthly), size = 0.25, colour = "NA", fill = "dodgerblue",stat = "identity") +
  geom_point(mapping = aes(y = min_temperature/0.5, x = date_monthly), size = 0.05, alpha = 0.75, colour = "black") +
  geom_point(mapping = aes(y = max_temperature/0.5, x = date_monthly),size = 0.05, alpha = 0.75, colour = "red") +
  scale_y_continuous(name = ("Rainfall (blue)"), limit = c(-5,65),sec.axis = sec_axis(~.*0.5,name = "Temperature (Max=red, Min=black)")) +
  facet_grid(rows = vars(city_name))+
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  labs(x = "Year",title = "Average Monthly Rainfall and Max/Min Temperatures") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave("montly_average_rainfall_temperatures_timeline.png")


#Yearly Average Timeline
combined_avg_rt <- aggregate(. ~city_name+date_yearly, data = combined_wider[, c("rainfall", "max_temperature", "min_temperature", "city_name", "date_yearly")], mean)

p1 <- ggplot(data = combined_avg_rt) +
  geom_bar(mapping = aes(y = rainfall, x = date_yearly), size = 0.05, colour = "NA", fill = "dodgerblue", stat = "identity") +
  geom_point(mapping = aes(y = min_temperature/0.75, x = date_yearly), size = 0.05, alpha = 0.75, colour = "black") +
  geom_point(mapping = aes(y = max_temperature/0.75, x = date_yearly), size = 0.05, alpha = 0.75, colour = "red") +
  scale_y_continuous(name = ("Rainfall (blue)"), limit = c(-5,65), sec.axis = sec_axis(~.*0.75, name = "Temperature (Max = red, Min = black)")) +
  facet_grid(rows = vars(city_name)) +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  labs(x = "Year", title = "Average Yearly Rainfall and Max/Min Temperatures") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave("yearly_average_rainfall_temperatures_timeline.png")

Run_Normality_HOV_ANOVA_LinearReg <- function(y_input, factor_input, dataframe_name){
model <- lm(y_input ~ factor_input, data = dataframe_name)
ols_plot_resid_qq(model)
ols_plot_resid_fit(model)
#Anderson-Darling Normality Test
ad_out <- ad.test(model$residuals) #Normality
print(ad_out)
cat("\n-------------------------------\n\n")

#HOV Test: Brown-Forsythe test
res <- bf.test(rainfall ~ city_name, data = dataframe_name)

cat("\n-------------------------------\n-\n-\n-\n")
#ANOVA Table
anova(model)

#Significance of Slope Linear Regression
summary(model)
}

combined_wider$month <- as.numeric(combined_wider$month) 
combined_wider$year <- as.numeric(combined_wider$year) 
combined_wider$city_name <- as.factor(combined_wider$city_name)

### Run the function with format as shown below:
### Run_Normality_HOV_ANOVA_LinearReg(dataframe$response_columnname,dataframe$group_columnname,dataframe)

#==== Looking at all cities from 2008 - 2018====
Current_period <- combined_wider %>% filter(date >= "2008-01-01", date <= "2018-12-31")

#Rainfall and Year 
Run_Normality_HOV_ANOVA_LinearReg(Current_period$rainfall,Current_period$year,Current_period)

#Max Temperature and Years 
Run_Normality_HOV_ANOVA_LinearReg(Current_period$max_temperature,Current_period$year,Current_period)

#Max Temperature and Year 
Run_Normality_HOV_ANOVA_LinearReg(Current_period$min_temperature,Current_period$year,Current_period)

#==== Looking at Sydney and Perth only from 1970-2018 ====
sydney_and_perth_only <- combined_wider %>% filter(city_name == "Sydney" | city_name == "Perth")

#Rainfall and Year 
Run_Normality_HOV_ANOVA_LinearReg(sydney_and_perth_only$rainfall,sydney_and_perth_only$year,sydney_and_perth_only)

#Max Temperature and Years 
Run_Normality_HOV_ANOVA_LinearReg(sydney_and_perth_only$max_temperature,sydney_and_perth_only$year,sydney_and_perth_only)

#Max Temperature and Year 
Run_Normality_HOV_ANOVA_LinearReg(sydney_and_perth_only$min_temperature,sydney_and_perth_only$year,sydney_and_perth_only)

means <- combined_wider %>%
  group_by(max_temperature, city_name) %>%
  summarise(mean = mean(rainfall, na.rm = T))
ggplot(means, aes(x = max_temperature, y = mean)) + 
  geom_point() +
  ylab("rainfall") + xlab("max_temperature") +
  facet_grid(~city_name) +
  ggtitle("Average rainfall by maximum temperature for each city") +
  ggsave("avg_rainfall_by_max_temp_for_each_city.png")

means0 <- combined_wider %>%
  group_by(min_temperature, city_name) %>%
  summarise(mean = mean(rainfall, na.rm = T))
ggplot(means0, aes(x = min_temperature, y = mean)) +
  geom_point() +
  ylab("rainfall") + xlab("min_temperature") +
  facet_grid(~city_name) +
  ggtitle("Average rainfall by minimum temperature for each city") +
  ggsave("avg_rainfall_by_min_temp_for_each_city.png")

means1 <- combined %>%
  group_by(year) %>%
  summarise(mean = mean(rainfall, na.rm = T))
ggplot(means1, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("rainfall") + xlab("year") +
  ggtitle("Average rainfall by year") +
  ggsave("avg_rainfall_by_year.png")

means2 <- combined_wider %>%
  group_by(year) %>%
  summarise(mean = mean(max_temperature, na.rm = T))
ggplot(means2, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("max_temperature") + xlab("year") +
  ggtitle("Average maximum temperature by year") +
  ggsave("avg_max_temp_by_year.png")

means3 <- combined_wider %>%
  group_by(year) %>%
  summarise(mean = mean(min_temperature, na.rm = T))
ggplot(means3, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("min_temperature") + xlab("year") +
  ggtitle("Average minimum temperature by year") +
  ggsave("avg_min_temp_by_year.png")

means4 <- combined %>%
  group_by(month) %>%
  summarise(mean = mean(rainfall, na.rm = T))
ggplot(means4, aes(x = month, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("rainfall") + xlab("month") +
  ggtitle("Average rainfall by month") +
  ggsave("avg_rainfall_by_month.png")

means5 <- combined_wider %>%
  group_by(month) %>%
  summarise(mean = mean(max_temperature, na.rm = T))
ggplot(means5, aes(x = month, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("max_temperature") + xlab("month") +
  ggtitle("Average maximum temperature by month") +
  ggsave("avg_max_temp_by_month.png")

means6 <- combined_wider %>%
  group_by(month) %>%
  summarise(mean = mean(min_temperature, na.rm = T))
ggplot(means6, aes(x = month, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("min_temperature") + xlab("month") +
  ggtitle("Average minimum temperature by month") +
  ggsave("avg_min_temp_by_month.png")

means7 <- combined %>%
  group_by(city_name, year) %>%
  summarise(mean = mean(rainfall, na.rm = T))
ggplot(means7, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("rainfall") + xlab("year") +
  facet_grid(~city_name) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average rainfall by city and year") +
  ggsave("avg_rainfall_by_city_and_year.png")

means8 <- combined_wider %>%
  group_by(city_name, year) %>%
  summarise(mean = mean(max_temperature, na.rm = T))
ggplot(means8, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("max_temperature") + xlab("year") +
  facet_grid(~city_name) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average maximum temperature by city and year") +
  ggsave("avg_max_temp_by_city_and_year.png")

means9 <- combined_wider %>%
  group_by(city_name, year) %>%
  summarise(mean = mean(min_temperature, na.rm = T))
ggplot(means9, aes(x = year, y = mean)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  ylab("min_temperature") + xlab("year") +
  facet_grid(~city_name) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average minimum temperature by city and year") +
  ggsave("avg_min_temp_by_city_and_year.png")

write.table(means, "rainfall means by max_temperature, city_name.txt")
write.table(means0, "rainfall means by min_temperature, city_name.txt")
write.table(means1, "rainfall means by year.txt")
write.table(means2, "max_temperature means by year.txt")
write.table(means3, "min_temperature means by year.txt")
write.table(means4, "rainfall means by month.txt")
write.table(means5, "max_temperature means by month.txt")
write.table(means6, "min_temperature means by month.txt")
write.table(means7, "rainfall means by city_name, year.txt")
write.table(means8, "max_temperature means by city_name, year.txt")
write.table(means9, "min_temperature means by city_name, year.txt")

