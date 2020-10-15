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