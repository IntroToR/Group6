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