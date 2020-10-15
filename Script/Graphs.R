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
