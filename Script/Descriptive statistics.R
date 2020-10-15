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
