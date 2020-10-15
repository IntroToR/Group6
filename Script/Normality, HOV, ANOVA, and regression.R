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
