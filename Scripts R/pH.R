#pH calcule / pH mesure

data <- read_delim("Data/DATA_SOMLIT_07-23_CLEAN.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

data <- data[c(997:1544),]

#plot pH_s vs plot_calc :

#plot deltas :
pH_plot_long <- data %>% 
  rename(`pH mesuré (spectro)` = pH_s_somlit_temp_insi, `pH calculé` = pH_calc) %>%
  pivot_longer(cols = c(`pH mesuré (spectro)`, `pH calculé`), names_to = "Legend", values_to = "value")

pH_plot <- pH_plot_long %>% 
  ggplot() +
  ggtitle("Calculated and measured pH by spectrophotometry") +
  geom_point(aes(x=sampling_date, y=value, color= Legend), size=2, shape = 18) +
  scale_y_continuous(name="pH", limits = c(7.8,8.25)) + 
  scale_x_date(name="", date_breaks="1 year", labels = date_format("%Y")) + 
  scale_colour_discrete(type=c("#303030", "#357AB7"))




#scatter plot :

plot <- data %>% 
  ggplot() + 
  aes(x = pH_s_somlit_temp_insi, y = pH_calc) +
  geom_point() +
  ggtitle("Scatter plot pH calculated vs pH measured") +
  stat_smooth(method="lm", formula = y ~ x) +
  scale_x_continuous(name="pH spectro") +
  scale_y_continuous(name="pH calc")

      
#regression
reg_pH <- lm(data = data, pH_calc ~ pH_s_somlit_temp_insi)
slope_pH <- reg_pH$coefficients[[2]]
intercept_pH <- reg_pH$coefficients[[1]]      
      

