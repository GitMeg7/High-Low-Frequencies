
### TDT : Point B - Jan.2007 - Dec.2022 ###
### Temperature (°C) - surface ###

DATA <- read_delim("PtB_07-22_temp_0m.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


## PART 1 : RESIDUALS BY BATES ET AL., 2014 ##

# Monthly means :
monthly_means <- ungroup(DATA) %>% 
  group_by(Month) %>%
  summarise(
    Temp_month = mean(temp, na.rm = TRUE),
    SD = sd(temp, na.rm = TRUE))

# Anomalies :
anomalies <- left_join(ungroup(DATA), monthly_means, by = "Month") %>%
  mutate(Temp_ano = temp - Temp_month)


# Regression and table anomalies
var_list <- "Temp_ano"

lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), data = anomalies))
})
reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}
colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano_surf <- reg

reg_ano_surf # slope : 0.0598



## TDT ## 

#formula :

#Fac1 = (3.3 * σN)/|ω0|
#Fac2 = √[(1+Φ)/(1-Φ)]
#TDT = (Fac1 * Fac2)^(2/3)

#with :

# σN :  sd(anomalies)
sd(anomalies$Temp_ano) # 1.232239
# ω0 : slope
# slope : 0.0598
# Φ : autocorr(anomalies) at lag 1
acf(anomalies$Temp_ano, plot=F) # 0.484


Fac1 <- (3.3 * 1.232239)/abs(0.0598)

Fac2 <- sqrt((1 + 0.484)/(1 - 0.484))

TDT <- (Fac1 * Fac2)^(2/3) # 23.69 years

##################################################

## PART 2 : RESIDUALS WITH TS OBJECT + DECOMPOSE FUCTION ##

# TS object : 
ts_temp <- ts(DATA$temp, start = c(2007,1), end = c(2022,52), freq = 52)

# DECOMPOSE function :
ts_temp_decomp <- decompose(ts_temp, type = "additive")

autoplot(ts_temp_decomp) +
  xlab(' ')

# Deseasonalizing data :
ts_temp_less_season <- ts_temp_decomp$x - ts_temp_decomp$seasonal

# Plot : 
plot_ts_temp_less_season <- autoplot(ts_temp_less_season) + 
  xlab("") + ylab("Temp. (°C)") +
  ggtitle("Sea surface temperature time series (without seasonality)") +
  geom_smooth(method=lm)

# Data of linear regression : 
reg_val_less_season <- as.data.frame(plot_ts_temp_less_season$data)

model_less_season <- lm(formula=y~x, data=reg_val_less_season)
summary(model_less_season) # slope : 0.058022

## TDT ## 

#formula :

#Fac1 = (3.3 * σN)/|ω0|
#Fac2 = √[(1+Φ)/(1-Φ)]
#TDT = (Fac1 * Fac2)^(2/3)

#with :

# σN :  sd(residuals)
sd(ts_temp_decomp$random[27:806]) # 1.134308
# ω0 : slope
# slope : 0.058022
# Φ : autocorr(residuals) at lag 1
acf(ts_temp_decomp$random[27:806], plot=F) # 0.674


Fac1_bis <- (3.3 * 1.134308)/abs(0.058022)

Fac2_bis <- sqrt((1 + 0.674)/(1 - 0.674))

TDT_2 <- (Fac1 * Fac2)^(2/3) # 23.69 years

########################################################

## TDT_1 et TDT_2 : same result











