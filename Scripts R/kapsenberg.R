#Libraries
library("readr")
library("viridis")
library("scales")
library("lubridate")
library("gsignal")
library("FactoMineR")
library("factoextra")
library("gplots")
library("heatwaveR")
library("dplyr")
library("imputeTS")
library("Metrics")
library("tidyr")
library("tseries")
library("forecast")
library("tsDyn")
library("tsoutliers")
library("seacarb")
library("lmodel2")
library("lmtest")

##

#import data

data_steeve <- read_csv("data_pH_steeve.csv")
data_steeve <- data_steeve %>% filter(depth==0)
data_steeve <- data_steeve %>% filter(sampling_date < "2016-01-05") #470 valeurs
##
Mytheme <- function(size_labs = 6, face_font="plain", ...) {
  theme_bw() +
    theme(axis.text.x = element_text(face=face_font, size=size_labs, color="black"),
          axis.title.x = element_text(face=face_font, size=size_labs, margin=margin(0,0,0,0,"pt")),
          axis.text.y = element_text(face=face_font, color="black", size=size_labs),
          axis.title.y = element_text(face=face_font, size=size_labs),
          axis.ticks.x = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.1),
          axis.ticks.length = unit(1.1, "mm"),
          panel.grid.major = element_line(size = 0.25, color="black", linetype="dotted"),
          aspect.ratio = 1 / 3,
          plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")
    )
}
##
#essai script Kapsenberg

# Calculate anomalies
# Surface
# Calculate monthly means


tmp2 <- ungroup(data_steeve) %>% 
  group_by(monthd) %>%
  summarise(
    Salinity_month = mean(salinity, na.rm = TRUE),
    Temperature_month = mean(temperature, na.rm = TRUE),
    dic_month = mean(dic, na.rm = TRUE),
    ta_month = mean(ta, na.rm = TRUE),
    pH_calc_month = mean(pH_calc, na.rm = TRUE),
    pH18_month = mean(pH18, na.rm = TRUE),
    pCO2_month = mean(pCO2, na.rm = TRUE),
    OmegaCalcite_month = mean(OmegaCalcite, na.rm = TRUE),
    OmegaAragonite_month = mean(OmegaAragonite, na.rm = TRUE),
    Nta_month = mean(Nta, na.rm = TRUE),
    Ndic_month = mean(dic, na.rm = TRUE))

# Calculate anomalies
ano <- left_join(ungroup(data_steeve), tmp2, by = "monthd") %>%
  mutate(
    Salinity_ano = salinity - Salinity_month,
    Temperature_ano = temperature - Temperature_month,
    dic_ano = dic - dic_month,
    ta_ano = ta - ta_month,
    pH_calc_ano = pH_calc - pH_calc_month,
    pH18_ano = pH18 - pH18_month,
    pCO2_ano = pCO2 - pCO2_month,
    OmegaCalcite_ano = OmegaCalcite - OmegaCalcite_month,
    OmegaAragonite_ano = OmegaAragonite - OmegaAragonite_month,
    Nta_ano = Nta - Nta_month,
    Ndic_ano = dic - Ndic_month)

# Regressions and tables of key parameters
var_list <-
  c("salinity",
    "temperature",
    "dic",
    "ta",
    "pH_calc",
    "pH18",
    "pCO2",
    "OmegaCalcite",
    "OmegaAragonite",
    "Nta",
    "Ndic")

 lm.test <- vector("list", length(var_list))
 
 for(i in seq_along(var_list)){
     lm.test[[i]] <- lm(reformulate(var_list[i], "sampling_date"), data = ano)
 }

#cutoff_date <- "2016-01-01" #to limit regressions on complete years

lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), 
             data = ano))
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
reg_ano_1m <- reg
slope_temp <- reg_ano_1m[2, 1]

# Regression and table anomalies
var_list <-
  c(
    "Salinity_ano",
    "Temperature_ano",
    "dic_ano",
    "ta_ano",
    "pH_calc_ano",
    "pH18_ano",
    "pCO2_ano",
    "OmegaCalcite_ano",
    "OmegaAragonite_ano",
    "Nta_ano",
    "Ndic_ano"
  )
lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), data = ano))
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
reg_ano_1m <- reg
slope_temp <- reg_ano_1m[2, 1]
slope_pH <- reg_ano_1m[7, 1]



# Plot anomalies
#Anomalies 0 m
plot_temp <- ggplot(data = ano, aes(x = sampling_date, y = Temperature_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Temp. (°C)") +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano$Temperature_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_sal <- ggplot(data = ano, aes(x = sampling_date, y = Salinity_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  scale_y_continuous(breaks=c(-2,-1,0,0.5)) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Salinity") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ph <- ggplot(data = ano, aes(x = sampling_date, y = pH_calc_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL, x="", y=expression(paste(pH[T]," calculated"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano$pH_calc_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_pCO2 <- ggplot(data = ano, aes(x = sampling_date, y = pCO2_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="",y=expression(paste(pCO[2], " (",mu, "atm)"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano$pCO2_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

########################################################################################
#mm technique pour 2007-2022

data_steeve2 <- data_steeve %>% filter(sampling_date < "2022-01-11") #774 valeurs
##
Mytheme <- function(size_labs = 6, face_font="plain", ...) {
  theme_bw() +
    theme(axis.text.x = element_text(face=face_font, size=size_labs, color="black"),
          axis.title.x = element_text(face=face_font, size=size_labs, margin=margin(0,0,0,0,"pt")),
          axis.text.y = element_text(face=face_font, color="black", size=size_labs),
          axis.title.y = element_text(face=face_font, size=size_labs),
          axis.ticks.x = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.1),
          axis.ticks.length = unit(1.1, "mm"),
          panel.grid.major = element_line(size = 0.25, color="black", linetype="dotted"),
          aspect.ratio = 1 / 3,
          plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")
    )
}
##
#essai script Kapsenberg

# Calculate anomalies
# Surface
# Calculate monthly means


tmp3 <- ungroup(data_steeve2) %>% 
  group_by(monthd) %>%
  summarise(
    Salinity_month = mean(salinity, na.rm = TRUE),
    Temperature_month = mean(temperature, na.rm = TRUE),
    dic_month = mean(dic, na.rm = TRUE),
    ta_month = mean(ta, na.rm = TRUE),
    pH_calc_month = mean(pH_calc, na.rm = TRUE),
    pH18_month = mean(pH18, na.rm = TRUE),
    pCO2_month = mean(pCO2, na.rm = TRUE),
    OmegaCalcite_month = mean(OmegaCalcite, na.rm = TRUE),
    OmegaAragonite_month = mean(OmegaAragonite, na.rm = TRUE),
    Nta_month = mean(Nta, na.rm = TRUE),
    Ndic_month = mean(dic, na.rm = TRUE))

# Calculate anomalies
ano2 <- left_join(ungroup(data_steeve2), tmp3, by = "monthd") %>%
  mutate(
    Salinity_ano = salinity - Salinity_month,
    Temperature_ano = temperature - Temperature_month,
    dic_ano = dic - dic_month,
    ta_ano = ta - ta_month,
    pH_calc_ano = pH_calc - pH_calc_month,
    pH18_ano = pH18 - pH18_month,
    pCO2_ano = pCO2 - pCO2_month,
    OmegaCalcite_ano = OmegaCalcite - OmegaCalcite_month,
    OmegaAragonite_ano = OmegaAragonite - OmegaAragonite_month,
    Nta_ano = Nta - Nta_month,
    Ndic_ano = dic - Ndic_month)

# Regressions and tables of key parameters
var_list2 <-
  c("salinity",
    "temperature",
    "dic",
    "ta",
    "pH_calc",
    "pH18",
    "pCO2",
    "OmegaCalcite",
    "OmegaAragonite",
    "Nta",
    "Ndic")

lm.test2 <- vector("list", length(var_list2))

for(i in seq_along(var_list2)){
  lm.test2[[i]] <- lm(reformulate(var_list2[i], "sampling_date"), data = ano2)
}

#cutoff_date <- "2016-01-01" #to limit regressions on complete years

lms2 <- lapply(var_list2, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), 
             data = ano2))
})

reg2 <- NULL
for (i in 1:length(var_list2)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob2 <-
    pf(lms2[[i]]$fstatistic[1],
       lms2[[i]]$fstatistic[2],
       lms2[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg2 <-
    rbind(reg2, as.numeric(
      c(
        lms2[[i]]$coefficients[2, 1],
        lms2[[i]]$coefficients[2, 2],
        lms2[[i]]$coefficients[2, 4],
        lms2[[i]]$coefficients[1, 1],
        lms2[[i]]$coefficients[1, 2],
        lms2[[i]]$coefficients[1, 4],
        lms2[[i]]$fstatistic[1],
        lms2[[i]]$fstatistic[3],
        lms2[[i]]$r.squared,
        prob2
      )
    ))
}

colnames(reg2) <-
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
row.names(reg2) <- var_list2
reg_ano_1m2 <- reg2
slope_temp2 <- reg_ano_1m2[2, 1]

# Regression and table anomalies
var_list2 <-
  c(
    "Salinity_ano",
    "Temperature_ano",
    "dic_ano",
    "ta_ano",
    "pH_calc_ano",
    "pH18_ano",
    "pCO2_ano",
    "OmegaCalcite_ano",
    "OmegaAragonite_ano",
    "Nta_ano",
    "Ndic_ano"
  )
lms2 <- lapply(var_list2, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), data = ano2))
})
reg2 <- NULL
for (i in 1:length(var_list2)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob2 <-
    pf(lms2[[i]]$fstatistic[1],
       lms2[[i]]$fstatistic[2],
       lms2[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg2 <-
    rbind(reg2, as.numeric(
      c(
        lms2[[i]]$coefficients[2, 1],
        lms2[[i]]$coefficients[2, 2],
        lms2[[i]]$coefficients[2, 4],
        lms2[[i]]$coefficients[1, 1],
        lms2[[i]]$coefficients[1, 2],
        lms2[[i]]$coefficients[1, 4],
        lms2[[i]]$fstatistic[1],
        lms2[[i]]$fstatistic[3],
        lms2[[i]]$r.squared,
        prob2
      )
    ))
}
colnames(reg2) <-
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
row.names(reg2) <- var_list2
reg_ano_1m2 <- reg2
slope_temp2 <- reg_ano_1m2[2, 1]
slope_pH2 <- reg_ano_1m2[7, 1]



# Plot anomalies
#Anomalies 0 m
plot_temp2 <- ggplot(data = ano2, aes(x = sampling_date, y = Temperature_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Temp. (°C)") +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano2$Temperature_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_sal2 <- ggplot(data = ano2, aes(x = sampling_date, y = Salinity_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  scale_y_continuous(breaks=c(-2,-1,0,0.5)) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Salinity") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ph2 <- ggplot(data = ano2, aes(x = sampling_date, y = pH_calc_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL, x="", y=expression(paste(pH[T]," calculated"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano2$pH_calc_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_pCO22 <- ggplot(data = ano2, aes(x = sampling_date, y = pCO2_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="",y=expression(paste(pCO[2], " (",mu, "atm)"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano2$pCO2_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
