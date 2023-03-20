##Script papier Kapsenberg

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

#import data (0m et 50m)

data_raw <- read_csv("data_pH_steeve.csv")
data_raw_surf <- data_raw %>% filter(depth==0)
data_raw_prof <- data_raw %>% filter(depth==50)

data_0715 <- data_raw_surf %>% filter(sampling_date < "2016-01-05") #470 valeurs

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

#periode 2007-2015 (Kapsenberg)
# Calculate anomalies
# Surface
# Calculate monthly means


monthly_means_0m_0715 <- ungroup(data_0715) %>% 
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
ano_0m_0715 <- left_join(ungroup(data_0715), monthly_means_0m_0715, by = "monthd") %>%
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
     lm.test[[i]] <- lm(reformulate(var_list[i], "sampling_date"), data = ano_0m_0715)
 }


lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), 
             data = ano_0m_0715))
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
slope_temp <- reg_ano_surf[2, 1]

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
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), 
             data = ano_0m_0715))
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
slope_temp <- reg_ano_surf[2, 1]
slope_pH <- reg_ano_surf[7, 1]



# Plot anomalies
#Anomalies 0 m
plot_ano_temp <- ggplot(data = ano_0m_0715, aes(x = sampling_date, y = Temperature_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Temp. (°C)") +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0715$Temperature_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_sal <- ggplot(data = ano_0m_0715, aes(x = sampling_date, y = Salinity_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  scale_y_continuous(breaks=c(-2,-1,0,0.5)) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Salinity") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_ph <- ggplot(data = ano_0m_0715, aes(x = sampling_date, y = pH_calc_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL, x="", y=expression(paste(pH[T]," calculated"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0715$pH_calc_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_pCO2 <- ggplot(data = ano_0m_0715, aes(x = sampling_date, y = pCO2_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="",y=expression(paste(pCO[2], " (",mu, "atm)"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0715$pCO2_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

########################################################################################
#mm technique pour 2007-2022

data_0722 <- data_raw_surf %>% filter(sampling_date < "2022-01-11") #774 valeurs
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

#periode 2007-2022
# Calculate anomalies
# Surface
# Calculate monthly means


monthly_means_0m_0722 <- ungroup(data_0722) %>% 
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
ano_0m_0722 <- left_join(ungroup(data_0722), monthly_means_0m_0722, by = "monthd") %>%
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
  lm.test2[[i]] <- lm(reformulate(var_list2[i], "sampling_date"), data = ano_0m_0722)
}

#cutoff_date <- "2016-01-01" #to limit regressions on complete years

lms2 <- lapply(var_list2, function(x) {
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), 
             data = ano_0m_0722))
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
reg_ano_surf2 <- reg2
slope_temp2 <- reg_ano_surf2[2, 1]

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
  summary(lm(substitute(i ~ decimal_date(sampling_date), list(i = as.name(x))), data = ano_0m_0722))
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
reg_ano_surf2 <- reg2
slope_temp2 <- reg_ano_surf2[2, 1]
slope_pH2 <- reg_ano_surf2[7, 1]



# Plot anomalies
#Anomalies 0 m
plot_ano_temp2 <- ggplot(data = ano_0m_0722, aes(x = sampling_date, y = Temperature_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Temp. (°C)") +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0722$Temperature_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_sal2 <- ggplot(data = ano_0m_0722, aes(x = sampling_date, y = Salinity_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  scale_y_continuous(breaks=c(-2,-1,0,0.5)) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="", y="Salinity") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_ph2 <- ggplot(data = ano_0m_0722, aes(x = sampling_date, y = pH_calc_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL, x="", y=expression(paste(pH[T]," calculated"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0722$pH_calc_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())
##

plot_ano_pCO22 <- ggplot(data = ano_0m_0722, aes(x = sampling_date, y = pCO2_ano), na.rm=TRUE) +
  scale_x_datetime(date_breaks="4 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="blue", na.rm=TRUE, size=1) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title=NULL,x="",y=expression(paste(pCO[2], " (",mu, "atm)"))) +
  annotate("text", x =as.POSIXct("2016-01-01"), y= min(ano_0m_0722$pCO2_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())


