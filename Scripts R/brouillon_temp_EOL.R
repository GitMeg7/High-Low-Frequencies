### Lecture données EOL (T, sal, Ox ; 2013:2022)
library("tidyverse")
data_EOL <- read_csv("SEANOE_data_EOL_corrig.csv")
cols(
  id = col_double(),
  Date = col_date(format=""),
  Temp = col_double(),
  Sal = col_double(),
  Oxy = col_double()
)
glimpse(data_EOL)

#visualisation des données de température
data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = temp_eol_c) +
  geom_line()
Temp <- data_EOL %>% dplyr::select(-...1, -sal_eol_psu, -oxy_eol_mll)

#Pour filtrer les données sur une période de temps :
data_EOL %>%
  filter(datetime >= ymd_hms("2020-01-01 01:00:00")) %>% 
  ggplot(aes(x = datetime, y = temp_eol_c)) +
  geom_line()

#Tendance
data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = temp_eol_c) +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 20))


## autocorrelation manuel
y <- Temp$temp_eol_c
n <- length(Temp$temp_eol_c)
## calcul de r(0), r(1) et r(2)
r0 <- var(y, y) / var(y, y)
r1 <- var(y[2:n], y[1:(n-1)]) / var(y, y)
r2 <- var(y[3:n], y[1:(n-2)]) / var(y, y)
r3 <- var(y[4:n], y[1:(n-3)]) / var(y, y)
r4 <- var(y[5:n], y[1:(n-4)]) / var(y, y)
r5 <- var(y[6:n], y[1:(n-5)]) / var(y, y)
r6 <- var(y[7:n], y[1:(n-6)]) / var(y, y)
r7 <- var(y[8:n], y[1:(n-7)]) / var(y, y)
r8 <- var(y[9:n], y[1:(n-8)]) / var(y, y)

lag <- 0:8
autocorr <- c(r0, r1, r2, r3, r4, r5, r6, r7, r8)
## graphique
plot(lag, autocorr, type="l")


#autocorrelation acf() - même chose

result_acf <- acf(Temp$temp_eol_c, type="correlation", plot=TRUE)
print(data.frame(result_acf$lag,result_acf$acf)[0:20,])
coef <- c(result_acf$acf [0:20])
plot(coef, type='l', xlim=c(0,20))
#lag 0 donc k=0 est 

