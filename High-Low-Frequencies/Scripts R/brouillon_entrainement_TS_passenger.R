help("AirPassengers")
data <- AirPassengers
plot(data)
data=ts(data, frequency=12, start=c(1949,1))
plot.ts(data)
component <- decompose(data)
plot(component$trend)
plot(data)
lines(component$trend, col='red')

#moyenne glissante avec 3 valeurs (k)
k <- 3
filter <- filter(data, rep(1/k,k)) #calcul de la moyenne glissante)
lines(filter, col='red')



#peut permettre de réduire le bruit sur un jeu de données
#car moyenne glissante fait abstraction des valeurs abbérantes
#équivaut à lisser les données

#série = tendance + composante périodique + résidus

#détermination de la composante périodique : autocorrélation (lags k)
#si une série est périodique : coef d'autocorrélation est maximal quand on à décalé la série d'une période

#coef d'autocorrélation : r(k) = s(k)/s(0)
#avec s(0) = variance
#et s(k) = autocovariance ou (autocorrelation) au lag k

#estimation de la tendance
#série = tendance + composante périodique + résidus
#donc tendance + résidus = série - composante périodique
#donc tendance + résidus ~ série lissée

#calcul autocorelation avec acf(), obtention d'un corrélogramme
result_acf <- acf(data, type="correlation")
#ligne pointillés bleus : seuil critique au-delà duquel l'autocorrélation est considérée significative
#on voit qu'il y a une remontée au 12e pic

#affichage manuel des 1er coef d'autocorrélation pour vérification
print(data.frame(result_acf$lag,result_acf$acf)[0:20,]) #pour afficher les 10 premiers
#les lag correspondent à 1/12, 2/12 etc.. donc 1 = 1an
#on confirme la remontée du coef à partir du 10e lag, soit au bout d'1an

coef <- c(result_acf$acf [0:20])
plot(coef, type='l', xlim=c(0,20))
abline(v=13, legend(14,0.9,"k=12")) #donc k = 13, mais la série commence à 1 donc k = 12
#donc série périodique avec T = 12 (mois)

#donc estimation de la tendance avec une moyenne glissante sur 12 intervalles
trend <- filter(data, rep(1/12,12))
plot(data)
lines(trend, col='red')

