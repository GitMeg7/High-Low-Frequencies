#exemple 1
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
print(kings)
s1 <- ts(kings)
print(s1)
plot(s1)

#exemple2
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
s2 <- ts(births, frequency=12, start=c(1946,1)) #frequency=12 car 12mois
#start : ST démarre en 1946, +1 an à chaque fois
plot.ts(s2)
#On distingue une composante tendancielle : une baisse au début 
#(entre 1946 et 194) puis une augmentation régulière jusqu'à la fin 
#on distingue aussi une composante saisonnière :
#certains mois il y a en moyenne moins de naissances (cyclique)

#les ST peuvent se décomposer en 3 composantes :
#composante tendancielle,
#composante saisonnière,
#résidus (diff entre la série observée et les 2 premières composantes)


#Pour prédire une série temporelle : identifier les composantes
#Prédictions sur les 2 premières composantes :
s2components <- decompose(s2)
plot(s2components) #random = les résidus
#Si on s'intéresse simultanément à la tendance et au terme résiduel
#rettrancher la composante saisonnière à la série observée : 
s2seasonallyadjusted <- s2 - s2components$seasonal
plot.ts(s2seasonallyadjusted)
