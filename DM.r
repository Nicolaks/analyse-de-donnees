# DM Analyse de données

# LEPETIT Lucie 21203742
# AUBRY Nicolas 21603763
# MAURAND Pierre 21702704
# GALLIS Robin 21700872

library(lubridate) # pour gestion des dates
library(dplyr) #pour select notamment
library(fastICA) #pour ACI


base = read.csv2("C:/Users/nico1/OneDrive/Bureau/Master/Semestre 2/Analyse de données/DM/base.csv", header = TRUE, sep = ";", dec=",")

#Pour pouvoir gérer la date.
base$date=ymd(base$date)


### Comparaison de la température de l'eau et de l'air

plot(base$date, base$Tair.EOBS, type = "p", col = "orange", xlab = "Date (en année)", ylab = "Température en °C", main="Comparaison de la température de l'eau et de l'air \n dans le temps") 
points(base$date, base$Teau, type = "p", col = "blue")
legend(x= "topleft", legend=c("Température de l'eau", "Température de l'air"),
       col=c("blue", "orange"),pch = 16, cex=0.8)

###

### Tableau des températures en fonction des saisons.

base$annee = year(base$date)
base$mois = month(base$date)
base$jour = day(base$date)

base$saison = "hiver"
base$saison[base$mois>=4 & base$mois<=6]="printemps"
base$saison[base$mois>=7 & base$mois<=9]="été"
base$saison[base$mois>=9 & base$mois<=12]="automne"

plot(base$Teau[base$saison=="été"] ~ base$Tair.EOBS[base$saison=="été"], col="orangered", xlab = "Température de l'air (en °C)", ylab="Température de l'eau (en °C)", main="Température de l'eau en fonction de l'air")
points(base$Teau[base$saison=="printemps"] ~ base$Tair.EOBS[base$saison=="printemps"], col="palegreen4")
points(base$Teau[base$saison=="automne"] ~ base$Tair.EOBS[base$saison=="automne"], col="sandybrown")
points(base$Teau[base$saison=="hiver"] ~ base$Tair.EOBS[base$saison=="hiver"], col="royalblue3")
legend(x= "topleft", legend=c("été", "printemps", "automne", "hiver"),
       col=c("orangered", "palegreen4", "sandybrown", "royalblue3"),pch = 16, cex=0.8)

###

### ACI


library(lubridate) # pour gestion des dates
library(dplyr) #pour select notamment
library(fastICA) #pour ACI


X<-read.csv2("C:/Users/nico1/OneDrive/Bureau/Master/Semestre 2/Analyse de données/DM/base.csv", header = TRUE, sep = ";", dec=",")


X$date=ymd(X$date)

Od1a<-filter(X, id_sonde=="825")
Od1aa<-aggregate(Teau~date,data=Od1a, FUN = mean, na.rm=T)
Od1aa$Teau825=Od1aa$Teau
plot(Teau825~date, data=Od1aa, type="l", col="blue")

Od2a<-filter(X, id_sonde=="827")
Od2aa<-aggregate(Teau~date,data=Od2a, FUN = mean, na.rm=T)
Od2aa$Teau827=Od2aa$Teau
plot(Teau827~date, data=Od2aa, type="l", col="blue")

Od3a<-filter(X, id_sonde=="828")
Od3aa<-aggregate(Teau~date,data=Od3a, FUN = mean, na.rm=T)
Od3aa$Teau828=Od3aa$Teau
plot(Teau828~date, data=Od3aa, type="l", col="blue")

Od4a<-filter(X, id_sonde=="830")
Od4aa<-aggregate(Teau~date,data=Od4a, FUN = mean, na.rm=T)
Od4aa$Teau830=Od4aa$Teau
plot(Teau830~date, data=Od4aa, type="l", col="blue")

plot(Teau825~date, type="l", main="Températures", data=Od1aa, col="blue", ylab="température")
lines (Teau827~date, type="l", data=Od2aa, col="black")
lines (Teau828~date, type="l", data=Od3aa, col="red")
lines (Teau830~date, type="l", data=Od4aa, col="green")
legend("topleft", legend=c("825", "827", "828","830"),
       col=c("blue", "black", "red","green"), lty=1:2, cex=0.8)


base4<-merge(Od1aa,Od2aa, by="date") 
base5<-merge(base4, Od3aa, by="date")
base9<-merge(base5, Od4aa, by="date")
base6<-select(base9, date,Teau825, Teau827, Teau828,Teau830)

base8<-select(base6,-date)
base7<-select(base6, date)

summary(base8)


set.seed(1)
a <- fastICA(base8, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)



a$A #Aucune valeur n'est négative, donc on peut interpréter les résultats sans changements.
a$S 
A<-data.frame(a$S) 

B<-cbind(base7, A) 


B$comp1=a$A[1,1]*a$S[,1]
B$comp2=a$A[2,1]*a$S[,2]

plot(comp1~date, type="l", data=B, col="blue", main="Représentation des 2 composantes de l'ACI") # Réchauffe le cours d'eau l'été et le refroidit l'hiver. 
lines(comp2~date, type="l", data=B, col="red") # Réchauffe le cours d'eau l'hiver et le refroidit l'été, c'est peut être une nappe.
legend("topleft", legend=c("Comp1","Comp2"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

B$Z=B$comp1+B$comp2+mean(base6$Teau825)

B$diff=B$Z-base6$Teau825
summary(B$diff)


###

### ACP

library(lubridate)
library(questionr)
library(data.table)
library(dplyr)
library(tidyverse)
library(fastICA)
library(FactoMineR)
library(factoextra)
library(EnvStats)

base = read.csv2("C:/Users/nico1/OneDrive/Bureau/Master/Semestre 2/Analyse de données/DM/base.csv", header = TRUE, sep = ";", dec=",")

base$date=ymd(base$date)
base<-filter(base, id_sonde=="825")

piezo = read.csv2("C:/Users/nico1/OneDrive/Bureau/Master/Semestre 2/Analyse de données/DM/PiezoLaFerriereHarang.csv", header = TRUE, sep = ";", dec=",")

piezo$date=ymd(piezo$date)#on met au format date

base<-merge(base,piezo, by="date")



comp1<-aggregate(comp1~date, data=B, FUN=mean, na.rm=TRUE)
comp2<-aggregate(comp2~date, data=B, FUN=mean, na.rm=TRUE)


base2<-merge(comp1, comp2, by="date")

base = merge(base, base2, by="date")

# Renomme les variables

base<-mutate(base,C2=comp2, Ta=Tair.EOBS, Tw=Teau, C1=comp1,PE=Rainf.EOBS, Precip = P)



baseACP = select(base, Tw,Ta, C1, C2, Precip,PE)

res.pca=PCA(baseACP, , quanti.sup=5:6)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

### Test des corrélations

cor.test(base$Teau, base$comp1) # Une forte corrélation
cor.test(base$Teau, base$comp2) # Une corrélation non négligeable 
cor.test(base$Teau, base$Tair.EOBS) # Une forte corrélation
cor.test(base$Rainf.EOBS, base$Teau) # Une corrélation très faible
cor.test(base$Rainf.EOBS, base$P) # Une petite corrélation

###


# Valeurs propres
res.pca$eig
# Les valeurs propres de comp1 et comp2 sont > 1 (règles du cours), donc on les sélectionnes.

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))
#Les deux premières dimensions sont suffisantes pour réprésenter la majorité des données (environ 98%)


res.var <- res.pca$var
res.var$coord          # Coordonnées
res.var$contrib        # Contributions aux axes
res.var$cos2           # Qualité de représentation
#Un cos2 élevé indique une bonne représentation de la variable sur les axes principaux 


# Résultats des individus
res.ind <- res.pca$var
res.ind$coord          # Coordonnées
res.ind$contrib        # Contributions aux axes
res.ind$cos2           # Qualité de représentation

###
