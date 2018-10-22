library(dplyr)
library(tidyverse)
######################################################################################################################
#FONCTIONS DE TRAITEMENT DU FICHIER Activité / Communes
#fonction de modification de la précision des champs
#et changement de nom de la colonne
#ajout de la nouvelle colonne transformée
#suppression de l'ancienne colonne
precision <- function(data,nomcol,newcol) {
  nomcol
  newcol
  data[,c(nomcol)]
  if (is.numeric(data[,c(nomcol)]))
  {
    #creation de la colonne
    msg <- paste("Creation de colonne",as.character(newcol))
    print(msg)
    data[,c(newcol)] = as.integer(data[,c(nomcol)])
    msg <- paste("Suppression de la colonne",as.character(nomcol))
    print(msg)
    data <- data[,colnames(data)!=nomcol]
  }
  return(data)
}

#Fonction de suppression d'une colonne par son indice
delColByNum <- function(data,colnum) {
  col <-colnames(actCom)
  msg <- paste("Suppression de la colonne",col[[colnum]])
  print(msg)
  data <- data[,-colnum]
  #data <- data[,-as.integer(colnum)]
}
#actCom <-delColByNum(actCom,2)

#Fonction de suppression d'une colonne par son nom  
# actCom <- delColByName(actCom,colonnes[[3]])
# actCom <- delColByName(actCom,"Code")
# actCom <- delColByName(actCom,"communes")
delColByName <- function(data,nomcol) {
  msg <- paste("Suppression de la colonne", nomcol)
  print(msg)
  data <- data[,colnames(data)!=nomcol]
}

#FONCTION d'HORODATAGE 
#on remplace chaque colonne par date / population
horodateColonne <- function(data,nomCol,annee,newcolonne) {
  compopannee<-select(data,codeCommune,nomCol)
  head(compopannee)
  nbrow <- nrow(compopannee)
  nomcol<-c("codeCommune",newcolonne,"date")
  dateToAdd<-rep(annee,nbrow)
  data<-cbind(compopannee,dateToAdd)
  colnames(data)<-nomcol
  return (data)
}
# horodateColonne <- function(data,nomCol,annee,newcolonne) {
#   compopannee<-select(data,codeCommune,nomCol)
#   head(compopannee)
#   nbrow <- nrow(compopannee)
#   nomcol<-c("codeCommune",newcolonne,"date")
#   dateToAdd<-rep(annee,nbrow)
#   popnew<-cbind(compopannee,dateToAdd)
#   colnames(popnew)<-nomcol
#   return (popnew)
# }
#Cas de la colonne population 2014
# compop2014<-select(comPop,codeCommune,p14_pop)
# nbrow <- nrow(compop2014)
# nomcol<-c("codeCommune","population","date")
# date2014<-rep(2014,nbrow)
# popnew<-cbind(compop2014,date2014)
# colnames(popnew)<-nomcol
#ajout d'une colonne Date à l'ensemble du data frame
horodateData <- function(data,annee) {
  nbrow <- nrow(data)
  date<-rep(annee,nbrow)
  data<-cbind(data,date)
  return (data)
}
#Ajout du referenciel [commune] ou epci
# epciref<-read.csv2("F:/Flexgrid/TalendREFERENTIEL/20180309epci.csv",encoding = "UTF-8")
# epcisoc<-read.csv2("F:/Flexgrid/ObservatoirePACA/ExportIndicateursObsRegional2016/EPCI/ExportSocialEPCI2016.csv",encoding="UTF-8")
# Ajout du referentiel EPCI au fichier soc
# epcisoc <- left_join(epcisoc, epciref, by=c("codeEPCI"))
addRef <- function(data,dataref,colRef) {
  data <- left_join(data,dataref,by=c(colRef))
}
# epcisoc <- addRefEpci(epcisoc, epciref, "codeEPCI")
# epcisoc <- delColByName(epcisoc,"epci2016")
#write.csv2(comEnergie, file="F:/Flexgrid/Sprint1/communeEnergie.csv", row.names=FALSE,fileEncoding = "UTF-8")
##################################################################################################
#### DONNEES AIR PACA
##################################################################################################
# DONNEES de production
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airpacaProd <- read.csv2("dumpProduction.csv", dec= ".", sep=",",encoding = "UTF-8")
#type factor => numeric
#replace by reading with dec='.' 
#airPacaConso$production<-gsub(".", ",", airPacaConso$production, fixed=TRUE)
summary(airpacaProd)
levels(airpacaProd$detail_filiere_cigale)
colnames(airpacaProd)
#changement de nom des colonnes
airpacaProd <- rename(airpacaProd,codeCommune = id_comm)
airpacaProd <- rename(airpacaProd,grandeFiliere = grande_filiere_cigale)
airpacaProd <- rename(airpacaProd,detailFiliere = detail_filiere_cigale)
airpacaProd <- rename(airpacaProd,productionMwh = val)
airpacaProd <- rename(airpacaProd,annee = an)
airpacaProd <- rename(airpacaProd,typeProduction = lib_type_prod)
#airpacaProd <- rename(airpacaProd,unite = id_unite)
airpacaProd$id_unite <- NULL
airpacaProd$siren_epci_2017 <- NULL
airpacaProd$color_grande_filiere_cigale <- NULL
airpacaProd$color_detail_filiere_cigale <- NULL
airpacaProd$id_grande_filiere_cigale <- NULL
airpacaProd$id_detail_filiere_cigale <- NULL
airpacaProd$nom_epci_2017 <- NULL
airpacaProd$dep <- NULL
airpacaProd$id_type_prod <- NULL
head(airpacaProd)
str(airpacaProd$production)
#type factor => numeric
#replace by reading with dec='.' 
#airpacaProd$production<-gsub(".", ",", airpacaProd$production, fixed=TRUE)
#changement de la pecision MWh avec 4 chiffres après la virgule
airpacaProd <- mutate(airpacaProd, productionMwh = round(productionMwh,4))
#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
airpacaProd <- addRef(airpacaProd, comRef, "codeCommune")
head(airpacaProd)
#ecrire le fichier traite
#write.csv2(airpacaProd, file="Traite/airPacaProduction.csv", row.names=FALSE,encoding = "UTF-8")
write.csv2(airpacaProd, file="F:/Flexgrid/Sprint1/airPacaProduction.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
# 26 avril 2018
# agregation des données par EPCI => valeur EPCI
# selection des données de l'année 2015
prod2015 <- filter(airpacaProd, annee==2015)
prod2015 <- select(prod2015, c(codeCommune,typeProduction,grandeFiliere,detailFiliere,productionMwh,codeEPCI,libelleEPCI))
epciProd2015 <- group_by(prod2015,codeEPCI,detailFiliere)
perEpci <- summarize(epciProd2015,productionMwh=sum(productionMwh))
##### AUTRE ECRITURE ###################
#tab <- prod2015
#tab2015 <- tab %>% group_by(codeEPCI,detailFiliere) %>% summarise(production=sum(productionMwh))
######################################################################
levels(epciProd2015$detailFiliere)
# biomasseEpci <- filter(perEpci,detailFiliere=="Biomasse")
# biomasseEpci <- select(biomasseEpci,c(codeEPCI,productionMwh))
# colnames(biomasseEpci) <- c("codeEPCI","prodBiomasse")
# biogazEpci <- filter(perEpci,detailFiliere=="Biogaz")
# biogazEpci <- select(biogazEpci,c(codeEPCI,productionMwh))
# colnames(biogazEpci) <- c("codeEPCI","prodBiogaz")
# laprodEpci <- left_join(biomasseEpci,biogazEpci,by=c("codeEPCI"))
# #Solaire thermique
# solthEpci <- filter(perEpci,detailFiliere=="Solaire thermique")
# solthEpci <- select(solthEpci,c(codeEPCI,productionMwh))
# colnames(solthEpci) <- c("codeEPCI","prodSolTh")
# laprodEpci <- left_join(laprodEpci,solthEpci,by=c("codeEPCI"))
#Transformer les lignes en colonnes
table(epciProd2015$detailFiliere)
addFiliereProduction <- function(data,filiere,nomcol) {
  msg <- paste(filiere,nomcol)
  print(msg)
  maprod <- filter(data, detailFiliere==filiere)
  maprod <- select(maprod,c(codeEPCI,productionMwh))
  colnames(maprod) <- c("codeEPCI",nomcol)
  return(maprod)
}
#detailFiliere
# Cogénération, solaire thermique, Biomasse, Grande hydraulique, Petite hydraulique, Solaire photovoltaïque
laprodEpci <- addFiliereProduction(perEpci,"Biomasse","prodBiomasse") 
newProd0 <- addFiliereProduction(perEpci,"Grande hydraulique","prodGdeHydrau") 
laprodEpci <- left_join(laprodEpci,newProd0,by=c("codeEPCI"))
newProd1 <- addFiliereProduction(perEpci,"Petite hydraulique","prodPteHydrau") 
laprodEpci <- left_join(laprodEpci,newProd1,by=c("codeEPCI"))
newProd2 <- addFiliereProduction(perEpci,"Solaire thermiques","prodSolTher") 
laprodEpci <- left_join(laprodEpci,newProd2,by=c("codeEPCI"))
newProd3 <- addFiliereProduction(perEpci,"Solaire photovoltaïque","prodSolairePV") 
laprodEpci <- left_join(laprodEpci,newProd3,by=c("codeEPCI"))
newProd4 <- addFiliereProduction(perEpci,"Cogénération","prodCoge") 
laprodEpci <- left_join(laprodEpci,newProd4,by=c("codeEPCI"))
#Sauvegarde des données de production par Epci
write.csv2(laprodEpci, file="F:/Flexgrid/Sprint2/epciProduction.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
# DONNEES de consommation
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaConsommation <- read.csv2("dumpConsommation.csv", dec= ".", sep=",",encoding = "UTF-8")
airPacaConso <-select(airPacaConsommation,c("an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaConso)
###################################################################
#TRAITEMENT DES VARIABLES FACTOR : secten1, catégorie d'energie
##################################################################
#Mettre ces variables numériques => factor et recoder les modalites
#id_secten1 : 8 modalités 
#1:Energie, 2:Industrie/déchets, 3: Résidentiel, 4: Tertiaire
#5:Agriculture, 6:Transports routiers 7: Autres transports, 8: Non inclus
table(airPacaConso$id_secten1)
# 1      2      3      4      5      6      7 
# 440  21148  84861 412078  27986  11340   2337
airPacaConso$id_secten1 <- as.factor(airPacaConso$id_secten1)
str(airPacaConso$id_secten1)
levels(airPacaConso$id_secten1)<-c("Extraction","Industrie","Résidentiel","Tertiaire","Agriculture","Transport routier","Transports autres")
head(airPacaConso)

#code_cat_energie : 8 modalités 
#1:Gaz Naturel, 2:Produits pétroliers, 3: Combustibles Minéraux Solides, 4: Bois-énergie
#5:Autres EnR, 6:Chaleur et froid 7:Autres non EnR, 8: Electricité
table(airPacaConso$code_cat_energie)
#1      2      3      4      5      6      7      8 
#55290 171307     10  89679  11447    920    170 231367
airPacaConso$code_cat_energie <- as.factor(airPacaConso$code_cat_energie)
str(airPacaConso$code_cat_energie)
levels(airPacaConso$code_cat_energie)<-c("Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Chaleur et froid","Autres non EnR","Electricité")
head(airPacaConso)
#l'unite id_unite=1 pour toutes les lignes cad tep
#changement de la precision MWh avec 4 chiffres après la virgule
airPacaConso <- mutate(airPacaConso, consommationMwh = round((val*11.63),4))
airPacaConso$val <- NULL
#changement de nom des colonnes
colnames(airPacaConso)
airPacaConso <- rename(airPacaConso,codeCommune = id_comm)
airPacaConso <- rename(airPacaConso,activite = id_secten1)
airPacaConso <- rename(airPacaConso,energie = code_cat_energie)
airPacaConso <- rename(airPacaConso,date = an)
airPacaConso <- rename(airPacaConso,ssEpci = ss_epci)
head(airPacaConso)
str(airPacaConso$codeEPCI)

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
airPacaConso <- addRef(airPacaConso, comRef, "codeCommune")
summary(airPacaConso)
#airPacaConso$codeEPCI <- replace(airPacaConso$codeEPCI,is.na(airPacaConso$codeEPCI),0)
head(airPacaConso)

#ecrire le fichier traite
#write.csv2(airPacaConso, file="Traite/airPacaConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8")
write.csv2(airPacaConso, file="F:/Flexgrid/Sprint1/airPacaConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

# PBairPacaConso <-select(airPacaConso, c(codeEPCI,codeCommune))
# PBairPacaConso <-filter(PBairPacaConso, is.na(codeEPCI))
# doublons <- which(duplicated(PBairPacaConso$codeCommune))
# pbcommunes<-PBairPacaConso[-doublons,]
# airPacaConso$codeEPCI <- replace(airPacaConso$codeEPCI,is.na(airPacaConso$codeEPCI),0)
#communes pas dans un EPCI ? 5002, 5005, 5020, 5069, 4100

##################################################################################################
# 27 avril 2018
# agregation des données par EPCI => valeur EPCI
# selection des données de l'année 2015
conso2015 <- filter(airPacaConso, date==2015)
conso2015 <- select(conso2015, c(codeCommune,activite,energie,consommationMwh,codeEPCI,libelleEPCI))
#Consommation par activité et par energie
# epciConso2015 <- group_by(conso2015,codeEPCI,activite,energie)
# perEpci <- summarize(epciConso2015,consommation=sum(consommationMwh))
# table(perEpci$activite)
# table(perEpci$energie)

#On filtre energie=valType et on ajoute les données dans une nouvelle colonne : nomCol
addColEnergieConso <- function(data,valType,nomCol) {
  msg <- paste(valType,nomCol)
  print(msg)
  maconso <- filter(data, energie==valType)
  maconso <- select(maconso,c(codeEPCI,consommation))
  colnames(maconso) <- c("codeEPCI",nomCol)
  return(maconso)
}

#Consommation par energie
epciConso2015Energie <- group_by(conso2015,codeEPCI,energie)
perEpciEnergie <- summarize(epciConso2015Energie,consommation=sum(consommationMwh))
table(perEpciEnergie$energie)
levels(perEpciEnergie$energie)
#[1] "Gaz Naturel"                   "Produits pétroliers"           "Combustibles Minéraux Solides" "Bois-énergie"                 
#[5] "Autres EnR" 
laconsoEpci <- addColEnergieConso(perEpciEnergie,"Produits pétroliers","consoPetrole") 
newConso0 <- addColEnergieConso(perEpciEnergie,"Gaz Naturel","consoGN") 
laconsoEpci <- left_join(laconsoEpci,newConso0,by=c("codeEPCI"))
newConso1 <- addColEnergieConso(perEpciEnergie,"Combustibles Minéraux Solides","consoCombo") 
laconsoEpci <- left_join(laconsoEpci,newConso1,by=c("codeEPCI"))
newConso2 <- addColEnergieConso(perEpciEnergie,"Bois-énergie","consoBois") 
laconsoEpci <- left_join(laconsoEpci,newConso2,by=c("codeEPCI"))
newConso3 <- addColEnergieConso(perEpciEnergie,"Autres EnR","consoAutreEnr") 
laconsoEpci <- left_join(laconsoEpci,newConso3,by=c("codeEPCI"))
newConso4 <- addColEnergieConso(perEpciEnergie,"Chaleur et froid","consoChaleurFroid") 
laconsoEpci <- left_join(laconsoEpci,newConso4,by=c("codeEPCI"))
newConso5 <- addColEnergieConso(perEpciEnergie,"Autres non EnR","consoNonEnr") 
laconsoEpci <- left_join(laconsoEpci,newConso5,by=c("codeEPCI"))
newConso6 <- addColEnergieConso(perEpciEnergie,"Electricité","consoElec") 
laconsoEpci <- left_join(laconsoEpci,newConso6,by=c("codeEPCI"))

#Consommation par activité
epciConso2015Act <- group_by(conso2015,codeEPCI,activite)
perEpciAct <- summarize(epciConso2015Act,consommation=sum(consommationMwh))
table(perEpciAct$activite)
levels(perEpciAct$activite)
#[1] "Extraction"        "Industrie"         "Résidentiel"       "Tertiaire"         "Agriculture"       "Transport routier"
#[7] "Transports autres"
#On filtre activite=valActivite et on ajoute les données dans une nouvelle colonne : nomCol
addColActiviteConso <- function(data,valAct,nomCol) {
  msg <- paste(valAct,nomCol)
  print(msg)
  maconso <- filter(data, activite==valAct)
  maconso <- select(maconso,c(codeEPCI,consommation))
  colnames(maconso) <- c("codeEPCI",nomCol)
  return(maconso)
}
newConso0 <- addColActiviteConso(perEpciAct,"Extraction","consoExtraction") 
laconsoEpci <- left_join(laconsoEpci,newConso0,by=c("codeEPCI"))
newConso1 <- addColActiviteConso(perEpciAct,"Industrie","consoIndustrie") 
laconsoEpci <- left_join(laconsoEpci,newConso1,by=c("codeEPCI"))
newConso2 <- addColActiviteConso(perEpciAct,"Résidentiel","consoRes") 
laconsoEpci <- left_join(laconsoEpci,newConso2,by=c("codeEPCI"))
newConso3 <- addColActiviteConso(perEpciAct,"Tertiaire","consoTertiaire") 
laconsoEpci <- left_join(laconsoEpci,newConso3,by=c("codeEPCI"))
newConso4 <- addColActiviteConso(perEpciAct,"Agriculture","consoAgri") 
laconsoEpci <- left_join(laconsoEpci,newConso4,by=c("codeEPCI"))
newConso5 <- addColActiviteConso(perEpciAct,"Transport routier","consoTR") 
laconsoEpci <- left_join(laconsoEpci,newConso5,by=c("codeEPCI"))
newConso6 <- addColActiviteConso(perEpciAct,"Transports autres","consoTA") 
laconsoEpci <- left_join(laconsoEpci,newConso6,by=c("codeEPCI"))

#Sauvegarde des données de consommation par Epci
write.csv2(laconsoEpci, file="F:/Flexgrid/Sprint2/epciConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
#
#       AIR PACA : GES : CO2, NH4eqCO2, N2OeqCO2, PRG100
#
##################################################################################################
# GES : CO2 - id_polluant 15 unite 21005 cad kg
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES1 <- read.csv2("dumpCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES1)
airPacaGES <-select(airPacaGES1,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : N2O eq CO2 - id_polluant 124
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES2 <- read.csv2("dumpN2OeqCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES2)
airPacaGES <-select(airPacaGES2,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : CH4 eq CO2 - id_polluant 123
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES3 <- read.csv2("dumpCH4eqCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES3)
airPacaGES <-select(airPacaGES3,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : PRG100 - id_polluant 128
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES4 <- read.csv2("dumpPRG100.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES4)
airPacaGES <-select(airPacaGES4,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################)
###################################################################
#TRAITEMENT DES VARIABLES FACTOR : secten1, catégorie d'energie
##################################################################
#Mettre ces variables numériques => factor et recoder les modalites
table(airPacaGES$id_polluant)
airPacaGES$id_polluant <- as.factor(airPacaGES$id_polluant)
#levels(airPacaGES$id_polluant)<-c("CO2")
levels(airPacaGES$id_polluant)<-c("N2OeqCO2")
levels(airPacaGES$id_polluant)<-c("CH4eqCO2")
#levels(airPacaGES$id_polluant)<-c("PRG100")
#id_secten1 : 8 modalités 
#1:Energie, 2:Industrie/déchets, 3: Résidentiel, 4: Tertiaire
#5:Agriculture, 6:Transports routiers 7: Autres transports, 8: Non inclus
#setModalites <- function(airPACAGES) {
table(airPacaGES$id_secten1)
# 1      2      3      4      5      6      7     8
# 253  19623  38955 251842  19246   5670   1319   1359
# 2015  19716  38955 246136  18515   6954   1319   2187 
# 2180  25681 101899 425749  28742   7384   2356   2187 
airPacaGES$id_secten1 <- as.factor(airPacaGES$id_secten1)
str(airPacaGES$id_secten1)
levels(airPacaGES$id_secten1)<-c("Extraction","Industrie","Résidentiel","Tertiaire","Agriculture","Transport routier","Transports autres", "Non inclus")
table(airPacaGES$id_secten1)

#code_cat_energie : 9 modalités 
#0: Aucune Energie 1:Gaz Naturel, 2:Produits pétroliers, 3: Combustibles Minéraux Solides, 
#4: Bois-énergie, 5:Autres EnR, 6:Chaleur et froid 7:Autres non EnR, 8: Electricité
table(airPacaGES$code_cat_energie)
#0      1     2      3      4      5      6      7      8 
#33344  56218 170894  7  89588     67    840    132 231533 #### CO2 de 0 à 8
#40982  56308 170965  8  89595   5808    840    139 231533 #### PRG100 de 0 à 8
#0      1      2      3      4      5      7 
#16980  54957 170891  7  89591     5775   66               #### N2OeqCO2 de 0-1-2-3-4-5-7 
#13223  56252 170897  2  89592     5783   48               #### CH4eqCO2 de 0-1-2-3-4-5-7
airPacaGES$code_cat_energie <- as.factor(airPacaGES$code_cat_energie)
str(airPacaGES$code_cat_energie)
#CO2 de 0 à 8 et PRG100 de 0 à 8
levels(airPacaGES$code_cat_energie)<-c("Aucune énergie", "Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Chaleur et froid","Autres non EnR","Electricité")
# N2OeqCO2 et CH4eqCO2 de 0-1-2-3-4-5-7
levels(airPacaGES$code_cat_energie)<-c("Aucune énergie", "Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Autres non EnR")

#l'unite id_unite=21005 cad kg 
#on laisse en kg
#changement de nom des colonnes
colnames(airPacaGES)
airPacaGES <- rename(airPacaGES,codeCommune = id_comm)
airPacaGES <- rename(airPacaGES,activite = id_secten1)
airPacaGES <- rename(airPacaGES,energie = code_cat_energie)
airPacaGES <- rename(airPacaGES,date = an)
airPacaGES <- rename(airPacaGES,typeGES = id_polluant)
airPacaGES <- rename(airPacaGES,emissionkg = val)
airPacaGES <- rename(airPacaGES,ssEpci = ss_epci)
head(airPacaGES)

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
airPacaGES <- addRef(airPacaGES, comRef, "codeCommune")
summary(airPacaGES)
#VERIFIER qu'il n'y a pas de NA dans les codes EPCI
#airPacaGES$codeEPCI <- replace(airPacaGES$codeEPCI,is.na(airPacaGES$codeEPCI),0)
head(airPacaGES)

#ecrire le fichier traite
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESN2OeqCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESCH4eqCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESPRG100.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#SI IL Y A DES NA DANS LES CODES EPCI
PBairPacaGES <-select(airPacaGES, c(codeEPCI,codeCommune))
PBairPacaGES <-filter(PBairPacaGES, is.na(codeEPCI))
doublons <- which(duplicated(PBairPacaGES$codeCommune))
pbcommunes<-PBairPacaGES[-doublons,]
airPacaGES$codeEPCI <- replace(airPacaGES$codeEPCI,is.na(airPacaGES$codeEPCI),0)
#communes pas dans un EPCI ? 5002, 5005, 5020, 5069, 4100
