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

#ajout d'une colonne Date à l'ensemble du data frame
# add_col
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
#############################################################
##  NOUVEAUX FICHIERS OT : 10 avril 2018
#############################################################
#############################################################
# TRAITEMENT DES FICHIERS EMPLOI                            #
#############################################################
setwd("F:/Flexgrid/ObservatoirePACA/Export_avril2018")
#### ANNEE 2013
fileCom1 <- read.csv2("export_emp_fonc_COM2017.csv",dec= ".", encoding = "UTF-8")
## nombre d'emploi total + nb emplois dans l'agriculture - Années 1990 - 1999 - 2008 - 2013
emploiCom <- select(fileCom1,c("com2017","annee","elt","agricu"))
emploiCom <- filter(emploiCom, annee==2013)
emploiCom <- rename(emploiCom,date = annee)
emploiCom <- rename(emploiCom,codeCommune = com2017)
emploiCom <- mutate(emploiCom, nbEmploiTotal = round(elt,0))
emploiCom <- mutate(emploiCom, nbEmploiAgricole = round(agricu,0))
emploiCom <- mutate(emploiCom, tauxEmploiAgricole = ifelse(nbEmploiTotal==0,0,round(nbEmploiAgricole / nbEmploiTotal * 100,2)))
summary(emploiCom)
## nombre d'emploi total + part d'emploi dans la sphére non présentielle (non_pres) / présentielle (pres)
## Années 1975 - 1982 - 1990 - 1999 - 2008 - 2013
fileCom2 <- read.csv2("export_emp_sphere_COM2017.csv",dec= ".", encoding = "UTF-8")
# AJOUT du salaire horaire : PB pas la même année, 659 observations en 2012 / 641 en 2010
## salaire net horaire moyen - Années 2006, 2007, 2008, 2009, 2010, 2012
# fileCom2 <- read.csv2("export_salaire_COM2016.csv",dec= ".", encoding = "UTF-8")
# salaireCom <- select(fileCom2,c("com","annee","snhmt"))
# salaireCom <- filter(salaireCom, annee==2012)

## ajout du référentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
emploiCom <- addRef(emploiCom, comRef, "codeCommune")
#supprimer les colonnes inutiles
emploiCom$elt <- NULL
emploiCom$agricu <- NULL
#ecrire le fichier traite
write.csv2(emploiCom, file="F:/Flexgrid/Sprint1/communeEmploi.csv",row.names=FALSE, fileEncoding = "UTF-8", quote=FALSE)

############################################################
# TRAITEMENT DES FICHIERS Surface - catégorie urbain/rural #
############################################################
#### CONSOLIDATION DES données de 3 fichiers
# - pop_demo : la superficie en km2
# - zau_type : typologie urbain/periurbain/rural
# - rga_h    : superficie agriculore utilisée et superficie toujours en herbe en hectares => mettre en km2
#### ANNEE 2010
fileCom1 <- read.csv2("export_pop_demo_COM2017.csv",dec= ".", encoding = "UTF-8")
superfCom <- select(fileCom1,c("com2017","superf"))
superfCom <- rename(superfCom,codeCommune = com2017)
superfCom <- rename(superfCom,superficie = superf)
# categorie urbain/rural 1,2,3 + date associée 1968 - 1975 - 1982 - 1990 - 1999 - 2010
fileCom2 <- read.csv2("export_zau_typo_COM2016.csv",dec= ".", encoding = "UTF-8")
categCom <- filter(fileCom2, an==2010)
categCom <- rename(categCom,date = an)
categCom <- rename(categCom,codeCommune = com)
#colonne categ : 1- urbain / 2- periurbain / 3- rural
categCom$categ <- as.factor(categCom$categ)
table(categCom$categ)
levels(categCom$categ)<-c("urbain","periurbain","rural")
categCom <- rename(categCom,typeUrbainRural = categ)
summary(categCom)
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
### Le référentiel contient 962 communes => filtrer seulement celles 2017 : uniquement 952 ?
### Dévoluy - Garde-Colombe - Val Buëch-Méouge - Aiglun - Aspremont - Chateauvieux
dataStep1 <- inner_join(categCom,superfCom,by=c("codeCommune"))
#data <- inner_join(data1,comRef,by=c("codeCommune"))
summary(dataStep1)
#### Jeu de données rga_h
# exp: exploitations - otex : orientation technico-économique - uta : unité de travail annuelles - sau: superficie agricole utilisée
# cheptel : cheptel - superf_labour : superficie en terres labourables 
# superf_cult : superficie en cultures permanentes - superf_herb : superficie toujours en herbe
#### ANNEE 2010 
fileCom3 <- read.csv2("export_rga_h_COM2017.csv",dec= ".", encoding = "UTF-8")
surfaceCom <- select(fileCom3,c("com2017","annee","superf_labour","superf_cult","superf_herb"))
surfaceCom <- filter(surfaceCom,annee==2010)
surfaceCom <- rename(surfaceCom,codeCommune = com2017)
surfaceCom <- rename(surfaceCom,superfLabour = superf_labour)
surfaceCom <- rename(surfaceCom,superfCulture = superf_cult)
surfaceCom <- rename(surfaceCom,superfHerbe = superf_herb)
#les surfaces sont en hectare => mettre en km2
surfaceCom <- mutate(surfaceCom, superfLabour = round(superfLabour/100,4))
surfaceCom <- mutate(surfaceCom, superfCulture = round(superfCulture/100,4))
surfaceCom <- mutate(surfaceCom, superfHerbe = round(superfHerbe/100,4))
surfaceCom$annee <- NULL
summary(surfaceCom)
dataStep2 <- inner_join(dataStep1,surfaceCom,by=c("codeCommune"))
# Mise à 0 des valeurs négatives de surfaceAgricole et de surfaceHerbe
# Vérifier à quoi correspondent ces surfaces négatives : NON définies ?
dataStep2 <- mutate(dataStep2, superfLabour = ifelse(superfLabour<=0,NA,superfLabour))
dataStep2 <- mutate(dataStep2, superfCulture = ifelse(superfCulture<=0,NA,superfCulture))
dataStep2 <- mutate(dataStep2, superfHerbe = ifelse(superfHerbe<=0,NA,superfHerbe))
summary(dataStep2)
## ajout du référentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
dataStep2 <- addRef(dataStep2, comRef, "codeCommune")
#ecrire le fichier traite
write.csv2(dataStep2, file="F:/Flexgrid/Sprint1/communeSurface.csv",row.names=FALSE, fileEncoding = "UTF-8", quote=FALSE)
############################################################
# TRAITEMENT DES FICHIERS Transport                        #
############################################################
### Population : mobilité - déplacement principal domicile - travail / domicile - lieu d'étude
### Années 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013
fileCom <- read.csv2("export_mobilite_pole_COM2016.csv",dec= ".", encoding = "UTF-8")
summary(fileCom)
deplactCom <- filter(fileCom, annee==2013)
deplactCom <- rename(deplactCom,date = annee)
deplactCom <- rename(deplactCom,codeCommune = com)
deplactCom <- rename(deplactCom,fluxTravail = nbflux_cxx_actocc15p)
deplactCom <- rename(deplactCom,fluxEtude = nbflux_cxx_scol02p)
deplactCom <- filter(deplactCom,!is.na(fluxTravail))

reduceData <- inner_join(deplactCom,comRef,by=c("codeCommune"))
##################################
#1.TRAITEMENT DU FICHIER ACTIVITE#
##################################
setwd("F:/Flexgrid/Sprint1/R/OT")
comAct <- read.csv2("ExportActiviteCom2016.csv",encoding = "UTF-8")
summary(comAct)
colnames(comAct)
comAct <- precision(comAct,"nb..d.emplois.total","nbEmploi")
comAct <- precision(comAct,"nb..d.emplois..Agriculture.","nbEmploiAgriculture")
#AJOUT D'UN TAUX
#si dénominateur(nbEmploi) = 0, alors taux= 0
#round: une précision sur ce champ taux à 2
comAct <- mutate(comAct, tauxAgriculture = ifelse(nbEmploi==0,0,round(nbEmploiAgriculture / nbEmploi * 100,2)))
#changer le nom de la colonne Code en codeCommune
comAct <- rename(comAct,codeCommune = Code)
#horodatage
#RAF 2013
#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comAct <- addRef(comAct, comRef, "codeCommune")
#supprimer les colonnes inutiles
comAct <- delColByName(comAct,"communes")
#ecrire le fichier traite
write.csv2(comAct, file="OTTraite/communesActivite.csv", row.names=FALSE,quote=FALSE)
##################################
#2.TRAITEMENT DU FICHIER COMMUNES#
##################################
comCom <- read.csv2("ExportCommunesCom2016.csv",encoding = "UTF-8")
summary(comCom)
colnames(comCom)
#changement de nom des colonnes
comCom <- rename(comCom,codeCommune = Code)
comCom <- rename(comCom,categorie = categ)
comCom <- rename(comCom,superficieHerbe = superf_herb)
#colonne categ : 1- urbain / 2- periurbain / 3- rural
levels(comCom$superficieHerbe)
levels(comCom$categorie)
levels(comCom$categorie) <- c(levels(comCom$categorie), "URBAIN")
levels(comCom$categorie) <- c(levels(comCom$categorie), "PERIURBAIN")
levels(comCom$categorie) <- c(levels(comCom$categorie), "RURAL")
comCom$categorie[grep("1 -",comCom$categorie,"categorie")] <- 'URBAIN'
comCom$categorie[grep("2 -",comCom$categorie,"categorie")] <- 'PERIURBAIN'
comCom$categorie[grep("3 -",comCom$categorie,"categorie")] <- 'RURAL'
comCom$categorie <- droplevels(comCom$categorie)
levels(comCom$categorie)
#colonne superficie en herbe => as numeric
#avant suppression des données incorrectes
levels(comCom$superficieHerbe)
comCom$superficieHerbe[grep("secret|NULL",comCom$superficieHerbe,"superficieHerbe")] <- '0'
comCom$superficieHerbe <- droplevels(comCom$superficieHerbe)
comCom$superficieHerbe <- as.integer(comCom$superficieHerbe)
#ajouter les colonnes au fichier précédent
#cbind ?
#ecrire le fichier traite
write.csv2(comCom, file="OTTraite/communesCommune.csv", row.names=FALSE,quote=FALSE)
######################################
#3.TRAITEMENT DU FICHIER ENTREPRISE  #
######################################
setwd("F:/Flexgrid/Sprint1/R/OT")
comEnt <- read.csv2("ExportEntreprisesCom2016.csv",encoding = "UTF-8")
summary(comEnt)
colnames(comEnt)
#HORODATAGE année 2014
# nbrow <- nrow(comEnt)
# date<-rep(2014,nbrow)
# comEnt<-cbind(comEnt,date)
comEnt <- horodateData(comEnt,2014)

#changement de nom des colonnes
comEnt <- rename(comEnt,codeCommune = Code)
comEnt <- rename(comEnt,nbEntTotal = enntot)
comEnt <- rename(comEnt,nbEntIndustrie = ennbe)
comEnt <- rename(comEnt,nbEntConstruction = enncfz)
comEnt <- rename(comEnt,nbEntPublic = ennoq)
comEnt <- rename(comEnt,nbEntCommerce = enngz)
#on supprime les entreprises de Commerce de la categorie enngu
comEnt <- rename(comEnt,nbEntTransportCommerce = enngu)
comEnt <- mutate(comEnt,nbEntTransport = (nbEntTransportCommerce - nbEntCommerce))
comEnt$nbEntTransportCommerce <- NULL
comEnt$com2016 <- NULL
summary(comEnt)
#Transformation colonne nb* en 2 colonnes typeEntreprise + nbEntreprise
#on remplace chaque colonne par date / population
addTypeColonne <- function(data,nomCol,type,newcolonne) {
  comenttype<-select(data,codeCommune,nomCol)
  head(comenttype)
  nbrow <- nrow(comenttype)
  nomcol<-c("codeCommune",newcolonne,"typeEntreprise")
  typeToAdd<-rep(type,nbrow)
  data<-cbind(comenttype,typeToAdd)
  colnames(data)<-nomcol
  return (data)
}
#Colonne générale nbEntreprise + type
newComEnt <- addTypeColonne(comEnt,"nbEntTotal","Total","nbEntreprise") 
newComEnt <- rbind(newComEnt,addTypeColonne(comEnt,"nbEntIndustrie","Industrie","nbEntreprise"))
newComEnt <- rbind(newComEnt,addTypeColonne(comEnt,"nbEntConstruction","Construction","nbEntreprise"))
newComEnt <- rbind(newComEnt,addTypeColonne(comEnt,"nbEntCommerce","Commerce","nbEntreprise"))
newComEnt <- rbind(newComEnt,addTypeColonne(comEnt,"nbEntPublic","Public","nbEntreprise"))
newComEnt <- rbind(newComEnt,addTypeColonne(comEnt,"nbEntTransport","Transport","nbEntreprise"))
newComEnt <- horodateData(newComEnt,2014)

#Ajout des taux
comEnt <- mutate(comEnt, tauxIndustrie = ifelse(nbEntTotal==0,0,round(nbEntIndustrie / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxConstruction = ifelse(nbEntTotal==0,0,round(nbEntConstruction / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxPublic = ifelse(nbEntTotal==0,0,round(nbEntPublic / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxCommerce = ifelse(nbEntTotal==0,0,round(nbEntCommerce / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxTransport = ifelse(nbEntTotal==0,0,round(nbEntTransport / nbEntTotal * 100,2)))
#verification coherence : OK sauf pour les communes avec 0
comEnt <- mutate(comEnt, taux = round(tauxIndustrie+tauxConstruction+tauxPublic+tauxCommerce+tauxTransport,0))
summary(comEnt)
comEnt$taux <- NULL
#ajout du référentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comEnt <- addRef(comEnt, comRef, "codeCommune")
comEnt$nbEntIndustrie <- NULL
comEnt$nbEntConstruction <- NULL
comEnt$nbEntPublic <- NULL
comEnt$nbEntCommerce <- NULL
comEnt$nbEntTransport <- NULL
#ecrire le fichier traite
#write.csv2(comEnt, file="OTTraite/communeEntreprise.csv", row.names=FALSE)
write.csv2(comEnt, file="F:/Flexgrid/Sprint1/communeEntrepriseTaux.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#ajout du référentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
newComEnt <- addRef(newComEnt, comRef, "codeCommune")
write.csv2(newComEnt, file="F:/Flexgrid/Sprint1/communeEntreprise.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#TEST EPCI NA
PBepci <-select(comEnt, c(codeEPCI,codeCommune))
PBepci <-filter(PBepci, is.na(codeEPCI))
doublons <- which(duplicated(PBepci$codeCommune))
pbcommunes<-PBairepci[-doublons,]

###### EXPORT MARS 
setwd("F:/Flexgrid/ObservatoirePACA/Export_mars2018")
comEnt <- read.csv2("export_demo_entreprises_COM2016.csv",encoding = "UTF-8")
summary(comEnt)
colnames(comEnt)
#Suppression des communes qui ne sont pas dans PACA
listCommunesPaca1 <- filter(comEnt, com > 4000 & com < 7000) #04-05-06
listCommunesPaca2 <- filter(comEnt, com >= 13000 & com < 14000) #13
listCommunesPaca3 <- filter(comEnt, com >= 83000 & com < 85000) #83 et 84
comEnt <- rbind(listCommunesPaca1,listCommunesPaca2)
comEnt <- rbind(comEnt,listCommunesPaca3)
summary(comEnt$com)
communes <- as.factor(comEnt$com)
str(communes)
#=> 979 communes
#changement de nom des colonnes
comEnt <- rename(comEnt,codeCommune = Code)
comEnt <- rename(comEnt,nbEntTotal = enntot)
comEnt <- rename(comEnt,nbEntIndustrie = ennbe)
comEnt <- rename(comEnt,nbEntConstruction = enncfz)
comEnt <- rename(comEnt,nbEntPublic = ennoq)
comEnt <- rename(comEnt,nbEntCommerce = enngz)
#on supprime les entreprises de Commerce de la categorie enngu
comEnt <- rename(comEnt,nbEntTransportCommerce = enngu)
comEnt <- mutate(comEnt,nbEntTransport = (nbEntTransportCommerce - nbEntCommerce))
comEnt$nbEntTransportCommerce <- NULL
#Ajout des taux
comEnt <- mutate(comEnt, tauxIndustrie = ifelse(nbEntTotal==0,0,round(nbEntIndustrie / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxConstruction = ifelse(nbEntTotal==0,0,round(nbEntConstruction / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxPublic = ifelse(nbEntTotal==0,0,round(nbEntPublic / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxCommerce = ifelse(nbEntTotal==0,0,round(nbEntCommerce / nbEntTotal * 100,2)))
comEnt <- mutate(comEnt, tauxTransport = ifelse(nbEntTotal==0,0,round(nbEntTransport / nbEntTotal * 100,2)))
#verification coherence : OK sauf pour les communes avec 0
comEnt <- mutate(comEnt, taux = round(tauxIndustrie+tauxConstruction+tauxPublic+tauxCommerce+tauxTransport,0))
summary(comEnt)
comEnt$taux <- NULL
#ecrire le fichier traite
write.csv2(comEnt, file="OTTraite/marsCommunesEntreprises.csv", row.names=FALSE)
write.csv2(comEnt, file="F:/Flexgrid/Sprint1/marsCommunesEntreprises.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################
#4. TRAITEMENT DU FICHIER SOCIAL #
##################################
comSoc <- read.csv2("ExportSocialCom2016.csv",dec= ",",encoding = "UTF-8")
summary(comSoc)
colnames(comSoc)

#changement de nom des colonnes
comSoc <- rename(comSoc,codeCommune = Code)
comSoc <- rename(comSoc,tauxSalarie = pt_pxx_sal15p)
comSoc <- rename(comSoc,tauxNonSalarie = pt_pxx_nsal15p)
comSoc <- mutate(comSoc,nbLogement=as.integer(nb_log))
comSoc <- mutate(comSoc,nbResidencePrincipale=as.integer(nb_rp))
#ajout d'une colonne nb de résidences secondaires
comSoc <- mutate(comSoc,nbResidenceSecondaire = (nbLogement - nbResidencePrincipale))
#Transformation colonne nb* en 2 colonnes typeLogement + nbLogements
addTypeLogementColonne <- function(data,nomCol,type,newcolonne) {
  comenttype<-select(data,codeCommune,nomCol)
  head(comenttype)
  nbrow <- nrow(comenttype)
  nomcol<-c("codeCommune",newcolonne,"typeLogement")
  typeToAdd<-rep(type,nbrow)
  data<-cbind(comenttype,typeToAdd)
  colnames(data)<-nomcol
  return (data)
}
#Colonne générale nbEntreprise + type
newComSoc <- addTypeLogementColonne(comSoc,"nbLogement","Total","nbLogement") 
newComSoc <- rbind(newComSoc,addTypeLogementColonne(comSoc,"nbResidencePrincipale","Principale","nbLogement"))
newComSoc <- rbind(newComSoc,addTypeLogementColonne(comSoc,"nbResidenceSecondaire","Secondaire","nbLogement"))
newComSoc <- horodateData(newComSoc,2013)

#HORODATAGE année 2013
comSoc <- horodateData(comSoc,2013)

#AJOUT D'UN TAUX
#si dénominateur(nbEmploi) = 0, alors taux= 0
#round: une précision sur ce champ taux à 2
comSoc <- mutate(comSoc, tauxResidencePrincipale = ifelse(nbLogement==0,0,round(nbResidencePrincipale / nbLogement * 100,2)))
comSoc <- mutate(comSoc, tauxResidenceSecondaire = ifelse(nbLogement==0,0,round(nbResidenceSecondaire / nbLogement * 100,2)))
#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comSoc <- addRef(comSoc, comRef, "codeCommune")
head(comSoc)
#supprimer les colonnes inutiles
comSoc <- delColByName(comSoc,"com2016")
comSoc <- delColByName(comSoc,"nb_log")
comSoc <- delColByName(comSoc,"nb_rp")
comSoc <- delColByName(comSoc,"nbResidencePrincipale")
comSoc <- delColByName(comSoc,"nbResidenceSecondaire")
summary(comSoc)
#ecrire le fichier traite
#write.csv2(comSoc, file="OTTraite/communeSocial.csv", row.names=FALSE)
write.csv2(comSoc, file="F:/Flexgrid/Sprint1/communeSocialTaux.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
newComSoc <- addRef(newComSoc, comRef, "codeCommune")
summary(newComSoc)
write.csv2(newComSoc, file="F:/Flexgrid/Sprint1/communeSocial.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#####################################
#5. TRAITEMENT DU FICHIER TRANSPORT #
#####################################
comTrans <- read.csv2("ExportTransportCom2016.csv",encoding = "UTF-8")
summary(comTrans)
colnames(comTrans)
#HORODATAGE année 2013
comTrans <- horodateData(comTrans,2013)
#changement de nom des colonnes
comTrans <- rename(comTrans,codeCommune = Code)
comTrans <- mutate(comTrans,nbMenage = as.integer(rpxx))
comTrans <- mutate(comTrans,nb0Voiture = as.integer(rpxx_voit_0))
comTrans <- mutate(comTrans,nb1Voiture = as.integer(rpxx_voit_1))
comTrans <- mutate(comTrans,nb2Voiture = as.integer(rpxx_voit_2))
comTrans <- mutate(comTrans,nb3Voiture = as.integer(rpxx_voit_3))

#Transformation colonne nb* en 2 colonnes NbVoiture + nbMenage
addNbVoitureColonne <- function(data,nomCol,type,newcolonne) {
  comenttype<-select(data,codeCommune,nomCol)
  head(comenttype)
  nbrow <- nrow(comenttype)
  nomcol<-c("codeCommune",newcolonne,"catVoiture")
  typeToAdd<-rep(type,nbrow)
  data<-cbind(comenttype,typeToAdd)
  colnames(data)<-nomcol
  return (data)
}
#Colonne générale nbMenage + catégorie=nbVoiture
newComTrans <- addNbVoitureColonne(comTrans,"nb0Voiture","0","nbMenage") 
newComTrans <- rbind(newComTrans,addNbVoitureColonne(comTrans,"nb1Voiture","1","nbMenage"))
newComTrans <- rbind(newComTrans,addNbVoitureColonne(comTrans,"nb2Voiture","2","nbMenage"))
newComTrans <- rbind(newComTrans,addNbVoitureColonne(comTrans,"nb3Voiture","3 et plus","nbMenage"))
newComTrans <- horodateData(newComTrans,2013)
#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
newComTrans <- addRef(newComTrans, comRef, "codeCommune")
summary(newComTrans)
write.csv2(newComTrans, file="F:/Flexgrid/Sprint1/communeTransport.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#AJOUT D'UN TAUX
#si dénominateur(nbEmploi) = 0, alors taux= 0
#round: une précision sur ce champ taux à 2
comTrans <- mutate(comTrans, taux0Voiture = ifelse(nbMenage==0,0,round(nb0Voiture / nbMenage * 100,2)))
comTrans <- mutate(comTrans, taux1Voiture = ifelse(nbMenage==0,0,round(nb1Voiture / nbMenage * 100,2)))
comTrans <- mutate(comTrans, taux2Voiture = ifelse(nbMenage==0,0,round(nb2Voiture / nbMenage * 100,2)))
comTrans <- mutate(comTrans, taux3Voiture = ifelse(nbMenage==0,0,round(nb3Voiture / nbMenage * 100,2)))
#verification coherence : OK 
comTrans <- mutate(comTrans, taux = round(tauxSansVoiture+taux1Voiture+taux2Voiture+taux3Voiture,0))
summary(comTrans)
comTrans$taux <- NULL
#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comTrans <- addRef(comTrans, comRef, "codeCommune")
head(comTrans)
#supprimer les colonnes inutiles
comTrans <- delColByName(comTrans,"com2016")
comTrans <- delColByName(comTrans,"rpxx")
comTrans <- delColByName(comTrans,"rpxx_voit_0")
comTrans <- delColByName(comTrans,"rpxx_voit_1")
comTrans <- delColByName(comTrans,"rpxx_voit_2")
comTrans <- delColByName(comTrans,"rpxx_voit_3")
#ecrire le fichier traite
#write.csv2(comTrans, file="OTTraite/communeTransport.csv", row.names=FALSE)
write.csv2(comTrans, file="F:/Flexgrid/Sprint1/communeTransportTaux.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
######################################
#6. TRAITEMENT DU FICHIER POPULATION #
######################################
comPop <- read.csv2("ExportPopulationCom2016.csv",encoding = "UTF-8")
summary(comPop)
colnames(comPop)
#changement de nom des colonnes
comPop <- rename(comPop,codeCommune = Code)

#horodatage pour faire un fichier population de 2006 à 2014  
newComPop <- horodateColonne(comPop,"p14_pop",2014,"population") 
newComPop <- rbind(newComPop,horodateColonne(comPop,"p13_pop",2013,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p12_pop",2012,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p11_pop",2011,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p10_pop",2010,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p09_pop",2009,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p08_pop",2008,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p07_pop",2007,"population"))
newComPop <- rbind(newComPop,horodateColonne(comPop,"p06_pop",2006,"population"))
summary(newComPop)
newComDensite <- rbind(horodateColonne(comPop,"dens14",2014,"densite"),horodateColonne(comPop,"dens90",1990,"densite"))
summary(newComDensite)
#Faire autre chose pour les indices de vieillissement et de jeunesse => 2 champs + date : merge ?
newComPopVieux <- horodateColonne(comPop,"ind_vieillissement",2014,"indiceVieux")
newComPopJeunesse <- horodateColonne(comPop,"ind_jeunesse",2014,"indiceJeunesse")
newComPopOther <- left_join(newComPopVieux,newComPopJeunesse,by=c("codeCommune","date"))

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
newComPop <- addRef(newComPop, comRef, "codeCommune")
head(newComPop)
newComDensite <- addRef(newComDensite, comRef, "codeCommune")
head(newComDensite)
newComPopOther <- addRef(newComPopOther, comRef, "codeCommune")
head(newComPopOther)
#ecrire le fichier traite
#write.csv2(newComPop, file="OTTraite/communeHistoriquePopulation.csv", row.names=FALSE)
write.csv2(newComPop, file="F:/Flexgrid/Sprint1/communeHistoriquePopulation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#write.csv2(newComDensite, file="OTTraite/communeHistoriqueDensite.csv", row.names=FALSE)
write.csv2(newComDensite, file="F:/Flexgrid/Sprint1/communeHistoriqueDensite.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#write.csv2(newComPopOther, file="OTTraite/communeIndicePopulation.csv", row.names=FALSE)
write.csv2(newComPopOther, file="F:/Flexgrid/Sprint1/communeIndicePopulation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

summary(newComDensite)
library(ggplot2)
ggplot(data=newComDensite,aes(x=densite)+geom_bar(densite))

#TEST EPCI NA
PBepci <-select(newComPopOther, c(codeEPCI,codeCommune))
PBepci <-filter(PBepci, is.na(codeEPCI))
doublons <- which(duplicated(PBepci$codeCommune))
pbcommunes<-PBairepci[-doublons,]

######################################
#7. TRAITEMENT DU FICHIER ENERGIE #
######################################
setwd("F:/Flexgrid/Sprint1/R/OT")
comEnergie <- read.csv2("ExportEnergiesCom2016.csv",encoding = "UTF-8")
summary(comEnergie)
colnames(comEnergie)
#HORODATAGE année 2014
comEnergie <- horodateData(comEnergie,2014)
#changement de nom des colonnes
comEnergie <- rename(comEnergie,codeCommune = Code)
comEnergie <- rename(comEnergie,nbChaufferies = nombre_chaufferies)
comEnergie <- rename(comEnergie,puissanceChaufferiekw = puiss_kw)
comEnergie <- rename(comEnergie,consoBoisTonnes = conso_bois_tonnes)
comEnergie <- rename(comEnergie,nbPVBati = nb_pv)
comEnergie <- rename(comEnergie,puissancePVmw = puiss_mw)
comEnergie <- rename(comEnergie,nbInstalPV = nb_inst)
comEnergie <- rename(comEnergie,surfacePVm2 = surface_m2)
head(comEnergie)

#nbPV factor => integer
comEnergie$nbPVBati <- as.integer(comEnergie$nbPVBati)
newComEnergie$surfacePVm2 <- as.integer(comEnergie$surfacePVm2)
#changement d'unite pour les puissances
comEnergie <- mutate(comEnergie, puissanceChaufferiemw = ifelse(puissanceChaufferiekw=="NA","NA",round(puissanceChaufferiekw / 1000,4)))
comEnergie <- mutate(comEnergie, puissancePVmw = ifelse(puissancePVmw=="NA","NA",round(puissancePVmw,3)))

#nbChaufferies - nbPVBati - nbInstall => nbInstal
#puissanceChaufferie - puissancePV => 
#homogeneisation des colonnes  nbInstall / puissanceMW + ajout du detailFiliere
addFiliereColonneAndSurface <- function(data,typeENR,ssType,colnb,colpuissance,colsurface) {
  msg <- paste(colnb, colpuissance, colsurface)
  print(msg)
  if (colsurface=="NA")
  {
    print("2 colonnes : nb, puissance")
    comtype <- select(data,codeCommune,date,colnb,colpuissance)
    comtype <- add_column(comtype,surface=0)
  }
  else 
  {
    print("3 colonnes : nb, puissance, surface")
    comtype <- select(data,codeCommune,date,colnb,colpuissance,colsurface)
  }
    comtype <- add_column(comtype,detailFiliere=typeENR)
    comtype <- add_column(comtype,typeFiliere=ssType)
    nomcol<-c("codeCommune","date","nbInstal","puissanceMW","surfacem2","detailFiliere","typeFiliere")
    colnames(comtype)<-nomcol
  return (comtype)
}
newComEnergie <- addFiliereColonneAndSurface(comEnergie,"Bois","énergie-chaufferie","nbChaufferies","puissanceChaufferiemw","NA") 
newComEnergie <- rbind(newComEnergie,addFiliereColonneAndSurface(comEnergie,"Solaire photovoltaïque","Bati","nbPVBati","puissancePVmw","surfacePVm2"))

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
newComEnergie <- addRef(newComEnergie, comRef, "codeCommune")
head(newComEnergie)

#Remplacement des NA par 0
newComEnergie$nbInstal <- replace(newComEnergie$nbInstal,is.na(newComEnergie$nbInstal),0)
newComEnergie$puissanceMW <- replace(newComEnergie$puissanceMW,is.na(newComEnergie$puissanceMW),0)
newComEnergie$surfacem2 <- replace(newComEnergie$surface,is.na(newComEnergie$surfacem2),0)
summary(newComEnergie)
#ecrire le fichier traite
#write.csv2(comEnergie, file="OTTraite/communeEnergie.csv", row.names=FALSE)
write.csv2(newComEnergie, file="F:/Flexgrid/Sprint1/communeProductionEnergie.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
