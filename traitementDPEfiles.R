library(dplyr)
library(tidyverse)

#Ajout du referenciel [commune] ou epci
# epciref<-read.csv2("F:/Flexgrid/TalendREFERENTIEL/20180309epci.csv",encoding = "UTF-8")
# epcisoc<-read.csv2("F:/Flexgrid/ObservatoirePACA/ExportIndicateursObsRegional2016/EPCI/ExportSocialEPCI2016.csv",encoding="UTF-8")
# Ajout du referentiel EPCI au fichier soc
# epcisoc <- left_join(epcisoc, epciref, by=c("codeEPCI"))
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
addRef <- function(data,dataref,colRef) {
  data <- left_join(data,dataref,by=c(colRef))
}
# epcisoc <- addRefEpci(epcisoc, epciref, "codeEPCI")
# epcisoc <- delColByName(epcisoc,"epci2016")
#############################################################
##  TRAITEMENT DPE Logements sociaux
#############################################################

logSociauxDPE <- read.csv2("F:/Flexgrid/LOGEMENT SOCIAL/2018logementsSociaux.csv",encoding = "UTF-8")
summary(logSociauxDPE)
logDPE <-select(logSociauxDPE,c("codeCommune","logementCommune","TYPECONST","nbPieces","surfaceHabitable","anneeAchevement","DPEDATE","DPEENERGIE","DPESERRE"))
###Supprimer les lignes qui n'ont pas de DPE
###SOIT DPE DATE à NUll soit DPEENERGIE/DPESERRE non défini
logDPE <-filter(logDPE, !is.na(DPEENERGIE))
summary(logDPE)
colnames(logDPE)
logDPE$nbPieces <- as.integer(logDPE$nbPieces)
#IL Y A DES NA DANS LES CODES commune
logDPE$logementCommune <- as.character(logDPE$logementCommune)
is.factor(logDPE$logementCommune)
#Erreurs Marseilles spécifie les arrondissements => on remet le code commune de Marseille
#Idem pour Vallouise
logDPE$codeCommune <- replace(logDPE$codeCommune,grep("MARSEILLE",logDPE$logementCommune,"logementCommune"),13055)
logDPE$codeCommune <- replace(logDPE$codeCommune,grep("VALLOUISE",logDPE$logementCommune,"logementCommune"),5101)
summary(logDPE)
#ajouter le referentiel 
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
logDPE <- left_join(logDPE,comRef,by=c("codeCommune"))
#Liste des communes qui ne sont pas dans le référentiel: VALOUISE et les arrondissements de Marseille
# PBlogCommunes <-filter(logCommunes, is.na(codeCommune))
# doublons <- which(duplicated(PBlogCommunes$libelleCommuneMaj))
# pbcommunes<-PBlogCommunes[-doublons,]

#supprimer les colonnes inutiles
logDPE$logementCommune <- NULL
logDPE$DPEDATE <- as.factor(logDPE$DPEDATE)
#ecrire le fichier traite
write.csv2(logDPE, file="F:/Flexgrid/Sprint1/logementsSociaux.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)


