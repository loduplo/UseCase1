library(dplyr)
library(tidyverse)
######################################################################################################################
#FONCTIONS DE TRAITEMENT DES FICHIERS OT pour les EPCI
######################################################################################################################
#FONCTION d'HORODATAGE 
#ajout d'une colonne Date à l'ensemble du data frame
# add_col
horodateData <- function(data,annee) {
  nbrow <- nrow(data)
  date<-rep(annee,nbrow)
  data<-cbind(data,date)
  return (data)
}
### REFERENTIEL EPCI - INSEE 01-01-2018
inseeref <-read.csv2("F:/Flexgrid/INSEE/IntercommunaliteMetropole2018.csv",encoding = "UTF-8")
pacaref <- filter(inseeref,REG==93)
pacaref <- select(pacaref, c(EPCI,LIBEPCI,DEP))
epciref <- distinct(pacaref,EPCI, .keep_all=TRUE)
epciref <- rename(epciref,codeEpci = EPCI)
epciref$codeEpci <- as.character(epciref$codeEpci)
epciref <- rename(epciref,libelleEpci = LIBEPCI)
epciref <- rename(epciref,dep = DEP)
#Ajout de caracteristiques des EPCI
inseeEpci <- read.csv2("F:/Flexgrid/INSEE/IntercommunaliteEPCI2018.csv",encoding = "UTF-8")

inseeEpci <- rename(inseeEpci,EPCI = X.U.FEFF.EPCI)
inseeEpci <- select(inseeEpci, c(EPCI,NATURE_EPCI,NB_COM))
inseeEpci <- rename(inseeEpci,codeEpci = EPCI)
inseeEpci$codeEpci <- as.character(inseeEpci$codeEpci)
pacaEpci <- left_join(epciref,inseeEpci,by=c("codeEpci"))
pacaEpci <- rename(pacaEpci,nature = NATURE_EPCI)
pacaEpci <- rename(pacaEpci,nbCommune = NB_COM)
#champ département transformé en numérique
droplevels(pacaEpci$dep)
#ATTENTION utiliser as.character avant le as.integer, sinon on perd les numéros de département (ordre des factor) 
pacaEpci$dep <- as.integer(as.character(pacaEpci$dep))
table(pacaEpci$dep)
#type de l'EPCI : CA, CC, ME
#CA CC CU ME ZZ 
#16 33  0  3  0 
table(pacaEpci$nature)
pacaEpci$nature <- as.numeric(pacaEpci$nature)
#stocker le referentel epci
write.csv2(pacaEpci, file="F:/Flexgrid/Sprint1/R/REF/referentielEpci.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#Ajout du referenciel epci
epciref<-read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielEpci.csv",encoding = "UTF-8")
addRefEpci <- function(data,epciref,colRef) {
  data <- left_join(data,epciref,by=c(colRef))
}
pacaEpci <- epciref
#epcisoc <- addRefEpci(epcisoc, epciref, "codeEPCI")
###################################################################################################################
# CARTOGRAPHIE AVEC R
###################################################################################################################
# Installation des packages de base
install.packages("sp")
install.packages("rgdal")

# Installation des packages accessoires
install.packages("RColorBrewer")
install.packages("classInt")

# Installation des packages de rendu
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maptools")
# Import des librairies nécessaires
library("sp")
library("rgdal")

library("RColorBrewer")
library("classInt")

library("ggplot2")
library("ggmap")
library("maptools")
####################################################################################################################

#Ajout des coordonnées géographiques des EPCI
library(jsonlite)
enedisgeo<-fromJSON("F:/Flexgrid/GEO/EPCI/production-electrique-par-filiere-a-la-maille-epci.geojson",flatten=FALSE)
###IL MANQUE les coordonnées d'Aix Marseille Provence code_epci = 200054807
#MARSEILLE : latitude=43.296482;longtitude=5.369779999999992
#Découpage en 80 EPCI
epcigeo <- enedisgeo$features
epcigeo <- epcigeo %>%
  set_names(names(.) %>% str_replace("properties.", ""))
epcigeoPoint <- select(epcigeo, c(code_epci,geo_point_2d))
epcigeoPoint <- rename(epcigeoPoint,codeEpci = code_epci)
epcigeoPoint$codeEpci <- as.integer(epcigeoPoint$codeEpci)
#transform geo_point_2d list in 2 columns
geo <- do.call(rbind,epcigeoPoint$geo_point_2d)
colnames(geo) <- c("lat","lon")
epcigeoPoint <- cbind(epcigeoPoint,geo)
epcigeoPoint$geo_point_2d <- NULL
epcigeoPoint <- distinct(epcigeoPoint,codeEpci,.keep_all=TRUE)
#stocker le referentel epci + geo 
write.csv2(epcigeoPoint, file="F:/Flexgrid/Sprint2/referentielEpciEnedisGeo.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#library(jsonlite)
## Il manque 2 CC : CA du grand avignon 248400251 et CC Enclave des Papes 200040681
epcigeo<-fromJSON("F:/Flexgrid/GEO/contours-epci-2017.json",flatten=TRUE)
epcigeoPoint <- select(epcigeo, c(fields.siren_epci,fields.geo_point_2d,fields.nom_comple))
epcigeoPoint <- epcigeoPoint %>%
  set_names(names(.) %>% str_replace("fields.", ""))
epcigeoPoint <- rename(epcigeoPoint,codeEpci = siren_epci)
epcigeoPoint$codeEpci <- as.integer(epcigeoPoint$codeEpci)
epcigeoPoint <- rename(epcigeoPoint,libelleEpci = nom_comple)
#transform geo_point_2d list in 2 columns
geo <- do.call(rbind,epcigeoPoint$geo_point_2d)
colnames(geo) <- c("latitude","longitude")
epcigeoPoint <- cbind(epcigeoPoint,geo)
epcigeoPoint$geo_point_2d <- NULL

#stocker le referentel epci + geo : point GPS en 2D
write.csv2(epcigeoPoint, file="F:/Flexgrid/Sprint2/referentielEpciGeo.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#selection des points shape
epcigeo <- epcigeo %>%
  set_names(names(.) %>% str_replace("fields.", ""))
epcigeoShape <- select(epcigeo, c(siren_epci,nom_comple,geo_shape.type,geo_shape.coordinates))
epcigeoShape <- rename(epcigeoShape,codeEpci = siren_epci)
epcigeoShape$codeEpci <- as.integer(epcigeoShape$codeEpci)
epcigeoShape <- rename(epcigeoShape,libelleEpci = nom_comple)

###RAF : traitements complémentaires 
write_json(epcigeoShape, path="F:/Flexgrid/Sprint2/referentielEpciShape.geojson", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)




#### NE FN PAS !!!!
epcigeoPoint <- mutate(epcigeoPoint,geo=as.list(geo_point_2d))
epcigeoPoint <- mutate(epcigeoPoint,geo=theList)
epcigeoPoint <- mutate(epcigeoPoint,latitude=theList[[1]])
epcigeoPoint <- mutate(epcigeoPoint,longitude=theList[[1]][1])
mode(epcigeoPoint$geo_point_2d[1])
length(epcigeoPoint$geo_point_2d[1])
data <- left_join(epcigeoPoint,clusterEpci,by=c("codeEpci"))
geoPoint <- select(epcigeoPoint, c(geo_point_2d))
length(unlist(geoPoint[[1]][[1]]))
length(unlist(geoPoint[[1]]))
length(unlist(geoPoint))
unlist(geoPoint)
nbPoint=length(geoPoint[[1]])
theList = as.list(geoPoint[[1]])
mode(theList)
#premier element
pointNb <- unlist(geoPoint[[1]][[1]])
mode(pointNb)
df <- data.frame(1,pointNb[1],pointNb[2])
names(df) <- c("nb","latitude","longitude")
pointNb <- unlist(geoPoint[[1]][[2]])
de <- data.frame(2,pointNb[1],pointNb[2])
names(de) <- c("nb","latitude","longitude")
df <- rbind(df,de)
for (nb in 3:50)
{
  pointNb <- unlist(geoPoint[[1]][[nb]])
  de <- data.frame(nb,pointNb[1],pointNb[2])
  names(de) <- c("nb","latitude","longitude")
  df <- rbind(df,de)
}
## df contient la liste des 50 observations

point1 <- unlist(geoPoint[[1]][[1]])
is.vector(point1)
is(point1)
point1[1]
is(geoPoint)
#epcigeo <- select(epcigeo, c(fields.siren_epci,fields.geo_point_2d,fields.nom_comple,fields.fiscalite,fields.geo_shape.type,fields.geo_shape.coordinates))
#############################################################
##  FICHIERS OT : export avril 2018
#############################################################
setwd("F:/Flexgrid/ObservatoirePACA/Export_avril2018/EPCI")
# ENERGIE
# fileEpci1 <- read.csv2("export_com_bois_EPCI2017.csv",dec= ".", encoding = "UTF-8")
# fileEpci2 <- read.csv2("export_com_pv_EPCI2017.csv",dec= ".", encoding = "UTF-8")
# fileEpci3 <- read.csv2("export_com_solther_EPCI2017.csv",dec= ".", encoding = "UTF-8")
#Caractéristiques
# fileEpci4 <- read.csv2("export_emp_fonc_EPCI2017.csv",dec= ".", encoding = "UTF-8")
# fileEpci5 <- read.csv2("export_pop_demo_EPCI2017.csv",dec= ".", encoding = "UTF-8")
# fileEpci6 <- read.csv2("export_rga_h_EPCI2017.csv",dec= ".", encoding = "UTF-8")
# fileEpci7 <- read.csv2("export_emp_sphere_EPCI2017.csv",dec= ".", encoding = "UTF-8")

#fileEpci8 <- read.csv2("F:/Flexgrid/ObservatoirePACA/ExportIndicateursObsRegional2016/EPCI/ExportSocialEPCI2016.csv",dec= ".", encoding = "UTF-8")
# NBepci <-select(fileEpci1, c(epci2017))
# allEpci1 <- distinct(fileEpci1,epci2017)

#Fichier des populations : choix année 2013
fileEpci5 <- read.csv2("export_pop_demo_EPCI2017.csv",dec= ".", encoding = "UTF-8")
popEpci5 <- select(fileEpci5,c(epci2017,superf,p13_pop,p13_poph,p13_popf))
popEpci5 <- rename(popEpci5,codeEpci = epci2017)
popEpci5 <- rename(popEpci5,superficie = superf)
popEpci5 <- rename(popEpci5,pop2013 = p13_pop)
popEpci5 <- rename(popEpci5,popHomme = p13_poph)
popEpci5 <- rename(popEpci5,popFemme = p13_popf)
#ajout de la densite et des taux femme/homme
popEpci5 <- mutate(popEpci5, tauxHomme = round(popHomme/pop2013*100,2))
popEpci5 <- mutate(popEpci5, tauxFemme = round(popFemme/pop2013*100,2))
popEpci5 <- mutate(popEpci5, densite = round(pop2013/superficie,2))
#Constitution du referentiel des EPCI PACA : step population
pacaEpci <- left_join(pacaEpci,popEpci5,by=c("codeEpci"))

#Fichier des emplois : choix année 2013 - 1990, 1999, 2008, 2013 -
#elt : nb total d'emploi - admpub : dans l'administration publique - btp : dans btp 
#comint: commerce inter-entreprises - conrec: conception, recherche - culloi: culture, loisirs
#distri: distribution - edufor: éducation, formation - entrep: entretien, réparation
#fabric: fabrication - gestio: gestion - logist: logistique - preint: prestations intellectuelles
#sansoc: santé, action sociale - serpro: services de proximité - comint_cad: part des cadres dans les emplois inter-entreprises
#culloi_cad: part des cadres dans les emplois culture, loisirs - gestio_cad: part des cadres dans les emplois gestion
#preint_cad: part des cadres dans les emplois prestations intellectuelles - cfm: nb de cadres des fonctions métropolitaines
fileEpci4 <- read.csv2("export_emp_fonc_EPCI2017.csv",dec= ".", encoding = "UTF-8")
empEpci4 <- filter(fileEpci4, annee==2013)
empEpci4 <- select(empEpci4,c(epci2017,elt,agricu))
empEpci4 <- rename(empEpci4,codeEpci= epci2017)
empEpci4 <- rename(empEpci4, nbEmploiTotal = elt)
empEpci4 <- rename(empEpci4, nbEmploiAgricole = agricu)
empEpci4 <- mutate(empEpci4, tauxEmploiAgricole = ifelse(nbEmploiTotal==0,0,round(nbEmploiAgricole / nbEmploiTotal * 100,2)))

#AJOUT DES DONNES SPHERE
fileEpci7 <- read.csv2("export_emp_sphere_EPCI2017.csv",dec= ".", encoding = "UTF-8")
empEpci7 <- filter(fileEpci7, annee==2013)
empEpci7 <- select(empEpci7,c(epci2017,elt,non_pres,pres))
empEpci7 <- rename(empEpci7,codeEpci = epci2017)
empEpci7 <- rename(empEpci7, nbEmploiTotal = elt)
empEpci7 <- rename(empEpci7, nbEmploiPres = pres)
empEpci7 <- rename(empEpci7, nbEmploiProd = non_pres)
empEpci7 <- mutate(empEpci7, tauxEmploiPres = ifelse(nbEmploiTotal==0,0,round(nbEmploiPres / nbEmploiTotal * 100,2)))
empEpci7 <- mutate(empEpci7, tauxEmploiProd = ifelse(nbEmploiTotal==0,0,round(nbEmploiProd / nbEmploiTotal * 100,2)))
#Constitution du referentiel des EPCI PACA : step emploi
pacaEpci <- left_join(pacaEpci,empEpci7,by=c("codeEpci"))
summary(pacaEpci)
#### Jeu de données rga_h
# exp: exploitations - otex : orientation technico-économique - uta : unité de travail annuelles - sau: superficie agricole utilisée
# cheptel : cheptel - superf_labour : superficie en terres labourables 
# superf_cult : superficie en cultures permanentes - superf_herb : superficie toujours en herbe
#### ANNEE 2010 
#Fichier des surfaces agricoles : choix année 2010
fileEpci6 <- read.csv2("export_rga_h_EPCI2017.csv",dec= ".", encoding = "UTF-8")
surfEpci <- filter(fileEpci6, annee==2010)
surfEpci <- select(surfEpci,c(epci2017,superf_cult,superf_labour,superf_herb))
surfEpci <- rename(surfEpci,codeEpci = epci2017)
surfEpci <- rename(surfEpci, superfLabour = superf_labour)
surfEpci <- rename(surfEpci, superfCulture = superf_cult)
surfEpci <- rename(surfEpci, superfHerbe = superf_herb)
#les surfaces sont en hectare => mettre en km2
surfEpci <- mutate(surfEpci, superfLabour = round(superfLabour/100,4))
surfEpci <- mutate(surfEpci, superfCulture = round(superfCulture/100,4))
surfEpci <- mutate(surfEpci, superfHerbe = round(superfHerbe/100,4))
# Vérifier à quoi correspondent ces surfaces négatives : NON définies ?
surfEpci <- mutate(surfEpci, superfLabour = ifelse(superfLabour<=0 ,NA,superfLabour))
surfEpci <- mutate(surfEpci, superfCulture = ifelse(superfCulture<=0,NA,superfCulture))
surfEpci <- mutate(surfEpci, superfHerbe = ifelse(superfHerbe<=0,NA,superfHerbe))
summary(surfEpci)
#Constitution du referentiel des EPCI PACA : step surfaces agricoles
pacaEpci <- left_join(pacaEpci,surfEpci,by=c("codeEpci"))
summary(pacaEpci)

#### EPCI2016
# allEpci7 <- distinct(fileEpci7,codeEPCI)
#############################################################
# TRAITEMENT DES FICHIER INSEE DEV DURABLE
#############################################################
setwd("F:/Flexgrid/INSEE")
fileInsee1 <- read.csv2("inseeDevDurableContexteNatura.csv",dec= ",", encoding = "UTF-8")
fileInsee2 <- read.csv2("inseeDevDurableContexteNbSeveso.csv",dec= ",", encoding = "UTF-8")
fileInsee3 <- read.csv2("inseeDevDurableFinalite1DureeMoyenneNavette.csv",dec= ",", encoding = "UTF-8")
fileInsee4 <- read.csv2("inseeDevDurableFinalite1PartTC.csv",dec= ",", encoding = "UTF-8")
fileInsee5 <- read.csv2("inseeDevDurableFinalite1PartVoiture.csv",dec= ",", encoding = "UTF-8")
#tourisme2013 : nb de lits pour 100 hts
fileInsee6 <- read.csv2("inseeDevDurableFinalite4Tourisme.csv",dec= ",", encoding = "UTF-8")
fileInsee7 <- read.csv2("inseeDevDurableFinalite4TauxActivite.csv",dec= ",", encoding = "UTF-8")
fileInsee1 <- select(fileInsee1,-c(libelleEpci))
fileInsee2 <- select(fileInsee2,-c(libelleEpci))
fileInsee5 <- select(fileInsee5,-c(libelleEpci))
fileInsee6 <- select(fileInsee6,-c(libelleEpci))
fileInsee7 <- select(fileInsee7,c(codeEpci,homme2013,femme2013))
pacaEpci <- left_join(pacaEpci,fileInsee1,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee2,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee3,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee4,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee5,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee6,by=c("codeEpci"))
pacaEpci <- left_join(pacaEpci,fileInsee7,by=c("codeEpci"))
summary(pacaEpci)
#############################################################
# TRAITEMENT DES FICHIERS EPCI                        #
#############################################################
# Emploi: pas le salaire horaire
# Surface : pas la typologie 1-urbain/2-periurbain/3-rural
# Pas de Transport : pas la mobilité - déplacement principal domicile - travail / domicile - lieu d'étude
# Pas d'entreprises
# enntot: nb total d'entreprises - ennbe: industrie - enncfz: construction - ennoq: publiques
# enngz: commerce - enngu: Transport+commerce : retirer les entreprises de commerce (enngz) 
# Pas de logement : nb logement, résidences principales / secondaires
# pas de transport : nb Menage + nb de voiture par ménage


############################################################################################################
# TRAITEMENT DES FICHIER ENERGIE 
# PV, chaufferies, solaire thermique
# années : 2009 2010 2011 2012 2013 2014
# nb d'installations, surface en m2
# évolution entre 2009 et 2014 ? possible sur la surface et le nb d'installations
############################################################################################################
setwd("F:/Flexgrid/ObservatoirePACA/Export_avril2018/EPCI")
fileEpci1 <- read.csv2("export_com_bois_EPCI2017.csv",dec= ".", encoding = "UTF-8")
fileEpci2 <- read.csv2("export_com_pv_EPCI2017.csv",dec= ".", encoding = "UTF-8")
fileEpci3 <- read.csv2("export_com_solther_EPCI2017.csv",dec= ".", encoding = "UTF-8")

#changement de nom des colonnes
fileEpci1 <- rename(fileEpci1,codeEpci = epci2017)
fileEpci1 <- rename(fileEpci1,nbChaufferies = nombre_chaufferies)
fileEpci1 <- rename(fileEpci1,puissanceChaufferiekw = puiss_kw)
fileEpci1 <- rename(fileEpci1,consoBoisTonnes = conso_bois_tonnes)
fileEpci2 <- rename(fileEpci2,codeEpci = epci2017)
fileEpci2 <- rename(fileEpci2,nbPV = nb_pv)
fileEpci2 <- rename(fileEpci2,puissancePVmw = puiss_mw)
fileEpci3 <- rename(fileEpci3,codeEpci = epci2017)
fileEpci3 <- rename(fileEpci3,nbInstalPV = nb_inst)
fileEpci3 <- rename(fileEpci3,surfacePVm2 = surface_m2)
#Constitution d'un seul fichier
epciEnergie <- left_join(fileEpci1,fileEpci2,by=c("codeEpci","annee"))
epciEnergie <- left_join(epciEnergie,fileEpci3,by=c("codeEpci","annee"))
head(epciEnergie)
summary(epciEnergie)
#allEpci <- distinct(epciEnergie,codeEpci)
#nbPV factor => integer
#changement d'unite pour les puissances kw => MW
epciEnergie <- mutate(epciEnergie, puissanceChaufferiemw = ifelse(puissanceChaufferiekw==0,0,round(puissanceChaufferiekw / 1000,4)))
epciEnergie <- mutate(epciEnergie, puissancePVmw = ifelse(puissancePVmw==0,0,round(puissancePVmw,3)))
epciEnergie$puissanceChaufferiekw <- NULL
##### TRAITEMENT pour classification EPCI
energieEpci <- filter(epciEnergie, annee==2013)
energieEpci$annee <- NULL
pacaEpci <- inner_join(pacaEpci,energieEpci,by=c("codeEpci"))
#######################################
# SAUVEGARDE FICHIER EPCI avec 35 variables
write.csv2(pacaEpci, file="F:/Flexgrid/Sprint2/pacaEpci.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
#
##### MODIFICATION pour ES
#nbChaufferies - nbPVBati - nbInstall => nbInstal
#puissanceChaufferie - puissancePV => 
#homogeneisation des colonnes  nbInstall / puissanceMW + ajout du detailFiliere
addFiliereColonneAndSurface <- function(data,typeENR,ssType,colnb,colpuissance,colsurface) {
  msg <- paste(colnb, colpuissance, colsurface)
  print(msg)
  if (colsurface=="NA")
  {
    print("2 colonnes : nb, puissance")
    comtype <- select(data,codeEpci,annee,colnb,colpuissance)
    comtype <- add_column(comtype,surface=0)
  }
  else 
  {
    print("3 colonnes : nb, puissance, surface")
    comtype <- select(data,codeEpci,annee,colnb,colpuissance,colsurface)
  }
    comtype <- add_column(comtype,detailFiliere=typeENR)
    comtype <- add_column(comtype,typeFiliere=ssType)
    nomcol<-c("codeEpci","annee","nbInstal","puissanceMW","surfacem2","detailFiliere","typeFiliere")
    colnames(comtype)<-nomcol
  return (comtype)
}
newEpciEnergie <- addFiliereColonneAndSurface(epciEnergie,"Bois","énergie-chaufferie","nbChaufferies","puissanceChaufferiemw","NA") 
newEpciEnergie <- rbind(newEpciEnergie,addFiliereColonneAndSurface(epciEnergie,"Solaire photovoltaïque","Bati","nbPV","puissancePVmw","surfacePVm2"))

#ajouter le referentiel
epciref<-read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielEpci.csv",encoding = "UTF-8")
newEpciEnergie <- inner_join(newEpciEnergie,epciref,by=c("codeEpci"))

#Remplacement des NA par 0
newEpciEnergie$nbInstal <- replace(newEpciEnergie$nbInstal,is.na(newEpciEnergie$nbInstal),0)
newEpciEnergie$puissanceMW <- replace(newEpciEnergie$puissanceMW,is.na(newEpciEnergie$puissanceMW),0)
newEpciEnergie$surfacem2 <- replace(newEpciEnergie$surface,is.na(newEpciEnergie$surfacem2),0)
summary(newEpciEnergie)
#ecrire le fichier traite
#write.csv2(comEnergie, file="OTTraite/communeEnergie.csv", row.names=FALSE)
write.csv2(newEpciEnergie, file="F:/Flexgrid/Sprint2/epciProductionEnergie.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)


######################################################################################################################################
# ETUDE DES Corrélations
pacaEpci <-read.csv2(file="F:/Flexgrid/Sprint2/20180426pacaEpci.csv", fileEncoding = "UTF-8")

#var <-c("annee1962","annee1968","annee1975","annee1982","annee1990","annee1999","annee2008","annee2013","nbSeveso2012","partSitesNatura2015","Tourisme2016","dureeMoyenneNavette2013","partTC2013","partVoiture2013")
library(psy)

testepci <- select_if(pacaEpci, is.numeric)
summary(testepci)
testepci <- mutate(testepci, superfLabour = ifelse(is.na(superfLabour) ,0,superfLabour))
testepci <- mutate(testepci, superfCulture = ifelse(is.na(superfCulture),0,superfCulture))
testepci <- mutate(testepci, superfHerbe = ifelse(is.na(superfHerbe),0,superfHerbe))
var <- colnames(testepci)
#analyse en composantes principales
#une ACP fonctionne bien en dessous de 10/12 variables
mdspca(testepci[,var])
sphpca(testepci[,var])
#classification ascendante hiérarchique
#fonctionne bien sur un grand nb de variables

library(FactoMineR)
cah <- hclust(dist(t(scale(testepci[,var]))),method="ward.D")
plot(cah,xlab="",ylab="",main="Classification hiérarchique")
library(corrplot)
#Matrice de corrélation avec code couleur
corrplot(cor(testepci[,var],use="complete.obs"),method="circle")

#heatmap : plus c'est foncé, plus c'est corrélé
obj <- cor(testepci[,var], use="pairwise.complete.obs")
heatmap(obj, col=gray(seq(1,0,length=16)))

#decoupage en classes de variables quantitatives
#ERROR in as.data.frame(data.clust[, -2]) : object 'data.clust' not found
don.quali <- testepci
for(i in 1:ncol(don.quali)) {
  vari = don.quali[,i]
  res.hcpc=HCPC(vari,nb.clust=-1, graph=FALSE)
  maxi = unlist(by(res.hcpc$data.clust[,1],res.hcpc$data.clust[,2],max))
  breaks=c(min(vari),maxi)
  aaQuali = cut(vari,breaks,include.lowest=TRUE)
  don.quali[,i] = aaQuali
}
#équiprobable
nb.classes <- 10
coupure <- quantile(testepci$pop2013,seq(0,1,1/nb.classes))
Xqual <- cut(testepci$pop2013,coupure,include.lowest=TRUE)
summary(Xqual)
#histogramme
hist(testepci$pop2013, col="blue", main="Histogramme de la population",freq=TRUE,xlab="pop2013",nclass=20)
#k-means clustering for this dataset with k=10
library(datasets)
summary(testepci)
testepci <- select(testepci, -c(surfacePVm2,puissanceChaufferiemw))
testepci <- select(testepci, -c(nbChaufferies,consoBoisTonnes))
set.seed(7)
ClustersKmeans <- kmeans(testepci,centers=5)
#plot results
plot(testepci, col=(ClustersKmeans$cluster +1), main="K-Means with 5 clusters",pch=20, cex=2)

# Check for the optimal number of clusters given the data
mydata <- testepci
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#==> ? 3 ou 6 ?
# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(mydata, 6, nstart=100)

# Examine the result of the clustering algorithm
km2
# Plot results
plot(mydata$codeEpci, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

clusterData <- select(testepci,c(codeEpci,dep,nbCommune))
clusterData <- add_column(clusterData,kmean=km2$cluster)
table(km2$cluster)
#ajouter le referentiel
epciref<-read.csv2("F:/Flexgrid/Sprint2/referentielEpciGeo.csv",encoding = "UTF-8")
clusterEpci <- left_join(clusterData,epciref,by=c("codeEpci"))

#stocker le clustering des epci avec Geo
write.csv2(clusterEpci, file="F:/Flexgrid/Sprint2/clusterEpciGeo.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

###########################################################################################################
# REPRESENTATION DES CLUSTER SUR UNE CARTOGRAPHIE
###########################################################################################################
#affectation d'une couleur pour chaque niveau
library("RgoogleMaps")
#calcul du centre de la carte
# clusterEpci<-read.csv2("F:/Flexgrid/Sprint2/clusterEpciGeoComplet.csv",dec= ".", sep=";",encoding = "UTF-8")
clusterEpci<-read.csv2("F:/Flexgrid/Sprint2/20180426clusterEpciGeo.csv",dec= ".", sep=",",encoding = "UTF-8")
# clusterEpci<-read.csv2("F:/Flexgrid/Sprint2/clusterEpciGeoConso.csv",dec= ".", sep=",",encoding = "UTF-8")
# clusterEpci<-read.csv2("F:/Flexgrid/Sprint2/clusterEpciGeoProd.csv",dec= ".", sep=",",encoding = "UTF-8")
#clusterEpci<-read.csv2("F:/Flexgrid/Sprint2/clusterEpciGeoAll.csv",dec= ".", sep=",",encoding = "UTF-8")
centre <- c(mean(range(clusterEpci$lat)),mean(range(clusterEpci$lon)))
#zoom : entre 0 et 19
zoom <- 8
#Fond de carte
MaCarte <- GetMap(center=centre,zoom=zoom,destfile="paca.png")
clusterEpci$kmeancol <- as.factor(as.character(clusterEpci$kmean))
table(clusterEpci$kmean)
#levels(clusterEpci$kmeancol) <- c("#FF0000FF","#FFFF00FF","#00FF00FF","#00FFFFFF","#0000FFFF","#FF00FFFF")
levels(clusterEpci$kmeancol) <- c("green","yellow2","orange2","blue","red","red3")
table(clusterEpci$kmeancol)

PlotOnStaticMap(MaCarte, lat=clusterEpci$lat, lon=clusterEpci$lon,
                cex=log(clusterEpci$nbCommune)*2, pch=16, col=as.character(clusterEpci$kmeancol), add=F)
#Ajout d'une donnée complémentaire : le code Epci
TextOnStaticMap(MaCarte, lat=clusterEpci$lat, lon=clusterEpci$lon,labels=clusterEpci$codeEpci, add=TRUE)
#Ajout d'une donnée complémentaire : le nombre de communes
TextOnStaticMap(MaCarte, lat=clusterEpci$lat, lon=clusterEpci$lon,labels=clusterEpci$nbCommune, add=TRUE)
library(RColorBrewer)
My.pal <- brewer.pal(6, "Reds")
legend("topleft", legend = "légende", fill = My.pal, title = "Clusterisation des Epci", bg = "white")
############################################################################################################
library(maptools)
############################################################################################################
library(leaflet)
pal <- colorFactor (c("#FF0000FF", "#FFFF00FF", "#00FF00FF", "#00FFFFFF",
                      "#0000FFFF", "#FF00FFFF"), domain = clusterEpci$kmeancol)
m <- leaflet (clusterEpci) %>%
  addTiles () %>%
  setView ( lng=5, lat=45,zoom = 5)%>%
  addCircleMarkers (
    radius = ~1,
    color=~pal(kmean)
  ) %>%
  addLegend (title= "classification",position = "bottomright",
             pal=pal,values=clusterEpci$kmean,labels=(clusterEpci$libelleEpci))

###############################################################################################################
# 27 avril 2018 : CLUSTERING Production et Consommation et All
###############################################################################################################
pacaEpci <- read.csv2("F:/Flexgrid/Sprint2/20180426pacaEpci.csv",encoding = "UTF-8")
prodEpci <- read.csv2("F:/Flexgrid/Sprint2/epciProduction.csv",encoding = "UTF-8")
consoEpci <- read.csv2("F:/Flexgrid/Sprint2/epciConsommation.csv",encoding = "UTF-8")

#ESSAI avec pacaEpci + prodEpci => 40 variables => dans clusterEpciGeoProd.csv
testProdEpci <- left_join(pacaEpci,prodEpci,by=c("codeEpci"))
testProdEpci <- select_if(testProdEpci, is.numeric)
summary(testProdEpci)
#ESSAI avec pacaEpci + consoEpci =>  49 variables => dans clusterEpciGeoConso.csv
testConsoEpci <- left_join(pacaEpci,consoEpci,by=c("codeEpci"))
testConsoEpci <- select_if(testConsoEpci, is.numeric)
summary(testConsoEpci)
#ESSAI avec pacaEpci + prodEpci + consoEpci => 55 variables => dans clusterEpciGeoAll.csv
testAllEpci <- left_join(pacaEpci,prodEpci,by=c("codeEpci"))
testAllEpci <- left_join(testAllEpci,consoEpci,by=c("codeEpci"))
testAllEpci <- select_if(testAllEpci, is.numeric)
summary(testAllEpci)
#k-means clustering for this dataset with k=10
library(datasets)
# Check for the optimal number of clusters given the data
mydata <- testAllEpci
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#==> ? 3 ou 6 ?
# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
#prod
mydata <- testProdEpci
km2Prod = kmeans(mydata, 6, nstart=100)
#conso
mydata <- testConsoEpci
km2Conso = kmeans(mydata, 6, nstart=100)
#all
mydata <- testAllEpci
km2All = kmeans(mydata, 6, nstart=100)
# Examine the result of the clustering algorithm
km2Prod
km2Conso
km2All
# Plot results
plot(mydata$codeEpci, col =(km2Prod$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
plot(mydata$codeEpci, col =(km2Prod$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
plot(mydata$codeEpci, col =(km2Prod$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
#Répartition
table(km2Prod$cluster)
table(km2Conso$cluster)
table(km2All$cluster)
#Ajout des 3 classements
clusterData <- select(testAllEpci,c(codeEpci,dep,nbCommune))
clusterData <- add_column(clusterData,kmeanProd=km2Prod$cluster)
clusterData <- add_column(clusterData,kmeanConso=km2Conso$cluster)
clusterData <- add_column(clusterData,kmeanAll=km2All$cluster)

#ajouter le referentiel
epciref<-read.csv2("F:/Flexgrid/Sprint2/referentielEpciGeo.csv",encoding = "UTF-8")
clusterEpci <- left_join(clusterData,epciref,by=c("codeEpci"))

#stocker le clustering des epci avec Geo
write.csv2(clusterEpci, file="F:/Flexgrid/Sprint2/clusterEpciGeoComplet.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
