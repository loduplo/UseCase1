library(dplyr)
library(tidyverse)

#Ajout du referenciel [commune] ou epci
# epciref<-read.csv2("F:/Flexgrid/TalendREFERENTIEL/20180309epci.csv",encoding = "UTF-8")
# epcisoc<-read.csv2("F:/Flexgrid/ObservatoirePACA/ExportIndicateursObsRegional2016/EPCI/ExportSocialEPCI2016.csv",encoding="UTF-8")
# Ajout du referentiel EPCI au fichier soc
# epcisoc <- left_join(epcisoc, epciref, by=c("codeEPCI"))
#F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
addRef <- function(data,dataref,colRef) {
  data <- left_join(data,dataref,by=c(colRef))
}
# epcisoc <- addRefEpci(epcisoc, epciref, "codeEPCI")
# epcisoc <- delColByName(epcisoc,"epci2016")

#############################################################
##  TRAITEMENT SRCAE : 2020 et 2030
#############################################################
objpcaet <- read.csv2("F:/Flexgrid/PCAET/ObjectifsPCAET.csv",encoding = "UTF-8")
summary(objpcaet)

##Les valeurs sont des pourcentage => division par 100
### BOIS : 
### PACA Puissance  2020 : 50 2030:  250
### PACA Production 2020 : 100 2030: 500 en GWh !!!! => mis en MWh
objpcaet <- mutate(objpcaet, boisMW2020 = round(BOIS_median_2020 * 50/100,3))
objpcaet <- mutate(objpcaet, boisMW2030 = round(BOIS_median_2030 * 250/100,3))
objpcaet <- mutate(objpcaet, boisMWh2020 = round(BOIS_median_2020 * 100/100,3)*1000)
objpcaet <- mutate(objpcaet, boisMWh2030 = round(BOIS_median_2030 * 500/100,3)*1000)
objpcaet$BOIS_median_2020 <- NULL
objpcaet$BOIS_median_2030 <- NULL
summary(objpcaet)
### FORET : 
### PACA Puissance  2020 : ? 2030: ?
### PACA Production 2020 : 610 2030: 1030
#objpcaet <- mutate(objpcaet, GdeHydroMW2020 = round(FORET_median_2020 * 3500/100,3))
#objpcaet <- mutate(objpcaet, GdeHydroMW2030 = round(FORET_median_2030 * 3600/100,3))
objpcaet <- mutate(objpcaet, foretMWh2020 = round(FORET_median_2020 * 610/100,3)*1000)
objpcaet <- mutate(objpcaet, foretMWh2030 = round(FORET_median_2030 * 1030/100,3)*1000)
objpcaet$FORET_median_2020 <- NULL
objpcaet$FORET_median_2030 <- NULL
summary(objpcaet)
### BIOMASSE : 
### PACA Puissance  2020 : 110 2030: 330
### PACA Production 2020 : 230 2030: 660
objpcaet <- mutate(objpcaet, biomasseMW2020 = round(BIOM_median_2020 * 110/100,3))
objpcaet <- mutate(objpcaet, biomasseMW2030 = round(BIOM_median_2030 * 330/100,3))
objpcaet <- mutate(objpcaet, biomasseMWh2020 = round(BIOM_median_2020 * 230/100,3)*1000)
objpcaet <- mutate(objpcaet, biomasseMWh2030 = round(BIOM_median_2030 * 660/100,3)*1000)
objpcaet$BIOM_median_2020 <- NULL
objpcaet$BIOM_median_2030 <- NULL
summary(objpcaet)
### ASSAINISSEMENT : 
##NON EXPLOITE
##ASSAIN_NB_STEP / ASSAIN_NB_COLL / ASSAIN_SURF_IC_M2_SHON
### PACA Puissance  2020 : 110 2030:  270
### PACA Production 2020 : 490 2030: 1200
objpcaet <- mutate(objpcaet, assainMW2020 = round(ASSAIN_median_2020 * 110/100,3))
objpcaet <- mutate(objpcaet, assainMW2030 = round(ASSAIN_median_2030 * 270/100,3))
objpcaet <- mutate(objpcaet, assainMWh2020 = round(ASSAIN_median_2020 * 490/100,3)*1000)
objpcaet <- mutate(objpcaet, assainMWh2030 = round(ASSAIN_median_2030 * 1200/100,3)*1000)
objpcaet$ASSAIN_median_2020 <- NULL
objpcaet$ASSAIN_median_2030 <- NULL
summary(objpcaet)
### THALASSOTHERMIE : 
##NON EXPLOITE
##Nb_instal_2020 / Nb_instal_2030
### PACA Puissance  2020 : 17 2030: 115
### PACA Production 2020 : 50 2030: 420
objpcaet <- mutate(objpcaet, thalassoMW2020 = round(MW_2020 * 17/100,3))
objpcaet <- mutate(objpcaet, thalassoMW2030 = round(MW_2030 * 115/100,3))
objpcaet <- mutate(objpcaet, thalassoMWh2020 = round(GWh_2020 * 50/100,3)*1000) ##ATTENTION GWh !
objpcaet <- mutate(objpcaet, thalassoMWh2030 = round(GWh_2030 * 420/100,3)*1000)##ATTENTION GWh !
objpcaet$MW_2020 <- NULL
objpcaet$MW_2030 <- NULL
objpcaet$MW_2020 <- NULL
objpcaet$MW_2030 <- NULL
summary(objpcaet)
### AEROTHERMIE : 
### PACA Puissance  2020 : 1400 2030: 2200
### PACA Production 2020 : 1400 2030: 2200
objpcaet <- mutate(objpcaet, aeroMW2020 = round(AERO_median_2020 * 1400/100,3))
objpcaet <- mutate(objpcaet, aeroMW2030 = round(AERO_median_2030 * 2200/100,3))
objpcaet <- mutate(objpcaet, aeroMWh2020 = round(AERO_median_2020 * 1400/100,3)*1000)
objpcaet <- mutate(objpcaet, aeroMWh2030 = round(AERO_median_2030 * 2200/100,3)*1000)
objpcaet$AERO_median_2020 <- NULL
objpcaet$AERO_median_2030 <- NULL
summary(objpcaet)
### Solaire Thermique
### PACA Puissance  2020 : 1200 2030: 2800
### PACA Production 2020 :  620 2030: 1400
objpcaet <- mutate(objpcaet, solaireThMW2020 = round(SOLth_median_2020 * 1200/100,3))
objpcaet <- mutate(objpcaet, solaireThMW2030 = round(SOLth_median_2030 * 2800/100,3))
objpcaet <- mutate(objpcaet, solaireThMWh2020 = round(SOLth_median_2020 * 620/100,3)*1000)
objpcaet <- mutate(objpcaet, solaireThMWh2030 = round(SOLth_median_2030 * 1400/100,3)*1000)
objpcaet$SOLth_median_2020 <- NULL
objpcaet$SOLth_median_2030 <- NULL
summary(objpcaet)
### GEOTHERMIE : 
### PACA Puissance  2020 : 200 2030: 400
### PACA Production 2020 : 270 2030: 550
objpcaet <- mutate(objpcaet, geoThMW2020 = round(GEOTH_median_2020 * 200/100,3))
objpcaet <- mutate(objpcaet, geoThMW2030 = round(GEOTH_median_2030 * 400/100,3))
objpcaet <- mutate(objpcaet, geoThMWh2020 = round(GEOTH_median_2020 * 270/100,3)*1000)
objpcaet <- mutate(objpcaet, geoThMWh2030 = round(GEOTH_median_2030 * 550/100,3)*1000)
objpcaet$GEOTH_median_2020 <- NULL
objpcaet$GEOTH_median_2030 <- NULL
summary(objpcaet)
### BIOGAZ : 
### PACA Puissance  2020 : 275 2030:  550
### PACA Production 2020 : 550 2030: 1100
objpcaet <- mutate(objpcaet, biogazMW2020 = round(BIOGAZ_median_2020 * 275/100,3))
objpcaet <- mutate(objpcaet, biogazMW2030 = round(BIOGAZ_median_2030 * 550/100,3))
objpcaet <- mutate(objpcaet, biogazMWh2020 = round(BIOGAZ_median_2020 * 550/100,3)*1000)
objpcaet <- mutate(objpcaet, biogazMWh2030 = round(BIOGAZ_median_2030 * 1100/100,3)*1000)
objpcaet$BIOGAZ_median_2020 <- NULL
objpcaet$BIOGAZ_median_2030 <- NULL
summary(objpcaet)
### PV_BATI : 
### PACA Puissance  2020 : 1150 2030: 2250
### PACA Production 2020 : 1380 2030: 2680
##objpcaet <-select(objpcaet,c("codeCommune","PV_bati_median_2020","PV_bati_median_2030"))
objpcaet <- mutate(objpcaet, batiPVMW2020 = round(PV_bati_median_2020 * 1150/100,3))
objpcaet <- mutate(objpcaet, batiPVMW2030 = round(PV_bati_median_2030 * 2250/100,3))
objpcaet <- mutate(objpcaet, batiPVMWh2020 = round(PV_bati_median_2020 * 1380/100,3)*1000)
objpcaet <- mutate(objpcaet, batiPVMWh2030 = round(PV_bati_median_2030 * 2680/100,3)*1000)
objpcaet$PV_bati_median_2020 <- NULL
objpcaet$PV_bati_median_2030 <- NULL
summary(objpcaet)
### PV_SOL : 
### PACA Puissance  2020 : 1150 2030: 2200
### PACA Production 2020 : 1380 2030: 2600
##objpcaet <-select(objpcaet,c("codeCommune","PV_sol_median_2020","PV_sol_median_2030"))
objpcaet <- mutate(objpcaet, solPVMW2020 = round(PV_sol_median_2020 * 1150/100,3))
objpcaet <- mutate(objpcaet, solPVMW2030 = round(PV_sol_median_2030 * 2200/100,3))
objpcaet <- mutate(objpcaet, solPVMWh2020 = round(PV_sol_median_2020 * 1380/100,3)*1000)
objpcaet <- mutate(objpcaet, solPVMWh2030 = round(PV_sol_median_2030 * 2600/100,3)*1000)
objpcaet$PV_sol_median_2020 <- NULL
objpcaet$PV_sol_median_2030 <- NULL
summary(objpcaet)
### GDE_HYDRO : 
### PACA Puissance  2020 : 3500 2030:  3600
### PACA Production 2020 : 9800 2030: 10100
objpcaet <- mutate(objpcaet, gdeHydroMW2020 = round(GDE_HYDRO_median_2020 * 3500/100,3))
objpcaet <- mutate(objpcaet, gdeHydroMW2030 = round(GDE_HYDRO_median_2030 * 3600/100,3))
objpcaet <- mutate(objpcaet, gdeHydroMWh2020 = round(GDE_HYDRO_median_2020 * 9800/100,3)*1000)
objpcaet <- mutate(objpcaet, gdeHydroMWh2030 = round(GDE_HYDRO_median_2030 * 10100/100,3)*1000)
objpcaet$GDE_HYDRO_median_2020 <- NULL
objpcaet$GDE_HYDRO_median_2030 <- NULL
summary(objpcaet)
### PTE_HYDRO : 
### PACA Puissance  2020 : 250  2030:  270
### PACA Production 2020 : 1100 2030: 1200
objpcaet <- mutate(objpcaet, pteHydroMW2020 = round(PTE_HYDRO_median_2020 * 250/100,3))
objpcaet <- mutate(objpcaet, pteHydroMW2030 = round(PTE_HYDRO_median_2030 * 270/100,3))
objpcaet <- mutate(objpcaet, pteHydroMWh2020 = round(PTE_HYDRO_median_2020 * 1100/100,3)*1000)
objpcaet <- mutate(objpcaet, pteHydroMWh2030 = round(PTE_HYDRO_median_2030 * 1200/100,3)*1000)
objpcaet$PTE_HYDRO_median_2020 <- NULL
objpcaet$PTE_HYDRO_median_2030 <- NULL
summary(objpcaet)
### EOL_TERR : 
### PACA Puissance  2020 : 545  2030: 1245
### PACA Production 2020 :1300  2030: 2860
objpcaet <- mutate(objpcaet, eolTerrMW2020 = round(EOL_TERR_median_2020 * 545/100,3))
objpcaet <- mutate(objpcaet, eolTerrMW2030 = round(EOL_TERR_median_2030 * 1245/100,3))
objpcaet <- mutate(objpcaet, eolTerrMWh2020 = round(EOL_TERR_median_2020 * 1300/100,3)*1000)
objpcaet <- mutate(objpcaet, eolTerrMWh2030 = round(EOL_TERR_median_2030 * 2860/100,3)*1000)
objpcaet$EOL_TERR_median_2020 <- NULL
objpcaet$EOL_TERR_median_2030 <- NULL
summary(objpcaet)
###SUPPRESSION DES COLONNES INUTILES
objpcaet$Nb_instal_2020 <- NULL
objpcaet$GWh_2020 <- NULL
objpcaet$Nb_instal_2030 <- NULL
objpcaet$GWh_2030 <- NULL
#### HORODATAGE
objpcaet <- rename(objpcaet, codeCommune2020 = codeCommune)
### SELECTIONNER Toutes les colonnes qui contiennent 2020 ? et ajouter la date
object2020 <- select(objpcaet,contains("2020"))
col2020 <- colnames(object2020)
nbcol <- length(col2020)
for (num in 1:nbcol)
{col2020[[num]] <- str_sub(col2020[[num]], 1,str_length(col2020[[num]]) -4)}
print(col2020)
colnames(object2020)<-col2020
object2020 <- add_column(object2020,date=2020)
objpcaet <- rename(objpcaet, codeCommune2030 = codeCommune2020)
### Puis selectionner toutes les colonnes qui contiennent 2030 ?
object2030 <- select(objpcaet,contains("2030"))
col2030 <- colnames(object2030)
nbcol <- length(col2030)
for (num in 1:nbcol)
{col2030[[num]] <- str_sub(col2030[[num]], 1,str_length(col2030[[num]]) -4)}
print(col2030)
colnames(object2030)<-col2030
object2030 <- add_column(object2030,date=2030)
###rbind objectif2020 et objectif2030
pcaetCom <- rbind(object2020,object2030)
#Transformation colonnes MW + MWh* en 4 colonnes :  productionMwh + puissanceMW + detailFiliere + typeFiliere
addFiliereColonneAndType <- function(data,typeENR,type,colproduction,colpuissance) {
  msg <- paste(colpuissance, colproduction)
  print(msg)
  comtype <- select(data,codeCommune,date,colproduction,colpuissance)
  comtype <- add_column(comtype,detailFiliere=typeENR)
  comtype <- add_column(comtype,typeFiliere=type)
  nomcol<-c("codeCommune","date","productionMWh","puissanceMW","detailFiliere","typeFiliere")
  colnames(comtype)<-nomcol
  return (comtype)
}
# A COMPLETER : autres valeurs Detail filiere
# [1] "Biogaz"                               "Biomasse"                            
# [3] "Centrales thermiques"                 "Cogénération"                        
# [5] "Eolien"                               "Grande hydraulique"                  
# [7] "Incinération des déchets industriels" "Incinération des ordures ménagères"  
# [9] "Petite hydraulique"                   "Pompes à chaleur"                    
# [11] "Réseaux de chaleur"                   "Solaire photovoltaïque"              
# [13] "Solaire thermique"                    "Thermique fossile autre"
newComPcaet1 <- addFiliereColonneAndType(pcaetCom,"Biomasse","","biomasseMWh","biomasseMW") 
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Petite hydraulique","","pteHydroMWh","pteHydroMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Grande hydraulique","","gdeHydroMWh","gdeHydroMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Biogaz","","biogazMWh","biogazMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Eolien","Terrestre","eolTerrMWh","eolTerrMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Solaire thermique","","solaireThMWh","solaireThMW"))
#GARDER une spécificité pour pouvoir la détailler : ici entre sol et Bati
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Solaire photovoltaïque","Sol","solPVMWh","solPVMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Solaire photovoltaïque","Bati","batiPVMWh","batiPVMW"))
# Trouver les filières qui correspondent à
# Bois énergie-chaufferie => thermique
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Bois","énergie-chaufferie","boisMWh","boisMW"))
#newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Exploitation forestière","",foretMWh","XXXXMW"))
#GARDER une spécificité pour pouvoir la détailler : ici on perd la spécificité
#tout est mis en Pompe à Chaleur : PAC
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Pompes à chaleur","Assainissement","assainMWh","assainMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Pompes à chaleur","Thalassothermie","thalassoMWh","thalassoMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Pompes à chaleur","Aerothermie","aeroMWh","aeroMW"))
newComPcaet1 <- rbind(newComPcaet1,addFiliereColonneAndType(pcaetCom,"Pompes à chaleur","Geothermie","geoThMWh","geoThMW"))
#newComPcaet1 <- rbind(newComPcaet1,addFiliereColonne(pcaetCom,"assainMWh","assainMW"))
#newComPcaet1 <- rbind(newComPcaet1,addFiliereColonne(pcaetCom,,"thalassoMWh","thalassoMW"))
#newComPcaet1 <- rbind(newComPcaet1,addFiliereColonne(pcaetCom,"Aerothermie","aeroMWh","aeroMW"))
#newComPcaet1 <- rbind(newComPcaet1,addFiliereColonne(pcaetCom,"Geothermie","geoThMWh","geoThMW"))

# newComPcaet <-select(pcaetCom,codeCommune,date,biomasseMWh,biomasseMW)
# newComPcaet <- add_column(newComPcaet,detailFiliere="Biomasse")
# newComPcaet <- rename(newComPcaet,productionMWh=biomasseMWh)
# newComPcaet <- rename(newComPcaet,puissance=biomasseMW)
newComPcaet1$detailFiliere <- as.factor(newComPcaet1$detailFiliere)
levels(newComPcaet1$detailFiliere)
#Ajouter la grande Filière : ENR
newComPcaet1 <- add_column(newComPcaet1,grandeFiliere="ENR")

###Suppression des NA Remplacé par 0 pour les 2 colonnes
newComPcaet1$productionMWh <- replace(newComPcaet1$productionMWh,is.na(newComPcaet1$productionMWh),0)
newComPcaet1$puissanceMW <- replace(newComPcaet1$puissanceMW,is.na(newComPcaet1$puissanceMW),0)
###Ajout du référentiel
pcaetCom1 <- addRef(newComPcaet1,comRef,"codeCommune")
write.csv2(pcaetCom1, file="F:/Flexgrid/Sprint1/productionPcaetCom.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

###Ajout du référentiel
pcaetCom <- addRef(pcaetCom,comRef,"codeCommune")
write.csv2(pcaetCom, file="F:/Flexgrid/Sprint1/pcaetCom.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
