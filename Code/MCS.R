
library("tidyverse")

# queste sono già preprocessate per i pazienti che hanno acquistato almeno uno dei farmaci di interesse

load("~/Documents/Applied Statistics/Project/Materiale_appstat/final dataframes/final_comorbidita.RData")

load("~/Documents/Applied Statistics/Project/Materiale_appstat/final dataframes/final_hospitalization.RData")

load("~/Documents/Applied Statistics/Project/Materiale_appstat/final dataframes/final_prescriptions.RData")

# abbiamo tre tipi di informazioni da ottenere
# qelle direttamente dalle comorbidità previste -> final_comorbidita  con diversi weight
# quelle ottenibili dalle prescrizioni -> dal dataset di partenza
#quelle ottenibili dai dati di ospitalizzazione, però bisogna covertire i codici CCS -> da final_hospitalization 

# dobbiamo creare un nuovo datased di precrizioni con tutte le prescrizioni dei pazienti di interesse

setwd("/Users/macbookpro/Documents/Applied Statistics/Project/Materiale_appstat/Database")
load("~/Documents/Applied Statistics/Project/Materiale_appstat/Database/HF.Rdata")

prescriptions <- subset(HF.2006_2012, tipo_prest == 30)
prescriptions <- prescriptions[(prescriptions$COD_REG %in% final_comorbidita$COD_REG), ]
prescriptions <- prescriptions[,c(1:10)]
prescriptions <- prescriptions[,c(1,2,7,9,10)]

# controlliamo che nessuna prescrizione sia vouta 
#table(prescriptions$qt_prest_Sum)
# ok possiamo togliere quantità
prescriptions <- prescriptions[,c(1:4)]

#diamo i nomi giusti alle colonne
colnames(prescriptions)[3] <- "date_prescription"
colnames(prescriptions)[4] <- "ATC_code"

# ci interessano le prescrizioni inerenti al primo anno del paziente
prescriptions <- prescriptions %>% filter(date_prescription - data_rif_ev <= 365)

# selezioniamo soltanto i farmaci relativi alle malattie che vogliamo stimare

prescriptions <- subset(prescriptions, grepl("^J04AB|^N05B|^N05C|^L01|^C07AB05|^L03AC|^L03AA|^A04|^N04B|^M01BA|^M01CB|^P01BA02|^N03AA|^N03AB02|^N03AB05|^N03AB52|^N03AX|^A02",ATC_code))
# soltanto 12 pazienti tutti aventi Malignancy medication

# per quanto riguarda le ospitalizzazioni siamo interessati alle ospitalizzazioni nel primo anno

hospitalization <- final_hospitalization %>% filter(date_discharge - data_rif_ev <= 365)

# ci interessa solo la CCS diagnosis per questo
hospitalization <- hospitalization[,c(1,9)]
colnames(hospitalization)[2] <- "CCS_principal_diagnosis"

# poichè con i codici ATC molte comorbitidà non erano preesenti, vvediamo se le abbiamo sotto forma di CCS
# estraiamo il codice e lo trasformo in numerico
hospitalization$CCS_principal_diagnosis <- as.numeric(substr(hospitalization$CCS_principal_diagnosis,start = 1, stop= 4))

# selezioniamo solo i codici che ci interessano
cod_interest = c(1,79,38,158,653,109,54,83,139,100,96,58,48,202,114,115)
hospitalization <- hospitalization[(hospitalization$CCS_principal_diagnosis %in% cod_interest), ]

# creiamo il dataframe con le diverse categorie

n <- length(final_comorbidita$COD_REG)

new_comorbidita <- data.frame(COD_REG =  final_comorbidita$COD_REG,tubercolosis = rep(0,n), parkinson = rep(0,n), lymphoma = rep(0,n), kidneyialysis = rep(0,n), 
                              neurological = rep(0,n), arthritis = rep(0,n), cerebrovasc = rep(0,n), vascular = rep(0,n), 
                              gout = rep(0,n), epilespsy = rep(0,n), ulcer = rep(0,n), mi = rep(0,n), valvular = rep(0,n), 
                              obesity = rep(0,n), hypothyroidism = rep(0,n))

# assegnamo a ogni paziente le corrispondenti comorbidità

for( i in 1:dim(hospitalization)[1]){
  if(hospitalization[i,"CCS_principal_diagnosis"] == 1){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"tubercolosis"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 79){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"parkinson"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 38){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"lymphoma"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 158){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"kidneyialysis"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 653){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"neurological"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 202){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"arthritis"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 109){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"cerebrovasc"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 114){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"vascular"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 115){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"vascular"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 54){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"gout"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 83){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"epilespsy"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 139){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"ulcer"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 100){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"mi"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 96){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"valvular"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 58){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"obesity"] = 1
  }
  else if(hospitalization[i,"CCS_principal_diagnosis"] == 48){
    new_comorbidita[new_comorbidita$COD_REG == hospitalization[i,"COD_REG"],"hypothyroidism"] = 1
  }
}

# aggiungiamo una colonna per malignancy medication ottenuta dagli ATC

new_comorbidita$malignancy_med <- rep(0,n)
# sono solo tre pazienti
new_comorbidita[new_comorbidita$COD_REG == 14434084,"malignancy_med"] = 1
new_comorbidita[new_comorbidita$COD_REG == 14755323,"malignancy_med"] = 1
new_comorbidita[new_comorbidita$COD_REG == 22131355,"malignancy_med"] = 1

#uniamo i due dataframes
total_comorbidità <-final_comorbidita[,c(1,3:22)]
total_comorbidità <- total_comorbidità %>% inner_join(new_comorbidita,by = "COD_REG")

# calcoliamo l'MCS

weigts <- c(18,4,6,4,6,5,11,10,1,2,5,2,3,4,8,1,8,0,0,0,10,5,5,4,3,3,3,2,2,2,2,1,1,1,1,5)

MCS<-function(df_row,w){
  return(as.numeric(df_row[2:37])%*%w)
}

res = rep(0,dim(total_comorbidità)[1])

for(i in 1:dim(total_comorbidità)[1]){
  res[i] = MCS(total_comorbidità[i,],weigts)
}

# ora dobbiamo raggruppare in 5 porzioni  0–4, 5–9, 10–14, 15–19 and ≥20

scores <- cut(res,c(-0.5,4.5,9.5,14.5,19.5,65),labels = c(0,1,2,3,4))

load("~/Documents/Applied Statistics/Project/Materiale_appstat/final dataframes/final_data.RData")

# selezioniamo le covariatte che non devono cambiare
data <- final_data[,c(1:7,10:12,17:22)]

# aggiungiamo i nuovi comorbidity scores
data$MCS <- scores

# riordiniamo i nomi
data <-data[,c(1:7,17,8:16)]

# saslviamo i nuovi dati

setwd("/Users/macbookpro/Documents/Applied Statistics/Project/Materiale_appstat/final dataframes")
save(data,file = "data.RData")

save(total_comorbidità,file = "total_comorbidità.RData")
