library(tidyverse)
load("~/Documents/Applied Statistics/Project/Materiale_appstat/Code/Functional/interest_drugs_functional.RData")
load("~/Documents/Applied Statistics/Project/Materiale_appstat/Code/Functional/pazienti.RData")
setwd("/Users/macbookpro/Documents/Applied Statistics/Project/Materiale_appstat/Code/Functional/")

sequenza <- function(patient_data,ATC_code){
  
  seq = rep(0,365)
  
  for(i in 1:dim(patient_data)[1]){
    
    if(patient_data$ATC_code[i] == ATC_code){
      
      duration = patient_data$duration_prescription[i]
      
      data_prescr = patient_data$date_prescription[i]
      
      for(j in 1:duration){
        index = as.numeric(data_prescr - patient_data$data_rif_ev[i]) + j
        if(index < 366){
          seq[index] = 1
        }
      }
    }
  }
  
  return(seq)
}

adherencedf_single <- function(dataframe,drug){
  
  adherence <- NULL
  
  adherence
  codici=unique(dataframe$COD_REG)
  
  for(i in 1:length(codici)){
    
    patient_data <- dataframe[dataframe$COD_REG %in% codici[i], ]
    adherence = cbind(adherence,cumsum(sequenza(patient_data,drug))/(1:365))
    
  }
  
  return(adherence)
}

PDC_AA <- adherencedf_single(interest_drugs_final,"AA")

save(PDC_AA,file = "PDC_AA.RData")

PDC_BB <- adherencedf_single(interest_drugs_final,"BB")

save(PDC_BB,file = "PDC_BB.RData")

PDC_RAS <- adherencedf_single(interest_drugs_final,"RAS")

save(PDC_RAS,file = "PDC_RAS.RData")


PDC_total <- function(patient_data){
  
  pdc_aa = sequenza(patient_data,"AA")
  pdc_bb = sequenza(patient_data,"BB")
  pdc_ras = sequenza(patient_data,"RAS")
  
  aa = sum(pdc_aa)
  bb = sum(pdc_bb)
  ras = sum(pdc_ras)
  
  n = 3 
  
  if(aa == 0){
    n = n-1
  }
  if(bb == 0){
    n = n-1
  }
  if(ras == 0){
    n = n-1
  }
  pdc_seq = (pdc_aa + pdc_bb + pdc_ras)/n
  
  return(cumsum(pdc_seq)/(1:365))
}

adherencedf_total <- function(dataframe){
  
  adherence <- NULL
  
  adherence
  codici=unique(dataframe$COD_REG)
  
  for(i in 1:length(codici)){
    
    patient_data <- dataframe[dataframe$COD_REG %in% codici[i], ]
    new_col=PDC_total(patient_data)
    adherence = cbind(adherence,new_col)
    
  }
  
  return(adherence)
}

PDC_TOT = adherencedf_total(interest_drugs_final)

save(PDC_TOT,file = "PDC_TOT.RData")
