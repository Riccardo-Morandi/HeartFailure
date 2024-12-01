
library("tidyverse")

setwd("/Users/macbookpro/Documents/Applied Statistics/Project/Materiale_appstat/Database")

load("~/Documents/Applied Statistics/Project/Materiale_appstat/Database/HF.Rdata")


dim(HF.2006_2012)
glimpse(HF.2006_2012)

#removing patients younger than 50
id_togliere_eta = HF.2006_2012$COD_REG[HF.2006_2012$eta_Min < "50"]
id_togliere_eta = unique(id_togliere_eta)
new = HF.2006_2012[!(HF.2006_2012$COD_REG %in% id_togliere_eta), ]

#removing observations before 2008
id_togliere_data = new$COD_REG[new$data_rif_ev <= "2008-12-31"]
id_togliere_data = unique(id_togliere_data)
new_new = new[!(new$COD_REG %in% id_togliere_data), ]

#removing patients with followup less than 1 year
HF.2006_2012 = new_new[(new_new$data_studio_out - new_new$data_rif_ev)>365, ]

dim(HF.2006_2012)
glimpse(HF.2006_2012)

#splitting hospitalizations and prescriptions
hospitalization <- subset(HF.2006_2012, tipo_prest == 41)

glimpse(hospitalization)
skimr::skim(hospitalization)

colnames(hospitalization)[7] <- "date_discharge"
colnames(hospitalization)[9] <- "CCS_principal_diagnosis"
colnames(hospitalization)[10] <- "time_in_hospital"
colnames(hospitalization)[12] <- "hospital_id"

names(hospitalization)

pharmacological <- subset(HF.2006_2012, tipo_prest == 30)
glimpse(pharmacological)
skimr::skim(pharmacological)

#renaming variables according to the different datasets
colnames(pharmacological)[7] <- "date_prescription"
colnames(pharmacological)[9] <- "ATC_code"
colnames(pharmacological)[10] <- "duration_prescription"
colnames(pharmacological)[12] <- "ASL_RESIDENZA"

pharmacological <- pharmacological[ , c("COD_REG","data_rif_ev","SESSO","ASL_RESIDENZA",
                                       "data_studio_out", "labelOUT" ,"date_prescription",
                                       "tipo_prest","ATC_code", "duration_prescription", "eta_Min")]

#selecting the drugs that we are interested in 
interest_drugs <- subset(pharmacological, grepl("^C07|^C03D|^C03E|^C09C|^C09D|^C09A|^C09B|^C09X",ATC_code))

#groupping together interest drugs into the 3 categories
filtered_codes <- interest_drugs

filtered_codes$ATC_code <- ifelse(grepl("^C09C|^C09D|^C09A|^C09B|^C09X",filtered_codes$ATC_code), "RAS",
                                  ifelse(grepl("^C07",filtered_codes$ATC_code), "BB", 
                                  ifelse(grepl("^^C03D|^C03E",filtered_codes$ATC_code), "AA",filtered_codes$ATC_code)))

#some consistecy checks
hospitalization_filtered = hospitalization %>% filter(data_rif_ev <= date_discharge) 
#  already verified

filtered_codes_check = filtered_codes %>% filter(data_rif_ev <= date_prescription) 
#  already verified

#filtered_codes=subset(filtered_codes, select=-12)
#ultima_data= filtered_codes %>% group_by(filtered_codes$COD_REG) %>% summarize(ultima_data=max(filtered_codes$date_prescription))
#gruppi = split(filtered_codes, filtered_codes$COD_REG)
#ultime_osservazioni<- lapply(gruppi, function (x) tail(x, n=1))

#Ricavo la prima prescrizione per ogni paziente
prime_prescrizioni=filtered_codes %>% group_by(filtered_codes$COD_REG) %>% slice(1)
prime_ospedalizzazioni=hospitalization %>% group_by(hospitalization$COD_REG) %>% slice(1)
#dal numero di osservazioni di prime_prescrizioni e di prime_ospedalizzazioni
#possiamo capire che ci sono dei pazienti che sono stati ospedalizzati ma che non
#hanno mai avuto nessuna prescrizione
codici_ospedalizzazioni = prime_ospedalizzazioni$COD_REG
codici_prescrizioni = prime_prescrizioni$COD_REG
codici_in_comune = intersect(codici_ospedalizzazioni,codici_prescrizioni)
codici_in_comune = unique(codici_in_comune)
prime_ospedalizzazioni = prime_ospedalizzazioni[prime_ospedalizzazioni$COD_REG %in% codici_in_comune,]
#ora aggiungo la colonna delle prime ospedalizzazioni a prime_prescrizioni
#cioè le prime prescrizioni
prime_prescrizioni$prime_osp = prime_ospedalizzazioni$date_discharge
codici_conalmenounaprescrizione = codici_in_comune[prime_prescrizioni$date_prescription - prime_prescrizioni$prime_osp < "365"]

interest_drugs_final = filtered_codes[filtered_codes$COD_REG %in% codici_conalmenounaprescrizione]
hospitalization_final = hospitalization[hospitalization$COD_REG %in% codici_conalmenounaprescrizione]

#creating the sequences and obtaining the PDC for each drug 

PDC <- function(patient_data,ATC_code){
  
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
  
  return(sum(seq)/365)
}

adherancedf <- function(dataframe){
  
  adherance <- data.frame(COD_REG = unique(dataframe$COD_REG), PDC_AA = rep(0,length(unique(dataframe$COD_REG))),
                          PDC_BB = rep(0,length(unique(dataframe$COD_REG))),  PDC_RAS = rep(0,length(unique(dataframe$COD_REG))))
  
  for(i in 1:length(adherance$COD_REG)){
    
    patient_data <- dataframe[dataframe$COD_REG %in% adherance$COD_REG[i],]
    
    
    adherance$PDC_AA[i] = PDC(patient_data,"AA")
    adherance$PDC_BB[i] = PDC(patient_data,"BB")
    adherance$PDC_RAS[i] = PDC(patient_data,"RAS")
    
  }
  
  return(adherance)
}

# Togliamo gli NA sui tempi di prescrizione
interest_drugs_final <- interest_drugs_final[complete.cases(interest_drugs_final$duration_prescription), ]

# Creazione dataframe con tre colonne con le tre percentuali per i tre farmaci AA BB RAS
PDC_df <- adherancedf(interest_drugs_final)

# computimg the number of different drugs taken by each patient
n_drugs = rep(0,length(PDC_df$COD_REG))

for(j in 2:4){
  for(i in 1:length(PDC_df$COD_REG)){
    if(PDC_df[i,j] !=0){
      n_drugs[i] = n_drugs[i] +1
    }
  }
}

PDC_df$n_drugs = n_drugs

#filtering out the patients that did not take any of the interest drugs in the first year

PDC_df <- PDC_df %>% filter(n_drugs > 0)

# we do the same but considering all of the drugs together
# creo una funzione che restituisce la seuquenza per un determinato paziente e un determinato tipo di farmaco
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

PDC_total <- function(patient_data){
  
  pdc_aa = sequenza(patient_data,"AA")
  pdc_aa
  pdc_bb=sequenza(patient_data,"BB")
  pdc_ras=sequenza(patient_data,"RAS")
  
  aa=sum(pdc_aa)
  bb=sum(pdc_bb)
  ras=sum(pdc_ras)
  
  if(aa>0){
    aa=1
  }
  
  if(bb>0){
    bb=1
  }
  if(ras>0){
    ras=1
  }
  aa
  bb
  ras
  #pdc_seq=rep(0,365)
  #if(aa=1 & bb=1 & ras=1)
  #  pdc_seq=(seq_aa & seq_bb) & seq_ras
  #if(aa=0 & bb=1 & ras=1)
  #  pdc_seq=seq_bb & seq_ras
  #if(aa=1 & bb=0 & ras=1)
  #  pdc_seq=seq_aa & seq_ras
  #if(aa=1 & bb=1 & ras=0)
  #  pdc_seq=seq_aa & seq_bb*/
  if(aa==0){
    pdc_aa=rep(1,365)
  }
  if(bb==0){
    pdc_bb=rep(1,365)
  }
  if(ras==0){
    pdc_ras=rep(1,365)
  }
  pdc_seq=pdc_aa & pdc_bb & pdc_ras
  
  return(sum(pdc_seq)/365)
}



adherencedf_total <- function(dataframe){
  
  adherence <- data.frame(COD_REG = unique(dataframe$COD_REG), PDC = rep(0,length(unique(dataframe$COD_REG))))
  
  for(i in 1:length(adherence$COD_REG)){
    
    patient_data <- dataframe[dataframe$COD_REG %in% adherence$COD_REG[i],]
    adherence$PDC[i] = PDC_total(patient_data,"AA","RAS","BB")
    
  }
  
  return(adherence)
}

# Creazione dataframe con una colonna con le tre percentuali per i tre farmaci AA BB RAS sovrapposte
PDC_df_TOTAL <- adherencedf_total(interest_drugs_final)
PDC_df_TOTAL <- PDC_df_TOTAL[ PDC_df_TOTAL$COD_REG %in% PDC_df$COD_REG, ]

# Creazione dataframe hospitalizations con solo righe presenti in interest_drugs
hospitalization_final <- hospitalization_final[ hospitalization_final$COD_REG %in% PDC_df$COD_REG, ]

# Creazione dataframe tenendo solo l'ultima riga per ogni ospedalizzazione
hosp_last_rows <- aggregate(hospitalization_final[,13:32], by = list(hospitalization_final$COD_REG), FUN = tail, n = 1)

# Creazione dataframe sommando le comorbidità tra di loro e tenendo solo questa colonna
hosp_comorbidita_somma_colonne <- rowSums(hosp_last_rows[, 2:21])

hosp_last_rows$comorbidita = hosp_comorbidita_somma_colonne

somma_trattamenti <- aggregate(hospitalization_final[, 33:36], by = list(COD_REG = hospitalization_final$COD_REG), sum)

somma_time_in_hospital <- aggregate(hospitalization_final[, 10], by = list(COD_REG = hospitalization_final$COD_REG), sum)

# Inserisco in hosp_last_rows le somme dei trattamenti per ogni paziente e il tempo totale di ospedalizzazione
hosp_last_rows$ICD = somma_trattamenti$ICD
hosp_last_rows$SHOCK = somma_trattamenti$SHOCK
hosp_last_rows$CABG = somma_trattamenti$CABG
hosp_last_rows$PTCA = somma_trattamenti$PTCA
hosp_last_rows$time_in_hospital = somma_time_in_hospital$time_in_hospital

save(hosp_last_rows, file = "hosp_last_rows.RData")
save(PDC_df_TOTAL, file = "PDC_df_TOTAL.RData")

# abbiamo un numero diverso di COD_REG nei tre diversi df

n_distinct(hospitalization_final$COD_REG)
n_distinct(interest_drugs_final$COD_REG)
n_distinct(PDC_df$COD_REG)

diff_id = setdiff(hospitalization_final$COD_REG,PDC_df$COD_REG)

#li possiamo togliere perchè hanno solo prescrizioni che hanno durata = NA 
# oppure non hanno farmaci prescritti nel primo anno 
hospitalization_final <- hospitalization_final %>% filter(!COD_REG %in% diff_id)
interest_drugs_final <- interest_drugs_final %>% filter(!COD_REG %in% diff_id)

save(hospitalization_final,file = "hospitalization_final.RData")
save(interest_drugs_final,file = "interest_drugs_final.RData")
save(PDC_df,file = "PDC_df.RData")

# combinig all of the information in a single dataframe having only one row per patient

PDC_df_TOTAL <- PDC_df_TOTAL %>% rename(PDC_TOT = PDC)

PDC_df_TOTAL<- PDC_df_TOTAL %>% filter(COD_REG %in% PDC_df$COD_REG)

PDC_df <- PDC_df %>% inner_join(PDC_df_TOTAL)

PDC_df <- PDC_df %>% select(c(1:4,6,5))

final_data <- hospitalization_final %>% filter(COD_REG %in% PDC_df$COD_REG) %>% 
  group_by(COD_REG) %>% select(c(c(1:6),11)) %>% slice(1) %>% ungroup()

hosp_last_rows <- hosp_last_rows %>% rename(COD_REG = Group.1) %>% select(c(1,27,2:26))

final_data <- final_data %>% inner_join(hosp_last_rows)

# inner join con PDC_df 
final_data <- final_data %>% inner_join(PDC_df)

# togliere i label_out PERSO
final_data <- final_data %>% filter(labelOUT %in% c("TRONCATO","DECEDUTO"))

glimpse(final_data)  

#building datastructures that are all final and no redundant data
final_PDC <- PDC_df %>% filter(COD_REG %in% final_data$COD_REG)

final_hospitalization <- hospitalization_final %>% filter(COD_REG %in% final_data$COD_REG)

final_comorbidita <- hosp_last_rows %>% filter(COD_REG %in% final_data$COD_REG)

final_prescriptions <- interest_drugs_final %>% filter(COD_REG %in% final_data$COD_REG)

#saving the dataframes:

#this is the dataframe with one row per patient with all of the condensed info 
save(final_data,file = "final_data.RData")

# this are the specific dddataframes about complete info on only the patient we are interested in
save(final_PDC,file = "final_PDC.RData")
save(final_hospitalization,file = "final_hospitalization.RData")
save(final_comorbidita,file = "final_comorbidita.RData")
save(final_prescriptions,file = "final_prescriptions.RData")


