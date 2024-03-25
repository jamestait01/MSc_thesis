#load main HES record-level dataset
hes_main <- read.delim("/rds/general/project/hda-22-23/live/TDS/General/Data/hesin.txt")

#load HES diagnoses dataset
hes_diag <- read.delim("/rds/general/project/hda-22-23/live/TDS/General/Data/hesin_diag.txt")
#Define episode ID to link the two HES files
hes_main$episode_id <- paste0(hes_main$eid, "_", hes_main$ins_index)
hes_diag$episode_id <- paste0(hes_diag$eid, "_", hes_diag$ins_index)
rownames(hes_main) <- hes_main$episode_id

library(dplyr)

#load baseline dataset 
baseline_data <- readRDS("~/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

#only keep hospitalisation data belonging to our cohort
hes_diag <- hes_diag[hes_diag$eid %in% baseline_data$eid,]
hes_main <- hes_main[hes_main$eid %in% baseline_data$eid,]


#keep all hospitalisations with CVD-related codes
icd10_codes <- c("G45","I20","I21","I22","I23","I24","I25","I50","I60","I61","I62",
                 "I63","I64","I65","I66","I67")

#428 = heart failure, 410-414 = heart disease, 430-438 = cerebrovascular disease/TIA
icd9_codes <- c("410","411","412","413","414","428","430","431","432","433","434","435","436",
                "437","438")

#replace "" with NA
hes_diag$diag_icd9<- ifelse(hes_diag$diag_icd9=="",NA,hes_diag$diag_icd9)
hes_diag$diag_icd10<- ifelse(hes_diag$diag_icd10=="",NA,hes_diag$diag_icd10)

#create empty df
CVD_diags <- data.frame()
for (row in 1:nrow(hes_diag)) {
  if (!is.na(hes_diag$diag_icd10[row])) {
    #Check for matching codes
    matches <- grepl(paste(icd10_codes, collapse = "|"), hes_diag$diag_icd10[row])
  } else {
    matches <- grepl(paste(icd9_codes, collapse = "|"), hes_diag$diag_icd9[row])
  }
  
  # If there is a match, add the row to the CVD_diags dataframe
  if (any(matches)) {
    CVD_diags <- rbind(CVD_diags, hes_diag[row, , drop = FALSE])
  }
}
  
summary(as.factor(CVD_diags$diag_icd10))

saveRDS(CVD_diags,"~/Summer_project/baseline_models/model_3a/proper_HES/CVD_hospitalisations.rds")

#we need to keep hospitalisations up until 04-05-2017 or 01-08-2016 as that's how long
#we have prescription data up to...

#convert dates into date format
hes_main$epistart <- as.Date(hes_main$epistart,format="%d/%m/%Y")
hes_main$epiend <- as.Date(hes_main$epiend,format="%d/%m/%Y")
hes_main$elecdate <- as.Date(hes_main$elecdate,format="%d/%m/%Y")
hes_main$admidate <- as.Date(hes_main$admidate,format="%d/%m/%Y")
hes_main$disdate <- as.Date(hes_main$disdate,format="%d/%m/%Y")

#find the earliest date of hospital admission for each episode
hes_main$mindate <-apply(hes_main[c("epistart","epiend","admidate","disdate")], 1, min,na.rm=T)

#extract dates of recruitment, eids and data providers for each person
dates <- subset(baseline_data,select=c("eid","date_recr","data_provider"))
#add max_date for hospitalisation extraction depending on data provider (date pres data available up to)
dates$max_date <- as.Date(ifelse(dates$data_provider==2 | is.na(dates$data_provider),
                                 "2017-05-04",
                                 "2016-08-01"))
#merge the 2 datasets
hes_main <- merge(hes_main,dates,by="eid")

#only keep hospitalisations up until end of study
hes_main <- hes_main %>%
  filter(mindate <= max_date)

hes_main$mindate <- as.Date(hes_main$mindate)
#check to see if each hospitalisation was before or after recruitment
hes_main$hosp_after_recr <- ifelse(hes_main$mindate > hes_main$date_recr,1,0)
#convert nas to 0s (never hospitalised for asthma) 
hes_main$hosp_after_recr <- ifelse(is.na(hes_main$hosp_after_recr),0,hes_main$hosp_after_recr)

#subset hes_main to only include cvd-related hospitalisations
CVD_main <- hes_main[hes_main$episode_id %in% CVD_diags$episode_id,]
  
library(dplyr)
CVD_counts <- CVD_main %>%
  group_by(eid) %>%
  summarise(sum(hosp_after_recr))

CVD_counts <- CVD_counts %>%
  rename("No. of CVD hosp" ="sum(hosp_after_recr)")

#Repeat for total hospitalisations

total_counts <- hes_main %>%
  group_by(eid) %>%
  summarise(sum(hosp_after_recr))

total_counts <- total_counts %>%
  rename("No. of total hosp" ="sum(hosp_after_recr)")

baseline_data <- merge(baseline_data,total_counts,by="eid",all.x=T)
baseline_data <- merge(baseline_data,CVD_counts,by="eid",all.x=T)

#convert NAs into 0s (no cvd hospitalisations)
baseline_data$`No. of CVD hosp` <- ifelse(is.na(baseline_data$`No. of CVD hosp`),0,baseline_data$`No. of CVD hosp`)

saveRDS(baseline_data,"~/Summer_project/baseline_models/model_3a/proper_HES/baseline_data.rds")
