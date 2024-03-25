setwd("/rds/general/user/jt1019/home/Summer_project/")

#load field id 41270 data, gives all of the first occurence of icd codes for people in the biobank, along with their date
icd10_data <- readRDS("extraction_and_recoding/outputs/ukb_extracted.rds")

#load our hypertensive cases required for analysis
eids <- readRDS("Data/follow_up1/merged_data.rds")
#keep only eid
eids <- unique(eids$eid)

#subset icd file to only keep hypertensive cases we're interested in.
icd10_data <- icd10_data[rownames(icd10_data) %in% eids,]

#add eid column to data
icd10_data$eid <- rownames(icd10_data)

#save data

saveRDS(icd10_data,"Data/comorbidities/icd10_data.rds")

##############################################################

icd_data <- readRDS("Data/comorbidities/icd10_data.rds")

#loads cases dataset with covariates etc required for analysis
data <- readRDS("Data/follow_up1/merged_data.rds")

#icd_data contains all icd codes participants have before and during biobank enrolment. We only want to keep codes
#people had at baseline, when they were recruited. So let's subset the data...

#select eid and date of recruitment for each person
enrolment_dates <- subset(data,select=c(eid,date_recr))
enrolment_dates <- unique(enrolment_dates)

#add date of recruitment to icd data
icd_data <- merge(icd_data,enrolment_dates,by="eid")


library(tidyverse)

icd_diag <- select(icd_data, eid, date_recr, starts_with("icd10_diag"))

icd_date_diag <- select(icd_data,eid,starts_with("icd10_date_diag"))

#transpose diag dataframe
icd_diag_t <- icd_diag %>%
  pivot_longer(cols = starts_with("icd10_diag"), names_to = "icd10_diag", values_to = "icd10")

icd_diag_t$icd10_diag <- NULL

icd_date_diag_t <- icd_date_diag %>%
  pivot_longer(cols=starts_with("icd10_date_diag"),names_to= "icd10_date_diag",values_to ="icd10_date")

icd_date_diag_t$icd10_date_diag <- NULL

#merge the dataframes together

icd_data <- bind_cols(icd_diag_t,icd_date_diag_t)

icd_data$eid...4 <- NULL

icd_data <- rename(icd_data, eid = eid...1)

#keep rows with NAs in icd10 and icd10_date columns
subset_df <- icd_data[icd_data$icd10 == "" & is.na(icd_data$icd10_date), ]

#remove rows with NA's in them
icd_data2 <-  icd_data[icd_data$icd10 != "" & !is.na(icd_data$icd10_date), ]

saveRDS(icd_data2,"Data/comorbidities/icd_original.rds")

#only keep rows where diagnosis date is before biobank recruitment

icd_baseline <- icd_data2[icd_data2$icd10_date <= icd_data2$date_recr,]

saveRDS(icd_baseline,"Data/comorbidities/icd_baseline.rds")

