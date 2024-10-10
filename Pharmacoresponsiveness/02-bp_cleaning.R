readv2_bp <- readRDS("~/Summer_project/hypermarker/readv2_bp.rds")
readv3_bp <- readRDS("~/Summer_project/hypermarker/readv3_bp.rds")

#load date of recruitment and age info 
ages <- readRDS("~/biobank_data/extraction_and_recoding/variable_files/ages.rds")

library(vtable)
sumtable(readv2_bp,vars=c('systolic_mean','diastolic_mean'),add.median=T,title="Summary statistics for read v2 data")
sumtable(readv3_bp,vars=c('systolic_mean','diastolic_mean'),add.median=T,title="Summary statistics for read v3 data")

boxplot(readv2_bp$systolic_mean)

#remove systolic values over 370 (highest value ever recorded)
readv2_bp <- readv2_bp[readv2_bp$systolic_mean<370,]
readv3_bp <- readv3_bp[readv3_bp$systolic_mean<370,]

#remove diastolic values over 360 (highest value ever recorded)
readv2_bp <- readv2_bp[readv2_bp$diastolic_mean<360,]
readv3_bp <- readv3_bp[readv3_bp$diastolic_mean<360,]

#merge datasets together
bp_data <- rbind(readv2_bp,readv3_bp)

#remove NAs
bp_data <- bp_data[!(is.na(bp_data$systolic_mean)) | !(is.na(bp_data$diastolic_mean)),]

#recode readv2/readv3 columns
bp_data$read_code <- ifelse(bp_data$read_3=="","Read v2","Read v3")

bp_data$read_code <- as.factor(bp_data$read_code)
bp_data$read_2 <- NULL
bp_data$read_3 <- NULL
bp_data$value3 <- NULL

library(ggplot2)
##gg boxplots for both systolic and diastolic BP
ggplot(bp_data,aes(x=read_code,y=systolic_mean,color=read_code)) + 
  geom_boxplot() + labs(col="Read code",x="Read code",y="Systolic blood pressure (mmHg)") +
  ggtitle("Systolic blood pressures of gp_clinical data")

ggplot(bp_data,aes(x=read_code,y=diastolic_mean,color=read_code)) + 
  geom_boxplot() + labs(col="Read code",x="Read code",y="Diastolic blood pressure (mmHg)") +
  ggtitle("Diastolic blood pressures of gp_clinical data")

ages$eid <- rownames(ages)
#merge with date of recruitment and age data
bp_data <- merge(bp_data,ages,by="eid")

saveRDS(bp_data,"~/Summer_project/hypermarker/semicleaned_bp.rds")

#find how many people had a bp reading within 12 months of UKB recruitment
date_diff <- as.numeric(bp_data$event_dt - bp_data$date_recr)

summary(date_diff<365.25 & date_diff>0)
#263685 BP measurements within 12 months after UKB recruitment

tmp <- bp_data[(date_diff<365.25 & date_diff>0),]
n_distinct(tmp$eid)
#from 117626
