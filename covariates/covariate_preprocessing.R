#Set wd
setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019/")

#loads data containing case information
output_final <- readRDS("outcome_definition/Outputs/output_final.rds")

#loads extracted variable data
variables <- readRDS("extraction_and_recoding/outputs/ukb_extracted.rds")

#eid is the rowname for variables, so make new column for eid in variables data
variables$eid <- rownames(variables)

variables$idx_multdep <- ifelse(!is.na(variables$idx_multdep_e.0.0), variables$idx_multdep_e.0.0,
                                ifelse(!is.na(variables$idx_multdep_w.0.0), variables$idx_multdep_w.0.0,
                                       variables$idx_multdep_s.0.0))

variables$idx_multdep_e.0.0 <- NULL
variables$idx_multdep_s.0.0 <- NULL
variables$idx_multdep_w.0.0 <- NULL

#recode alcohol intake
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==1,"Daily/almost daily",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==2,"3-4 times a week",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==3,"1-2 times a week",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==4,"1-3 times a month",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==5, "Special occasions",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==6,"Never",variables$alc_intake.0.0)
variables$alc_intake.0.0 <- ifelse(variables$alc_intake.0.0==-3,"Prefer not to answer",variables$alc_intake.0.0)

variables$alc_intake.0.0 <- as.factor(variables$alc_intake.0.0)

original_data <- merge(output_final,variables,by="eid")

library(tidyverse)

#code below calculates the average for pairs of blood pressure measurements

# Define the column pairs
column_pairs <- c("sys_bp_manual.0", "sys_bp_manual.1", "sys_bp_manual.2", "sys_bp_manual.3",
                  "dias_bp_manual.0", "dias_bp_manual.1", "dias_bp_manual.2", "dias_bp_manual.3",
                  "sys_bp_auto.0", "sys_bp_auto.1", "sys_bp_auto.2", "sys_bp_auto.3",
                  "dias_bp_auto.0", "dias_bp_auto.1", "dias_bp_auto.2", "dias_bp_auto.3")

# Iterate over the column pairs and calculate the averages in a new avg column
for (pair in column_pairs) {
  col1 <- paste0(pair, ".0")
  col2 <- paste0(pair, ".1")
  new_col <- paste0(pair, ".avg")
  original_data[[new_col]] <- (original_data[[col1]] + original_data[[col2]]) / 2
}

data_cleaned <- original_data

#now that we've calculated the average we don't need the original bp columns so let's remove them
cols_to_remove <-  c("sys_bp_manual.0.0", "sys_bp_manual.0.1", "sys_bp_manual.1.0", "sys_bp_manual.1.1",
                     "sys_bp_manual.2.0", "sys_bp_manual.2.1", "sys_bp_manual.3.0", "sys_bp_manual.3.1",
                     "dias_bp_manual.0.0", "dias_bp_manual.0.1", "dias_bp_manual.1.0", "dias_bp_manual.1.1",
                     "dias_bp_manual.2.0", "dias_bp_manual.2.1", "dias_bp_manual.3.0", "dias_bp_manual.3.1",
                     "dias_bp_auto.0.0", "dias_bp_auto.0.1", "dias_bp_auto.1.0", "dias_bp_auto.1.1",
                     "dias_bp_auto.2.0", "dias_bp_auto.2.1", "dias_bp_auto.3.0", "dias_bp_auto.3.1",
                     "sys_bp_auto.0.0", "sys_bp_auto.0.1", "sys_bp_auto.1.0", "sys_bp_auto.1.1",
                     "sys_bp_auto.2.0", "sys_bp_auto.2.1", "sys_bp_auto.3.0","sys_bp_auto.3.1")

data_cleaned <- data_cleaned[, -which(names(data_cleaned) %in% cols_to_remove)]

#remove pregnant people as pregnancy can increase blood pressure

# Subset original data_cleaned to only keep non-pregnant people 
data_cleaned2 <- subset(data_cleaned, is.na(pregnant.0.0) | pregnant.0.0 == 0)
data_cleaned2 <- subset(data_cleaned2, is.na(pregnant.1.0) | pregnant.1.0 == 0)
data_cleaned2 <- subset(data_cleaned2, is.na(pregnant.2.0) | pregnant.2.0 == 0)
data_cleaned2 <- subset(data_cleaned2, is.na(pregnant.3.0) | pregnant.3.0 == 0)

#remove redundant pregnancy columns
preg_cols <-  c("pregnant.0.0", "pregnant.1.0", "pregnant.2.0", "pregnant.3.0")

data_cleaned2 <- data_cleaned2[, -which(names(data_cleaned2) %in% preg_cols)]

#compares each instance of smoking stauts to see if the values are the same for each person and instance
table(apply(data_cleaned2[, c("smoking_status.0.0", "smoking_status.1.0", "smoking_status.2.0", 
                              "smoking_status.3.0")],1, function(x) all(x == x[1], na.rm = TRUE)))

#The same smoking status is recorded for 496,077 people, and not for 5943 people
#Just keep the smoking status from baseline, -3 = prefer not to answer, 0 = never, 1 = previous, 2 = current

data_cleaned2$smoking_status.1.0 <- NULL
data_cleaned2$smoking_status.2.0 <- NULL
data_cleaned2$smoking_status.3.0 <- NULL

#recode smoking column
data_cleaned2 <- data_cleaned2 %>%
  mutate(smoking_status.0.0 = case_when(
    smoking_status.0.0 == -3 ~ 'Prefer not to answer',
    smoking_status.0.0 == 0 ~ 'Never',
    smoking_status.0.0 == 1 ~ 'Previous',
    smoking_status.0.0 == 2 ~ 'Current'))

data_cleaned2$smoking_status.0.0 <- as.factor(data_cleaned2$smoking_status.0.0)


#repeat the above for bmi
table(apply(data_cleaned2[, c("bmi.0.0","bmi.1.0","bmi.2.0","bmi.3.0")],1,
            function(x) all(x == x[1], na.rm = TRUE)))

#437758 have the same BMI (87%), 64262 people do not (8.7%), still drop other instances as mostly missing

data_cleaned2$bmi.1.0 <- NULL
data_cleaned2$bmi.2.0 <- NULL
data_cleaned2$bmi.3.0 <- NULL

#drop duplicate ethnicities
data_cleaned2$ethnicity.1.0 <- NULL
data_cleaned2$ethnicity.2.0 <- NULL

#recode sex
data_cleaned2$sex.0.0 <- ifelse(data_cleaned2$sex.0.0==1,"Male","Female")
data_cleaned2$sex.0.0 <- as.factor(data_cleaned2$sex.0.0)

#recode ethnicities
data_cleaned2 <- data_cleaned2 %>%
  mutate(ethnicity.0.0 = case_when(
    ethnicity.0.0 == -3 ~ 'Prefer not to answer',ethnicity.0.0 == -1 ~ 'Do not know', ethnicity.0.0 == 1 ~ 'White',
    ethnicity.0.0 == 2 ~ 'Mixed', ethnicity.0.0 == 3 ~ 'Asian', ethnicity.0.0 ==4 ~ "Black",
    ethnicity.0.0 ==5 ~ "Chinese", ethnicity.0.0 ==6 ~ "Other", ethnicity.0.0 ==1001 ~ "British",
    ethnicity.0.0 ==1002 ~"Irish", ethnicity.0.0 ==1003 ~ "Other white", ethnicity.0.0 ==2001 ~ "White and Black Caribbean",
    ethnicity.0.0 == 2002 ~ "White and Black African", ethnicity.0.0 == 2003 ~ "White and Asian",
    ethnicity.0.0 == 2004 ~ "Other mixed", ethnicity.0.0 == 3001 ~ "Indian", ethnicity.0.0 == 3002 ~ "Pakistani",
    ethnicity.0.0 == 3003 ~ "Bangladeshi", ethnicity.0.0 == 3004 ~ "Other asian", ethnicity.0.0 == 4001 ~ "Caribbean",
    ethnicity.0.0 == 4002 ~ "African", ethnicity.0.0 == 4003 ~ "Other black"))

data_cleaned2$ethnicity.0.0 <- as.factor(data_cleaned2$ethnicity.0.0)

eth_prevalence <- prop.table(table(data_cleaned2$ethnicity.0.0))

other_levels <- names(eth_prevalence[eth_prevalence < 0.02])

data_cleaned2$ethnicity.0.0[data_cleaned2$ethnicity.0.0 %in% other_levels] <- "Other"

#remove factor levels with 0 values
data_cleaned2$ethnicity.0.0 <- droplevels(data_cleaned2$ethnicity.0.0)

#confirm that everyone with manual blood measurements do not have automatic measurements
bp_subset <- data_cleaned2 %>%
  select(sys_bp_manual.0.avg,sys_bp_auto.0.avg, dias_bp_manual.0.avg,dias_bp_auto.0.avg,sys_bp_manual.2.avg,sys_bp_auto.2.avg)

View(bp_subset)
#confirmed
rm(bp_subset)

#take either the average manual or automatic blood pressure measurement for each instance, depending on which is recorded
data_cleaned2$sys_bp.0 <- ifelse(is.na(data_cleaned2$sys_bp_auto.0.avg),
                                 data_cleaned2$sys_bp_manual.0.avg, data_cleaned2$sys_bp_auto.0.avg)

data_cleaned2$sys_bp.1 <- ifelse(is.na(data_cleaned2$sys_bp_auto.1.avg),
                                 data_cleaned2$sys_bp_manual.1.avg, data_cleaned2$sys_bp_auto.1.avg)

data_cleaned2$sys_bp.2 <- ifelse(is.na(data_cleaned2$sys_bp_auto.2.avg),
                                 data_cleaned2$sys_bp_manual.2.avg, data_cleaned2$sys_bp_auto.2.avg)

data_cleaned2$sys_bp.3 <- ifelse(is.na(data_cleaned2$sys_bp_auto.3.avg),
                                 data_cleaned2$sys_bp_manual.3.avg, data_cleaned2$sys_bp_auto.3.avg)

data_cleaned2$dias_bp.0 <- ifelse(is.na(data_cleaned2$dias_bp_auto.0.avg),
                                 data_cleaned2$dias_bp_manual.0.avg, data_cleaned2$dias_bp_auto.0.avg)

data_cleaned2$dias_bp.1 <- ifelse(is.na(data_cleaned2$dias_bp_auto.1.avg),
                                  data_cleaned2$dias_bp_manual.1.avg, data_cleaned2$dias_bp_auto.1.avg)

data_cleaned2$dias_bp.2 <- ifelse(is.na(data_cleaned2$dias_bp_auto.2.avg),
                                  data_cleaned2$dias_bp_manual.2.avg, data_cleaned2$dias_bp_auto.2.avg)

data_cleaned2$dias_bp.3 <- ifelse(is.na(data_cleaned2$dias_bp_auto.3.avg),
                                  data_cleaned2$dias_bp_manual.3.avg, data_cleaned2$dias_bp_auto.3.avg)

#remove the original bp measurements
bp_cols_to_remove <- colnames(data_cleaned2[,15:30])

data_cleaned2 <- data_cleaned2[, -which(names(data_cleaned2) %in% bp_cols_to_remove)]

#rename variables
data_cleaned2 <- data_cleaned2 %>%
  rename(
    Sex = sex.0.0,
    `Smoking status` = smoking_status.0.0,
    Ethnicity = ethnicity.0.0,
    BMI = bmi.0.0,
    Age = age_recr.0.0,
    `Number of comorbidities` = n_comorbid_cat_no_hypert,
    `Weekly alcohol intake` = alc_intake.0.0,
    `Index of Multiple Deprivation` = idx_multdep)

data_cleaned2$`Age group` <- cut(data_cleaned2$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                                 ,right = FALSE)

saveRDS(data_cleaned2,"preprocessing/data_cleaned.rds")
