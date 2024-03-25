#loads data
setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019/")
meds <- readRDS("Data/cases_meds_final.rds")

library(tidyverse)
#filters to keep only prevalent cases
meds <- meds %>%
  filter(prevalent_case==1)

no_meds <- readRDS("Data/cases_no_meds.rds")

#keep only prevalent cases
no_meds <- no_meds %>%
  filter(prevalent_case==1)

#convert issue_date to date so we can merge the dataframes
no_meds$issue_date <- as.Date(no_meds$issue_date, format = "%d/%m/%Y")

library(dplyr)
prevalent <- bind_rows(meds,no_meds)

#removes redundant columns
prevalent$incident_case <- NULL


#saveRDS(prevalent,"Data/prevalent_cases_meds_baseline.rds")

#We only want to keep medications these people are on at baseline, when the blood pressure measurements were taken.

#remove all prescriptions after the recruitment date for each person. 

prevalent$prescription_before_recruitment <- prevalent$issue_date <= prevalent$date_recr

n_distinct(prevalent$eid)
#there are 73735 total people in the prevalent dataframe


#create df of people with no prescriptions at baseline
prevalent_base_nomeds <- prevalent %>%
  group_by(eid) %>%
  filter(all(!prescription_before_recruitment | is.na(prescription_before_recruitment)))

#create df of people with prescriptions at baseline
prevalent_base_meds <- prevalent %>%
  group_by(eid) %>%
  filter(any(prescription_before_recruitment))

n_distinct(prevalent_base_meds$eid)
#44,416 people had meds at baseline

n_distinct(prevalent_base_nomeds$eid)
#29,319 people had no meds at baseline


#saveRDS(prevalent_base_meds,"Data/prevalent_baseline_analysis/prevalent_base_meds.rds")

#copied code below from elsewhere, change df name so i dont have to change code below
meds <- prevalent_base_meds

#all these people had prescriptions before their recruitment in the biobank, however,
#we need to find out how many of those people had active prescriptions when they were enrolled,
#not just a one-off etc. Let's calculate the date difference for each prescription date and
#the date of recruitment. 

#let's see how long the average prescription is for

summary(as.factor(meds$quantity))

#most prescriptions are 28 or 56 tablets, 17549 rows are for 112 tablets, why don't I 
#use 6 months as a buffer period? 

#calculates for each medication, the difference in time between recruitment date and issue date
meds$date_diff <- as.numeric(meds$date_recr - meds$issue_date)

#Only keep prescriptions that are before the person's enrollment in the Biobank.
meds <- subset(meds,date_diff >0)

#how many people had medications before they were diagnosed with hypertension?

#calculate date difference between prescription and hypertension diagnosis
meds$pres_case_date_diff <- as.numeric(meds$issue_date - meds$date_diagnosis)

#counts number of rows with hypertension prescriptions before hypertension diagnosis 
summary((meds$pres_case_date_diff<0))

#create df of all people with a hypertensive prescription before their hypertension diagnosis
pres_before_case <- meds %>%
  group_by(eid) %>%
  filter(any(pres_case_date_diff<0))

n_distinct(pres_before_case$eid)

#21,555 people were given a prescription before they were diagnosed with hypertension

#let's analyse these prescriptions that were given before hypertension diagnosis

#subset people with prescriptions before their hypertensive diagnosis to only include the prescriptions
#before their diagnosis
only_pres_before_case <- pres_before_case %>%
  filter(pres_case_date_diff<0)

summary(only_pres_before_case$pres_case_date_diff)

summary(only_pres_before_case$med_cat)

summary(as.factor(only_pres_before_case$first_word))


#saveRDS(meds,"prevalent_base_meds.rds")

############################################################################
#setwd("Documents/MSc/Thesis/Data")
#meds <- readRDS("prevalent_base_meds.rds")

#produce summary statistics of datediff 

summary(meds$date_diff)
hist(meds$date_diff)

#keep only prescriptions within the last 6 months, 2 months and month

#we had 44,403 people initially, 
six_months <- meds %>%
  filter(date_diff <=180)

three_months <- meds %>%
  filter(date_diff <=90)

one_month <- meds %>%
  filter(date_diff<=31)

n_distinct(six_months$eid)
#34,463 have prescriptions within 6 months of enrollment

n_distinct(three_months$eid)
#33,100 have prescriptions within 3 months of enrollment

n_distinct(one_month$eid)
#21,864 people have prescriptions within a month of enrollment

#Let's work with the 6 month dataframe

#First, let's try and understand why we have people with no meds within 6 months of 
#enrollment in the Biobank when they have hypertension! 

#create df of all people with only meds prescribed more than 6 months before UKB
old_meds <- meds %>%
  group_by(eid) %>%
  filter(all(date_diff>180))

#check none of the eids in old_meds are found in meds
eid_check <- old_meds$eid

found_ids <- six_months$eid %in% eid_check

summary(found_ids)

#no eids are found, the code worked. 

n_distinct(old_meds$eid)
#9940 people had their last hypertensive prescription over 6 months before recruitment

#remove duplicated eid rows to produce summary statistics 
old_meds_unique <- old_meds[!duplicated(old_meds$eid), ]

old_meds_t1 <- CreateTableOne(vars,data=old_meds_unique,includeNA=TRUE)

old_meds_t1 <- print(old_meds_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(old_meds_t1, file = "old_meds_t1_baseline.csv")

#old_meds_unique have a lower sys BP.0 than expected (140.1), 
#however there are more females than males, is this why?

library(psych)

describeBy(old_meds_unique$sys_bp.0, group=old_meds_unique$sex.0.0)

#Yes it is, let's redo the table 1 for each gender

old_meds_sex_t1 <- CreateTableOne(vars,strata="sex.0.0",data=old_meds_unique,includeNA=TRUE)

old_meds_sex_t1 <- print(old_meds_sex_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(old_meds_sex_t1, file = "old_meds_sex_t1_baseline.csv")


saveRDS(old_meds,"old_meds.rds")
saveRDS(six_months,"six_monthmeds.rds")




###########################################################################
setwd("Documents/MSc/Thesis/Data")

meds <- readRDS("six_monthmeds.rds")

#remove duplicated eid rows to produce summary statistics 
meds_unique <- meds[!duplicated(meds$eid), ]

meds_sex_t1 <- CreateTableOne(vars,strata="sex.0.0",data=meds_unique,includeNA=TRUE)

meds_sex_t1 <- print(meds_sex_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(meds_sex_t1, file = "meds_sex_t1_baseline.csv")

#make other datasets for prescriptions 3 months or 1 month before Biobank recruitment
three_months <- meds %>%
  filter(date_diff <=90)

one_month <- meds %>%
  filter(date_diff<=31)

three_months_unique <- three_months[!duplicated(three_months$eid), ]
one_month_unique <- one_month[!duplicated(one_month$eid), ]

three_meds_sex_t1 <- CreateTableOne(vars,strata="sex.0.0",data=three_months_unique,includeNA=TRUE)

three_meds_sex_t1 <- print(three_meds_sex_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(three_meds_sex_t1, file = "3month_meds_sex_t1_baseline.csv")

one_meds_sex_t1 <- CreateTableOne(vars,strata="sex.0.0",data=one_month_unique,includeNA=TRUE)

one_meds_sex_t1 <- print(one_meds_sex_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(one_meds_sex_t1, file = "1month_meds_sex_t1_baseline.csv")

#########################MORE MEDICATION TIDYING#################################

#for each eid, take whichever rows contain the  most recent prescriptions based on date_diff
setwd("Documents/MSc/Thesis/Data")

meds <- readRDS("six_monthmeds.rds")

library(dplyr)

#for each eid, take the 10 most recent prescriptions
recentmeds <- meds %>%
  group_by(eid) %>%
  slice_min(date_diff,n=10)

View(recentmeds)

#remove duplicate medications

duplicate_rows <- duplicated(recentmeds[, !colnames(recentmeds) %in% c('issue_date', 'date_diff','quantity')])

# Remove duplicate rows from the dataframe
recentmeds2 <- recentmeds[!duplicate_rows, ]

n_distinct(recentmeds2$eid)

#check frequency of different date_diffs
hist(recentmeds2$date_diff)


#subset df to get a feel for the data
subset <- subset(recentmeds2,select=c(eid,drug_name,quantity,dosage,date_diff))

subset2 <- subset %>%
  group_by(eid) %>%
  slice_min(date_diff,n=1)


#okay, so this code works, let's implement it for real.

recentmedsfinal <- recentmeds2 %>%
  group_by(eid) %>%
  slice_min(date_diff,n=1)

#add daily_dose column 
recentmedsfinal <- recentmedsfinal %>%
  group_by(eid) %>%
  mutate(daily_dose = sum(dosage))

saveRDS(recentmedsfinal,"recentmedsfinal.rds")

#########################################################################
meds <- readRDS("recentmedsfinal.rds")


library(psych)

describeBy(meds$sys_bp.0,group=meds$med_cat,data=meds)

#make df to investigate relationship between daily dosages and baseline bp
daily_doses <- subset(meds,select=c(eid,daily_dose,sys_bp.0,dias_bp.0))

#remove duplicate eids that are present as they have multiple meds
daily_doses <- daily_doses[!duplicated(daily_doses$eid),]

plot(daily_doses$daily_dose,daily_doses$sys_bp.0)
#need to add the information back from the other dataframes (total daily dosage, first_word etc)

library(ggplot2)

ggplot(daily_doses, aes(x = daily_dose, y = sys_bp.0)) +
  geom_jitter() +
  xlab("Daily Dose") +
  ylab("Systolic Blood Pressure") +
  ggtitle("Relationship between Daily Dose and Systolic Blood Pressure")

###############################################################################################
meds <- readRDS("baseline_analysis/recentmedsfinal.rds")

library(dplyr)
#re-do daily_dose column 
meds <- meds %>%
  group_by(eid) %>%
  mutate(daily_dose = sum(dosage))

#remove dmd_code and read_2 codes as not needed

meds$dmd_code <- NULL
meds$read_2 <- NULL


#only keep people with non-missing values in sys_bp.0 and dias_bp.0
meds <- meds[!is.na(meds$sys_bp.0) & !is.na(meds$dias_bp.0),]

############################################################################

#check how many ids are on 1 medication, 2 meds, 3 meds etc.

eid_counts <- table(meds$eid)

table(eid_counts)

#check no. of people on n different meds
repeated <- meds[meds$eid %in% names(eid_counts[eid_counts == 8]), ]

library(tidyverse)


# Transpose the dataframe
transposed_meds <- meds %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case, sex.0.0, smoking_status.0.0,
                pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider, issue_date,
                idx_multdep, prescription_before_recruitment, date_diff, daily_dose),
    names_from = medication_index,
    values_from = c(bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

View(transposed_meds)

# Define the drug name columns
drug_name_columns <- grep("^drug_name_", names(transposed_meds), value = TRUE)

# Create a new column 'n_meds_baseline'
transposed_meds$n_meds_baseline <- rowSums(!is.na(transposed_meds[, drug_name_columns]))

saveRDS(transposed_meds,"baseline_analysis/transposed_meds.rds")

#make summary function
generate_summary_stats <- function(data, grouping_vars, target_var) {
  library(dplyr)
  library(tidyr)
  library(broom)
  
  # Group the data by grouping variables
  grouped_data <- data %>% group_by(across(all_of(grouping_vars)))
  
  # Calculate summary statistics
  summary_stats <- grouped_data %>%
    summarise(
      n = n(),
      mean = mean({{ target_var }}),
      se = sd({{ target_var }}) / sqrt(n()),
      conf_lower = mean - 1.96 * se,
      conf_upper = mean + 1.96 * se)
  
  return(summary_stats)
}

sys_nmeds <- generate_summary_stats(transposed_meds,c("sex.0.0","n_meds_baseline"),sys_bp.0)
dias_nmeds <- generate_summary_stats(transposed_meds,c("sex.0.0","n_meds_baseline"),dias_bp.0)

summary(transposed_meds$daily_dose)

# Define the breaks for the daily dose category
breaks <- c(0, 10, 20, 30, 40, 50, 100, Inf)

# Create the daily_dose_category column
transposed_meds$daily_dose_cat <- cut(transposed_meds$daily_dose, breaks = breaks, labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-100", "100+"))

summary(transposed_meds$daily_dose_cat)

sys_dose <- generate_summary_stats(transposed_meds,c("sex.0.0","daily_dose_cat"),sys_bp.0)
dias_dose <- generate_summary_stats(transposed_meds,c("sex.0.0","daily_dose_cat"),dias_bp.0)

saveRDS(transposed_meds,"baseline_analysis/transposed_meds.rds")

#investigate how many different combinations of med_cats people are on

transposed_meds <- readRDS("Data/baseline_analysis/transposed_meds.rds")
# Create a new column with the combined med_cat values
transposed_meds <- transposed_meds %>%
  mutate(med_combo = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,med_cat_6,med_cat_7,med_cat_8,
                           sep = " + "),na.rm=TRUE)

# Count the occurrences of each combination
combo_counts <- transposed_meds %>%
  group_by(med_combo) %>%
  count(med_combo)

View(combo_counts)

# Remove "NA" strings from combo_counts
combo_counts$med_combo <- gsub("NA", "", combo_counts$med_combo)
View(combo_counts)

#remove NA strings from med_combo
transposed_meds$med_combo <- gsub("NA","",transposed_meds$med_combo)

# Remove "+ + " strings from combo_counts
combo_counts$med_combo <- sub("[^A-Za-z]+$", "", combo_counts$med_combo)
View(combo_counts)

# Remove "+ + " strings from transposed_meds
transposed_meds$med_combo <- sub("[^A-Za-z]+$", "", transposed_meds$med_combo)

saveRDS(transposed_meds,"Data/baseline_analysis/transposed_meds.rds")

sys_medcat <- generate_summary_stats(transposed_meds,c("sex.0.0","med_combo"),sys_bp.0)
dias_medcat <- generate_summary_stats(transposed_meds,c("sex.0.0","med_combo"),dias_bp.0)

#someone may be on ARBs and CCBs, but depending on which one is prescribed first, will either appear
#as ARBs/CCBs or CCBs/ARBs. Can we combine these 2 options together?

# Define a helper function to combine and sort medication types within a string
combine_meds <- function(string) {
  meds <- strsplit(string, "_")[[1]]  # Split the string into separate medication types
  combined_meds <- sort(meds)  # Sort the medication types alphabetically
  combined_string <- paste(combined_meds, collapse = "_")  # Combine the medication types into a single string
  return(combined_string)
}

# Apply the helper function to the medication_column and create a new combined_medication_column
transposed_meds <- transposed_meds %>%
  mutate(med_combo = sapply(med_combo, combine_meds))

# Count the occurrences of each combination
med_combo_counts <- transposed_meds %>%
  group_by(med_combo) %>%
  count(med_combo)

View(med_combo_counts)

transposed_meds <- transposed_meds %>%
  rename(`Medication combination`= med_combo)

saveRDS(transposed_meds,"Data/baseline_analysis/transposed_meds.rds")

#only keep those on treatment that fall within NICE guidelines 
NICE_data <- transposed_meds[transposed_meds$`Medication combination`=="Untreated" |
                                         transposed_meds$`Medication combination`=="ACE inhibitors/ARBs" |
                                         transposed_meds$`Medication combination`=="Calcium channel blockers" |
                                         transposed_meds$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                         transposed_meds$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                         transposed_meds$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                         transposed_meds$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                         transposed_meds$`Medication combination`=="Calcium channel blockers + Diuretics",]

#drop redundant factor levels
NICE_data$`Medication combination` <- droplevels(NICE_data$`Medication combination`)

saveRDS(NICE_data,"Data/NICE_medications.rds")

#############################################################################################

#how many people in six_monthmeds were given a prescription before their diagnosis?
sixmonthmeds <- readRDS("six_monthmeds.rds")
eids <- unique(sixmonthmeds$eid)

meds <- readRDS("cases_meds_final.rds")

#remove columns so we can merge the 2 datasets
sixmonthmeds$date_diff <- NULL
sixmonthmeds$prescription_before_recruitment <- NULL

#merge meds and old_meds keeping common eids
merged_df <- merge(meds,sixmonthmeds,by="eid")

#remove duplicate columns
merged_df2 <- merged_df[, !grepl("\\.y$", names(merged_df))]

#count number of eids in merged_df2, same as in old_meds
n_distinct(merged_df2$eid)

#calculate date difference between prescription and hypertension diagnosis
merged_df2$pres_case_date_diff <- as.numeric(merged_df2$issue_date.x - merged_df2$date_diagnosis.x)

#counts number of rows with hypertension prescriptions before hypertension diagnosis 
summary((merged_df2$pres_case_date_diff<0))

#create df of all people with a hypertensive prescription before their hypertension diagnosis
pres_before_case <- merged_df2 %>%
  group_by(eid) %>%
  filter(any(pres_case_date_diff<0))

n_distinct(pres_before_case$eid)

#2516 people were given a prescription before they were diagnosed with hypertension

#let's see what these medications were

#subset people with prescriptions before their hypertensive diagnosis to only include the prescriptions
#before their diagnosis
only_pres_before_case <- pres_before_case %>%
  filter(pres_case_date_diff<0)

summary(only_pres_before_case$pres_case_date_diff)

summary(only_pres_before_case$med_cat.x)

summary(as.factor(only_pres_before_case$first_word.x))


