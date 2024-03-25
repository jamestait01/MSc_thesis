#load medication dataset
meds <- readRDS("~/Summer_project/Data/cases_meds_final.rds")

#load baseline dataset (should contain n=38364 complete cases with covariate data etc)
data <- readRDS("~/Summer_project/Data/baseline_data_NICE.rds")

#keep meds of eids in our dataset
meds <- meds[meds$eid %in% data$eid,]

#keep the latest medication for each person 
library(dplyr)

last_meds <- meds %>%
  group_by(eid) %>%
  filter(ifelse(!is.na(date_death), issue_date <= date_death, TRUE)) %>%
  filter(issue_date == max(issue_date)) %>%
  ungroup()
#calculate the date difference for each person between their last medication and date of death/end of study

# Define a function to calculate date difference
calculate_date_diff <- function(issue_date, date_death, data_provider) {
  if (data_provider == 2) {
    if (!is.na(date_death) && date_death <= as.Date("2017-05-04")) {
      return(as.numeric(date_death - issue_date))
    } else {
      return(as.numeric(as.Date("2017-05-04") - issue_date))
    }
  } else if (data_provider == 3) {
    if (!is.na(date_death) && date_death <= as.Date("2016-08-01")) {
      return(as.numeric(date_death - issue_date))
    } else {
      return(as.numeric(as.Date("2016-08-01") - issue_date))
    }
  }
  return(NA)  # If data_provider is not 2 or 3, return NA
}

# Use sapply to calculate date_diff for each row
last_meds$date_diff <- sapply(1:nrow(last_meds), function(i) {
  calculate_date_diff(last_meds$issue_date[i], last_meds$date_death[i], last_meds$data_provider[i])
})


#if someone's last prescription is >=6 months from the end date, assume they are untreated and remove rows
last_meds <- last_meds[last_meds$date_diff<=180,]

last_meds <- last_meds %>%
  rename(`Last medication combination` = med_cat)

#re-do daily_dose column 
last_meds <- last_meds %>%
  group_by(eid) %>%
  mutate(`Last daily dose` = sum(dosage))


last_meds2 <- subset(last_meds,select=c(eid,`Last medication combination`,`Last daily dose`))

last_meds_transposed <- last_meds2 %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid,`Last daily dose`),
    names_from = medication_index,
    values_from = `Last medication combination`)

last_meds_transposed <- last_meds_transposed %>%
  rename(med_cat_1 = 3, med_cat_2 = 4, med_cat_3 = 5, med_cat_4 = 6, med_cat_5 = 7, med_cat_6 = 8,
         med_cat_7 = 9)

# Create a new column with the combined med_cat values
last_meds_transposed <- last_meds_transposed %>%
  mutate(`Last medication combination` = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,
                                               med_cat_6,med_cat_7,
                                               sep = " + "))

#remove NA strings from med_combo
last_meds_transposed$`Last medication combination` <- gsub("NA","",last_meds_transposed$`Last medication combination`)

# Remove "+ + " strings from transposed_meds
last_meds_transposed$`Last medication combination` <- sub("[^A-Za-z]+$", "", last_meds_transposed$`Last medication combination`)

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
last_meds_transposed <- last_meds_transposed %>%
  mutate(`Last medication combination` = sapply(`Last medication combination`, combine_meds))

last_meds_transposed <- subset(last_meds_transposed,select=c(eid,`Last daily dose`,`Last medication combination`))

merged_df <- merge(data,last_meds_transposed,by="eid",all.x=T)

merged_df$`Last medication combination` <- ifelse(is.na(merged_df$`Last medication combination`),"Untreated",
                                                  merged_df$`Last medication combination`)

merged_df$`Last daily dose` <- ifelse(is.na(merged_df$`Last daily dose`),0,
                                      merged_df$`Last daily dose`)

#we only want to keep the medication prescriptions that fall within NICE guidelines
merged_df <- merged_df[merged_df$`Medication combination`=="Untreated" |
                         merged_df$`Medication combination`=="ACE inhibitors/ARBs" |
                         merged_df$`Medication combination`=="Calcium channel blockers" |
                         merged_df$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                         merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                         merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                         merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                         merged_df$`Medication combination`=="Calcium channel blockers + Diuretics",
]
#only keep people that are either untreated or had a final medication combination falling within NICE guidelines
merged_df <- merged_df[merged_df$`Last medication combination`=="Untreated" |
                         merged_df$`Last medication combination`=="ACE inhibitors/ARBs" |
                         merged_df$`Last medication combination`=="Calcium channel blockers" |
                         merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                         merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                         merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                         merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                         merged_df$`Last medication combination`=="Calcium channel blockers + Diuretics",
]

saveRDS(merged_df,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df.rds")

##################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df.rds")

#make dummy variables indicating who started medications, who ended them, who increased dosage etc...
#make new variable to see who's started medications
data$`Started medication` <- as.numeric((data$`Medication combination` == "Untreated" & !(data$`Last medication combination`=="Untreated")))

#make new variable to see who stopped medications
data$`Ended medication` <- as.numeric((!(data$`Medication combination` == "Untreated") & data$`Last medication combination`=="Untreated"))

#make new variable to see who changed medications
data$`Changed medications` <- as.numeric(as.character(data$`Medication combination`)!= as.character(data$`Last medication combination`) & data$`Medication combination`!="Untreated" & data$`Last medication combination`!="Untreated")


#make new variable to see who was still untreated
data$`Stayed untreated` <- as.numeric(data$`Medication combination` == "Untreated" & data$`Last medication combination`=="Untreated")

#make variable to see who stayed on the same medications at the same dosage
data$`Stable medications` <- as.numeric(as.character(data$`Medication combination`) == as.character(data$`Last medication combination`) & data$`Medication combination`!="Untreated" 
                                        & data$`daily_dose.y` == data$`Last daily dose`)

#make column to see who increased their dosage but stayed on the same medications
data$`Increased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`daily_dose.y` < data$`Last daily dose` ,1,0)

#make column to see who decreased their dosage but stayed on the same medications
data$`Decreased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`daily_dose.y` > data$`Last daily dose` ,1,0)

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

######################################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

#Make new column identifying which medication status people belong to.
library(dplyr)
data <- data %>%
  rename(med_stat = `Medication status`)

data$`Medication status` <- ifelse(data$`Started medication` == 1, "Started medication",
                                   ifelse(data$`Ended medication` == 1, "Ended medication",
                                          ifelse(data$`Changed medications` == 1, "Changed medications",
                                                 ifelse(data$`Stayed untreated` == 1, "Stayed untreated",
                                                        ifelse(data$`Increased dosage` == 1, "Increased dosage",
                                                               ifelse(data$`Stable medications` == 1, "Stable medications",
                                                                      "Decreased dosage"))))))

data$`Medication status` <- as.factor(data$`Medication status`)
data$`Medication status` <- relevel(data$`Medication status`,ref="Stable medications")

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")
