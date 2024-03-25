setwd("Documents/MSc/Thesis/Data")


old_meds <- readRDS("old_meds.rds")
meds <- readRDS("cases_meds_final.rds")

library(dplyr)
n_distinct(old_meds$eid)

#remove columns so we can merge the 2 datasets
old_meds$date_diff <- NULL
old_meds$prescription_before_recruitment <- NULL

#merge meds and old_meds keeping common eids
merged_df <- merge(meds,old_meds,by="eid")

#remove duplicate columns
merged_df2 <- merged_df[, !grepl("\\.y$", names(merged_df))]

#count number of eids in merged_df2, same as in old_meds
n_distinct(merged_df2$eid)

#calculate date difference between prescription and biobank enrollment
merged_df2$pres_enrol_date_diff <- as.numeric(merged_df2$issue_date.x - merged_df2$date_recr.x)

#calculate date difference between prescription and hypertension diagnosis
merged_df2$pres_case_date_diff <- as.numeric(merged_df2$issue_date.x - merged_df2$date_diagnosis.x)

#check if prescription was after biobank recruitment
merged_df2$pres_after_recruitment <- merged_df2$pres_enrol_date_diff > 0

summary(merged_df2$pres_after_recruitment)

#it might be that these people were incident cases, but mistakenly diagnosed as prevalent cases


merged_df2$time_to_diagnosis <- as.numeric(merged_df2$date_diagnosis.x - merged_df2$date_recr.x)


#as we're looking at prevalent cases, we should only see negative values

#remove duplicated eid rows to produce summary statistics 
merged_df2_unique <- merged_df2[!duplicated(merged_df2$eid), ]

summary(merged_df2_unique$time_to_diagnosis)

temp <- merged_df2_unique %>%
  filter(time_to_diagnosis >0)

#566 people were diagnosed after enrolment?
#checks for each eid, if they pres_after_recruitment rows = TRUE

count <- aggregate(pres_after_recruitment ~ eid, data = merged_df2, FUN = function(x) sum(x==TRUE))

#counts the no. of eids with one prescription after recruitment and those with none.
summary(as.factor(count$pres_after_recruitment>0))

#4356 people had prescriptions after Biobank recruitment, but not in the 6 months before.
#5584 people had no prescriptions after Biobank recruitment, or within the 6 months before recruitment

#let's create separate dataframes to analyse them

# Filter for IDs with at least one prescription after biobank recruitment
ids_with_1s <- count[count$pres_after_recruitment > 0, "eid"]
ids_with_0s <- count[count$pres_after_recruitment == 0, "eid"]

# Create separate datasets for 1s and 0s
df_1s <- merged_df2[merged_df2$eid %in% ids_with_1s, ]
df_0s <- merged_df2[merged_df2$eid %in% ids_with_0s, ]


saveRDS(df_0s,"df_0s.rds")
saveRDS(df_1s,"df_1s.rds")

#investigate df_0s a bit more

df_0s_subset <- subset(df_0s,select=c(eid,date_recr.x,date_diagnosis.x,date_death.x,issue_date.x,drug_name.x,
                                      quantity.x,dosage.x,med_cat.x,pres_enrol_date_diff,pres_case_date_diff))


hist(df_0s$pres_case_date_diff,breaks=100,xaxp=c(-9000,9000,100),cex.axis=0.5)

#counts number of rows with hypertension prescriptions before hypertension diagnosis 
summary((df_0s$pres_case_date_diff<0))



#what about their time_to_diagnosis data, we've discovered that some people are marked as prevalent
#cases even though they were diagnosed after recruitment, might this explain something?

df_0s$time_to_diagnosis <- as.numeric(df_0s$date_diagnosis.x - df_0s$date_recr.x)


#as we're looking at prevalent cases, we should only see negative values

#remove duplicated eid rows to produce summary statistics 
df_0s_unique <- df_0s[!duplicated(df_0s$eid), ]

summary(df_0s_unique$time_to_diagnosis)


temp <- df_0s_unique %>%
  filter(time_to_diagnosis >0)

#108 people had were diagnosed with hypertension after enrolment


#create df of all people with a hypertensive prescription before their hypertension diagnosis
pres_before_case <- df_0s %>%
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

#It's likely that they were given these medications to treat other conditions.

#The date_diagnosis column is strange for some of these people...

eid3380138 <- only_pres_before_case %>%
  filter(eid=="3380138")

#for this person, they are a prevalent case even though their dod is 2021, let's work out the ttd 
#for people with only_pres_before_case

only_pres_before_case$time_to_diagnosis <- as.numeric(only_pres_before_case$date_diagnosis.x - only_pres_before_case$date_recr.x)

#as we're looking at prevalent cases, we should only see negative values

#remove duplicated eid rows to produce summary statistics 
only_pres_before_case_unique <- only_pres_before_case[!duplicated(only_pres_before_case$eid), ]

temp <- only_pres_before_case_unique %>%
  filter(time_to_diagnosis >0)

#108 people were diagnosed after enrollment even though they were classified as prevalent cases...

##################### looking at df_1s, people with prescriptions after enrollment but not before ##################################################

#again, maybe people were diagnosed with hypertension after enrollment - we've seen issues with ttd dates

df_1s$time_to_diagnosis <- as.numeric(df_1s$date_diagnosis.x - df_1s$date_recr.x)


#as we're looking at prevalent cases, we should only see negative values

#remove duplicated eid rows to produce summary statistics 
df_1s_unique <- df_1s[!duplicated(df_1s$eid), ]

summary(df_1s_unique$time_to_diagnosis)

temp <- df_1s_unique %>%
  filter(time_to_diagnosis >0)

#369 people in df_1s were diagnosed with hypertension after enrollment?

#counts number of rows with hypertension prescriptions before hypertension diagnosis 
summary((df_1s$pres_case_date_diff<0))

#create df of all people with a hypertensive prescription before their hypertension diagnosis
df1s_pres_before_case <- df_1s %>%
  group_by(eid) %>%
  filter(any(pres_case_date_diff<0))

n_distinct(df1s_pres_before_case$eid)

#1904 people had prescription before hypertension diagnosis

#subset people with prescriptions before their hypertensive diagnosis to only include the prescriptions
#before their diagnosis
df1s_only_pres_before_case <- df1s_pres_before_case %>%
  filter(pres_case_date_diff<0)


summary(df1s_only_pres_before_case$med_cat.x)

summary(as.factor(df1s_only_pres_before_case$first_word.x))
