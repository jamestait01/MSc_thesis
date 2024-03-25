setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019")
meds <- readRDS("Data/cases_unique_meds.rds")

#some of the extracted medications have been incorrectly classified as hypertensive meds, let's get a list of all unique
#medications in the drug name column

#Let's first remove all of the nitrates and other antianginal/vascular disease drugs

#Nitrates are coded as 020601 whereas CCBs are 020602. Other antianginal drugs are 020603
#020604 drugs are used for vascular diseases (not hypertension seemingly)

# Specify the codes to be removed
codes_to_remove <- c("020601","02.06.01", "020603","02.06.03", "020604","02.06.04")

# Remove rows with the specified codes
meds_filtered <- meds[!grepl(paste(codes_to_remove, collapse = "|"), meds$bnf_code), ]

saveRDS(meds_filtered,"Data/cases_unique_meds.rds")



# Extract unique medication names
medications <- unique(gsub("\\d.*", "", meds_filtered$drug_name))

#save list of unique medications names as csv
write.csv(medications,"preprocessing/prescriptions/medications_list.csv")


#I manually look at the csv file to identify which drugs are hypertensive/which ones are not, then reupload the 'cleaned' version.




