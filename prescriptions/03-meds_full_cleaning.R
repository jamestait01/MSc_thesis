meds <- readRDS("Data/old_data/cases_meds.rds")

########################################################

#Let's first remove all of the nitrates and other antianginal/vascular disease drugs

#Nitrates are coded as 020601 whereas CCBs are 020602. Other antianginal drugs are 020603
#020604 drugs are used for vascular diseases (not hypertension seemingly)

# Specify the codes to be removed
codes_to_remove <- c("020601","02.06.01", "020603","02.06.03", "020604","02.06.04")

#original dataframe is 8,498,405 rows

# Remove rows with the specified codes
meds <- meds[!grepl(paste(codes_to_remove, collapse = "|"), meds$bnf_code), ]


med_list <- read.csv("preprocessing/prescriptions/medications_list_cleaned.csv",header=T)

#creates new column to store matched medicines
meds$matched_medicine <- NA

# Iterate over rows in meds
for (i in seq_len(nrow(meds))) {
  drug_name <- meds$drug_name[i]
  
  # Iterate over rows in med_list
  for (j in seq_len(nrow(med_list))) {
    medicine <- med_list$Medicine[j]
    
    # Check if the medicine name in med_list is found in the drug name in meds
    if (grepl(medicine, drug_name)) {
      meds$matched_medicine[i] <- medicine
      break  # Exit the inner loop once a match is found
    }
  }
}


meds$matched_medicine <- as.factor(meds$matched_medicine)

#makes vector of drugs identified as non-hypertensive treatments

drugs_to_investigate <- c("Nimodipine ","Atenix ","Burinex ","Torem ","DIUMIDE K-P ","Univer ","flunarizine (roi) capsules ",
                          "Acetazolamide ","ADALAT ","Aldactone ","Co-amilofruse ","Aldactide ","Bumetanide ",
                          "Cardicor ","Entresto ","Sacubitril And Valsartan  Tablets ","Diamox ","Eytazox ",
                          "Timolol ", "Alfuzosin ", "Combodart ", "Diffundox XL ", "Doralese Tiltab ", "Flomax MR ",
                          "Indoramin ", "Kelanu XL ", "Omnic MR ", "Pamsvax XL ", "Prosurin XL ", "Stronazon ",
                          "Tabphyn MR ", "Tamsulosin ", "Vesomni ", "Cialis ", "Sildenafil ", "Beta-Cardone Tablets ",
                          "Betaloc ", "Sotalol ", "Tildiem ", "Optil ", "Slofedipine XL ", "Zufal XL ","Hydrosaluric ")

#remove rows with non-hypertensive drugs 
meds <- meds[!(meds$matched_medicine %in% drugs_to_investigate), ]
#from ~8,050,000 rows to 7,663,329

#drop the now removed medications from matched_medicine factor
meds$matched_medicine <- droplevels(meds$matched_medicine)

#convert issue_date to date
meds$issue_date <- as.Date(meds$issue_date,format = "%d/%m/%Y")

#convert rows with prescription dates in 1902 or 2037 to NA
meds$issue_date[format(meds$issue_date, "%Y") == "1902"] <- NA
meds$issue_date[format(meds$issue_date, "%Y") == "2037"] <- NA
sum(is.na(meds$issue_date))

##############################################################

#make new columns for first word in drug name column
meds$drug_name <- as.character(meds$drug_name)
# Splitting drug names into separate words
words <- strsplit(meds$drug_name, " ")

# Creating new column for first word (drug name)
meds$first_word <- sapply(words, function(x) x[1])

#removed matched_medicine column as redundant
meds$matched_medicine <- NULL

# Extracting dosage and unit using regular expressions
dosage <- gsub(".*?([0-9.]+)\\s*([a-zA-Z]+).*", "\\1", meds$drug_name)
unit <- gsub(".*?([0-9.]+)\\s*([a-zA-Z]+).*", "\\2", meds$drug_name)

# Adding dosage and unit as new columns
meds$dosage <- dosage
meds$unit <- unit

# Count the number of rows without an integer in the dosage column
contains_integer <- function(text) {
  result <- grepl("\\d", text)
  return(!result)
}
sum(sapply(meds$dosage, contains_integer))
#796 rows without an integer in the dosage column

#remove these rows
# Subsetting to remove rows with non-integer values in the dosage column
meds <- meds[!sapply(meds$dosage, contains_integer), ]

#counts no. of rows with alphabetical letters in dosage column
sum(grepl("[a-zA-Z]", meds$dosage))

#remove these rows
meds <- meds[!grepl("[a-zA-Z]", meds$dosage), ]

#All capsules are Angitil/Dilzem -> convert to mg
meds$unit <- ifelse(meds$unit=="capsules","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="Capsules","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="CAPSULES","mg",meds$unit)


#MG = mg -> convert to mg
meds$unit <- ifelse(meds$unit=="MG","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="MGM","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="mgs","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="MGS","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="MGTAB","mg",meds$unit)

#convert all micrograms to same category
meds$unit <- ifelse(meds$unit=="micrograms","microgram",meds$unit)
meds$unit <- ifelse(meds$unit=="MICROGRAMS","microgram",meds$unit)

#most tablets are from adalat or Adipine -> convert to mg

meds$unit <- ifelse(meds$unit=="Tablet","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="tablet","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="TABLET","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="tablets","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="Tablets","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="TABLETS","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="tabs","mg",meds$unit)
meds$unit <- ifelse(meds$unit=="TABS","mg",meds$unit)

#convert 12 hour dosages into 24 hour dosages
meds$dosage <- ifelse(meds$drug_name=="diltiazem 12 hour modified release capsules 120mg",
                           "240", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="diltiazem 12 hour modified release capsules 180mg",
                           "360", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="diltiazem 24 hour modified release capsules 120mg",
                           "120", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="diltiazem 24 hour modified release capsules 180mg",
                           "180", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="diltiazem 24 hour modified release capsules 300mg",
                           "300", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="nifedipine 12 hour modified release capsules 20mg",
                           "40", meds$dosage)

meds$dosage <- ifelse(meds$drug_name=="nifedipine 24 hour modified release capsules 20mg",
                           "20", meds$dosage)

#convert hours units to mg
meds$unit <- ifelse(meds$unit=="hour","mg",meds$unit)

meds$dosage <- as.numeric(meds$dosage)

summary(as.factor(meds$unit))

#everyone in XL is Zemtard, all their prescriptions are in mg, convert XL to mg
meds$unit <- ifelse(meds$unit=="XL",
                         "mg", meds$unit)

summary(as.factor(meds$unit))

#7,597,477 rows have mg, 55905 rows have micrograms, remove the other units

meds <- subset(meds, unit %in% c("mg", "microgram"))

#convert microgram to mg
meds$dosage <- ifelse(meds$unit=="microgram",meds$dosage/1000,meds$dosage)

meds$unit <- ifelse(meds$unit=="microgram","mg",meds$unit)
summary(as.factor(meds$unit))

#I've cleaned the units and the dosages, now I just need to clean the quantity, or is that really necessary?
#I don't see how we'd need to use the quantity column... Let's not clean it for now.

#order rows for each person from oldest to newest prescriptions
# Order rows within each ID based on the Date column
library(dplyr)

meds <- meds%>%
  arrange(eid,issue_date)

#create new column grouping 0202, 0204, 0205 and 0206 into diuretics, b blockers, arbs/acei and ccbs
meds$med_cat <- ifelse(substr(meds$bnf_code, 1, 4) == "0202", "Diuretics",
                              ifelse(substr(meds$bnf_code,1,5) == "02.02", "Diuretics",
                                     ifelse(substr(meds$bnf_code, 1, 4) == "0204", "B blockers",
                                            ifelse(substr(meds$bnf_code, 1, 5) == "02.04", "B blockers",
                                                   ifelse(substr(meds$bnf_code, 1, 4) == "0205", "ACE inhibitors/ARBs",
                                                          ifelse(substr(meds$bnf_code, 1, 5) == "02.05", "ACE inhibitors/ARBs",
                                                                 ifelse(substr(meds$bnf_code, 1, 5) == "02.06", "Calcium channel blockers",
                                                                        ifelse(substr(meds$bnf_code, 1, 4) == "0206", "Calcium channel blockers", "Other"))))))))
meds$med_cat <- as.factor(meds$med_cat)
saveRDS(meds,"Data/cases_meds_final.rds")
