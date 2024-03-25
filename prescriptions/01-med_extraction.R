
#The gp_scripts data is 4GB with 56 million rows, so it's too large to work with in it's entirety

#load the first 1000 rows to get a feel for what's in the data

file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/gp_data/gp_scripts.txt"
num_rows_to_read <- 1000


data <- matrix(nrow = num_rows_to_read, ncol = 8)  #Assuming 8 columns
con <- file(file_path, "r")
for (i in 1:num_rows_to_read) {
  line <- readLines(con, n = 1)
  values <- unlist(strsplit(line, "\t"))  #Assuming tab-separated values
  data[i, ] <- values
}
close(con)

pres_data <- as.data.frame(data)

View(pres_data)
colnames(pres_data)
#convert first row to column names
colnames(pres_data) = pres_data[1,]
View(pres_data)

#remove now redundant first row
pres_data <- pres_data[-1,]
View(pres_data)

#some of the bnf codes have a different format, let's look further
subset_df <- pres_data[grepl("\\.", pres_data$bnf_code), ]
#there are 8 columns, eid, data provider (whether record is from england, scotland or wales)
# issue_date = issue of prescription, read_2 = read v2 code, a coded thesaurus of clinical terms like icd10 icd9 etc
# bnf_code = drug info (what bnf chapter it's for treating, etc.)

#bnf codes beginning with 0205 are drugs used for hypertension and heart failure.
#0202 is for diuretics
#0204 is for β blockers
#0205 is for ACE inhibitors/ARBs
#0206 is for calcium channel blockers

#note, columns are coded as 0202 or sometimes by 02.02. We need to extract both


#however, let's first see if there are ANY OTHER ways of writing the bnf codes

file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/gp_data/gp_scripts.txt"

chunk_size <- 1000000  # Number of rows to process in each chunk

#Initialize an empty vector to store unique values
#unique_values <- c()

#Open the file connection
#con <- file(file_path, open = "r")
#header <- readLines(con, n = 1)  #read header line
#col_names <- unlist(strsplit(header, "\t"))  #split the header by tabs

#bnf_code_col <- which(col_names == "bnf_code")  #Identify the column index of "bnf_code"


#read the file in chunks
#while (length(lines <- readLines(con, n = chunk_size)) > 0) {
  # Process each line in the chunk
  #for (line in lines) {
    #values <- strsplit(line, "\t")[[1]]
    #bnf_code <- values[bnf_code_col]
    #unique_values <- unique(c(unique_values, bnf_code))
  #}
#}

# Close the file connection
#close(con)

# save the unique values

write(unique_values,"bnf_codes.txt")

#finds list of unique bnf codes beginning with 02
file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/bnf_codes.txt"
lines <- readLines(file_path)

matching_strings <- character(0)  #vector to store matching strings

for (line in lines) {
  if (startsWith(line, "02")) {
    matching_strings <- c(matching_strings, line)
  }
}

matching_strings

#repeat the above but for 2, 

file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/bnf_codes.txt"
lines <- readLines(file_path)

matching_strings <- character(0)  # Empty character vector to store matching strings

for (line in lines) {
  if (startsWith(line, "2")) {
    matching_strings <- c(matching_strings, line)
  }
}

matching_strings
#no other matching strings, so we have got everyone with hypertensive medications now.

#attempts to read the gp_scripts file line by line, only extracting rows where bnf code starts with 0202, 0204, 0205 or 0206


con <- file(file_path, "r")
header <- readLines(con, n = 1)  # Read the header line
col_names <- unlist(strsplit(header, "\t"))  # Split the header by tabs

bnf_code_col <- which(col_names == "bnf_code")  # Identify the column index of "bnf_code"

matching_rows <- list()

# Define the desired bnf_code values
desired_codes <- c("0202","02.02", "0204","02.04", "0205","02.05", "0206","02.06")

num_rows_to_read <- 10000  # Update the number of rows to read

# Initialize an empty list to store the matching rows
matching_rows <- list()

# Read the file line by line and check for people in with listed bnf codes, for the first 10,000 rows
#this is a test to see if the code works 

counter <- 0  # Initialize a counter variable

while (counter < num_rows_to_read && length(line <- readLines(con, n = 1)) > 0) {
  values <- unlist(strsplit(line, "\t"))
  if (startsWith(values[bnf_code_col], "0202") ||
      startsWith(values[bnf_code_col], "02.02") ||
      startsWith(values[bnf_code_col], "0204") ||
      startsWith(values[bnf_code_col], "02.04") ||
      startsWith(values[bnf_code_col], "0205") ||
      startsWith(values[bnf_code_col], "02.05") ||
      startsWith(values[bnf_code_col], "0206") ||
      startsWith(values[bnf_code_col], "02.06")) {
    matching_rows[[length(matching_rows) + 1]] <- values
  }
  
  counter <- counter + 1  # Increment the counter
}
close(con)

# Combine the matching rows into a data frame
pres_data <- as.data.frame(do.call(rbind, matching_rows), stringsAsFactors = FALSE)
names(pres_data) <- col_names

View(pres_data)

#####################################so the code works, now let's run it on the full dataset#################

con <- file(file_path, "r")
header <- readLines(con, n = 1)  # Read the header line
col_names <- unlist(strsplit(header, "\t"))  # Split the header by tabs

bnf_code_col <- which(col_names == "bnf_code")  # Identify the column index of "bnf_code"

matching_rows <- list()

# Define the desired bnf_code values
desired_codes <- c("0202","02.02", "0204","02.04", "0205","02.05", "0206","02.06")
matching_rows <- list()

# Read the file line by line and check for the desired pattern
while (length(line <- readLines(con, n = 1)) > 0) {
  values <- unlist(strsplit(line, "\t"))
  if (startsWith(values[bnf_code_col], "0202") ||
      startsWith(values[bnf_code_col], "02.02") ||
      startsWith(values[bnf_code_col], "0204") ||
      startsWith(values[bnf_code_col], "02.04") ||
      startsWith(values[bnf_code_col], "0205") ||
      startsWith(values[bnf_code_col], "02.05") ||
      startsWith(values[bnf_code_col], "0206") ||
      startsWith(values[bnf_code_col], "02.06")) {
    matching_rows[[length(matching_rows) + 1]] <- values
  }
}
close(con)

# Combine the matching rows into a data frame
pres_data <- as.data.frame(do.call(rbind, matching_rows), stringsAsFactors = FALSE)
names(pres_data) <- col_names

#count number of unique eids in pres data

library(dplyr)
n_distinct(pres_data$eid)

#save hypertension prescription data
saveRDS(pres_data,"/rds/general/project/hda-22-23/live/Summer_projects/jt1019/Data/hypertensive_pres.rds")

#now we have everyone with medications, let's merge the hypertensive medication data to the cleaned_data

data_cleaned <- readRDS("Data/data_cleaned.rds")
final_df <- merge(data_cleaned,pres_data,by="eid",all.x=TRUE)

#convert relevant variables

final_df$data_provider <- as.factor(final_df$data_provider)
#final_df$issue_date <- as.Date(final_df$issue_date)
final_df$read_2 <- as.factor(final_df$read_2)
final_df$bnf_code <- as.factor(final_df$bnf_code)
final_df$dmd_code <- as.factor(final_df$dmd_code)
final_df$drug_name <- as.factor(final_df$drug_name)

library(dplyr)
n_distinct(final_df$eid)
#502,020 eids are in final_df (everyone in the biobank)

#only keep people diagnosed with hypertension

cases <- final_df %>%
  filter(case==1)

n_distinct(cases$eid)
#back to 452000 people

#create separate df's for everyone with no hypertensive meds and everyone with hypertensive meds
no_meds <- cases[is.na(cases$drug_name), ]

meds <- cases[!is.na(cases$drug_name),]

n_distinct(meds$eid)

 
#group ethnicities with <2% prevalence into 'Other category'

# Calculate the prevalence of each factor level
factor_counts <- table(no_meds$ethnicity.0.0)
factor_prevalence <- prop.table(factor_counts)

#Set the threshold for grouping levels (2%)
threshold <- 0.02

#Identify levels to be grouped as "Other"
other_levels <- names(factor_prevalence[factor_prevalence < threshold])

#Reclassify levels as "Other"
no_meds$ethnicity.0.0 <- factor(no_meds$ethnicity.0.0)
levels(no_meds$ethnicity.0.0)[levels(no_meds$ethnicity.0.0) %in% other_levels] <- "Other"

summary(no_meds$ethnicity.0.0)


saveRDS(no_meds,"Data/cases_no_meds.rds")
saveRDS(meds,"Data/cases_meds.rds")

n_distinct(meds$eid)
#There are 8498405 rows in meds df yet only 87000 unique ids. There are a lot of duplicated rows where the
#same pres is represcribed over and over, the only difference being the prescription date. Can we 
#remove these duplicated rows? This will help us identify the unique drugs that are prescribed.

# Create a logical vector indicating duplicated rows based on all columns except "issue_date"
dup_rows <- duplicated(meds[, -24])

# Subset the dataframe to exclude the duplicated rows
unique_meds <- subset(meds, !dup_rows)

#create new column grouping 0202, 0204, 0205 and 0206 into diuretics, b blockers, arbs/acei and ccbs
unique_meds$med_cat <- ifelse(substr(unique_meds$bnf_code, 1, 4) == "0202", "Diuretics",
                              ifelse(substr(unique_meds$bnf_code,1,5) == "02.02", "Diuretics",
                                     ifelse(substr(unique_meds$bnf_code, 1, 4) == "0204", "B blockers",
                                            ifelse(substr(unique_meds$bnf_code, 1, 5) == "02.04", "B blockers",
                                                   ifelse(substr(unique_meds$bnf_code, 1, 4) == "0205", "ACE inhibitors/ARBs",
                                                          ifelse(substr(unique_meds$bnf_code, 1, 5) == "02.05", "ACE inhibitors/ARBs",
                                                                 ifelse(substr(unique_meds$bnf_code, 1, 5) == "02.06", "Calcium channel blockers",
                                                                        ifelse(substr(unique_meds$bnf_code, 1, 4) == "0206", "Calcium channel blockers", "Other"))))))))

saveRDS(unique_meds,"Data/cases_unique_meds.rds")
