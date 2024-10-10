#Goal: Extract blood pressure codes from gp_clinical and do some cleaning (get average BP for each person & date)

#get file path of gp clinical data data
file_path <- "/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_669759/gp_data/gp_clinical.txt"

#load sample of clinical data
tmp <- readLines(file_path,n=1000)
#\t shows tab delimited file

#load entire dataset
clinical <- read.delim(file_path, header = TRUE, sep = "\t",quote="")

library(ConceptLibraryClient)

#Connect to API

client = ConceptLibraryClient::Connection$new(public=TRUE)

#Get details of phenotype
bp_measurement  <- client$phenotypes$get_codelist(
  'PH408',
  version_id=816
)

bp  <- client$phenotypes$get_codelist(
  'PH16',version_id=32
)

library(tidyverse)

#get vector of v2/v3 codes
bp_codes <- c(bp_measurement$code,bp$code,"246")

#remove 00s from bp_codes, otherwise rows may not be picked up, eg 246. rows won't be kept if bp_code is 246.00
bp_codes <- gsub(".00",".",bp_codes)

#code works, run on full dataset, only keep rows with read codes in v2_codes
clinical_bp <- clinical %>%
  filter(if_any(all_of(c("read_2","read_3")),~grepl((paste(bp_codes,collapse="|")),.)))

#246A = diastolic, 2469 = systolic
clinical_bp$event_dt <- as.Date(clinical_bp$event_dt,format="%d/%m/%Y")

#arrange by eid and date
clinical_bp <- clinical_bp %>% arrange(event_dt,by_group=eid)

#remove missing dates
dates <- as.Date(c("1901-01-01","1902-02-02","1903-03-03","2037-07-07"),format="%Y-%m-%d")

clinical_bp <- clinical_bp[!(clinical_bp$event_dt %in% dates),]

#replace any blank strings with NA values
clinical_bp <- clinical_bp %>%
  mutate(across(c(value1,value2,value3), ~ na_if(.,"")))

#logical for each column and row whether na or not
idx <- is.na(clinical_bp[,6:8])
#sum up no. of nas in value1-3 for each row
idx_na <- apply(idx,1,sum)

#keep rows with at least one value in value1-3 columns, as they're likely to be BP readings
clinical_bp <- clinical_bp[!(idx_na==3),]

saveRDS(clinical_bp,"~/Summer_project/hypermarker/clinical_bp.rds")


###############################################################################
clinical_bp <- readRDS("~/Summer_project/hypermarker/clinical_bp.rds")

summary(as.factor(clinical_bp$read_2))
summary(as.factor(clinical_bp$read_3))


#let's look at each code and see if they're relevant and/or how to clean
#read3 codes
read3_list <- split(clinical_bp, clinical_bp[["read_3"]])
names(read3_list)
View(read3_list[["246E."]])

#"" indicates read2 code instead
#246.. is BP reading, no cleaning needed here 
#2464. says normal BP - no other info, remove
#2469. is systolic reading
#246A is diastolic reading
#246C, 246D and 246E refer to sitting, standing or lying BP measurements, though they mostly contain 0 values, 
#keep 2469.,246A and 246..

clinical_bp_read3 <- clinical_bp[clinical_bp$read_3=="246A." | clinical_bp$read_3=="2469." | clinical_bp$read_3=="246..",]

#one row for each measurement for each person/date, each measurement separated by ,
clinical_bp_read3 <- clinical_bp_read3 %>%
  group_by(eid,event_dt) %>%
  reframe(value1 = paste(value1,collapse=","),value2=value2,value3=value3,
          data_provider=data_provider,read_2=read_2,read_3=paste(read_3,collapse=","))%>%
  ungroup()

#replace NA's with empty strings
#clinical_bp_read3$value1 <- gsub("NA,","",clinical_bp_read3$value1)

sum(grepl("[A-Za-z]",clinical_bp_read3$value1)) #check if any non-number characters present in any rows
#nope, good to go

summary(as.factor(clinical_bp_read3$value2))
summary(as.factor(clinical_bp_read3$value3))
#only nas in value2,value3, good.

#remove duplicate rows
clinical_bp_read3 <- unique(clinical_bp_read3)

library(data.table)
library(dplyr)

#function to clean BP measurements
BP_averages <- function(data) {
  for (row in 1:nrow(data)) {
    #separate out each read3 code into separate list
    codes <- tstrsplit(data$read_3[row],",",fixed=T)
    
    #get indexes of list elements with diastolic BP (246A.)
    idx <- lapply(codes, function(x) any(x=="246A."))
    values <- tstrsplit(data$value1[row],",",fixed=T)
    diastolic_bps <- values[idx==TRUE]
    systolic_bps <- values[idx==FALSE]
    diastolic_mean <- mean(as.numeric(unlist(diastolic_bps)))
    systolic_mean <- mean(as.numeric(unlist(systolic_bps)))
    
    #replace value1 and value 2 column with mean systoic/diastolic BP
    data$value1[row] <- systolic_mean
    data$value2[row] <- diastolic_mean
  }
  #rename columns
  data <- rename(data,systolic_mean =value1,diastolic_mean=value2)
  data$systolic_mean <- as.numeric(data$systolic_mean)
  data$diastolic_mean <- as.numeric(data$diastolic_mean)
  return(data)
}

  
#split data into chunks for parallelisation
nchunks=7

library(tidyr)

#split df into list of smaller dfs
list <- clinical_bp_read3 %>% 
  group_by((row_number()-1) %/% (n()/nchunks)) %>%
  nest %>% pull(data)

library(parallel)
cores <- 7
cl <- makeCluster(7)

#export relevant stuff to cluster
clusterExport(cl, c("list", "BP_averages"))
clusterEvalQ(cl, c(library(data.table),library(dplyr)))

output <- parLapply(cl=cl,X=list, fun=BP_averages)

stopCluster(cl)

output <- bind_rows(output)

saveRDS(output,"~/Summer_project/hypermarker/readv3_bp.rds")

############################################################################repeat for read v2 codes
#read2 codes
read2_list <- split(clinical_bp, clinical_bp[["read_2"]])

summary(as.factor(clinical_bp$read_2))

names(read2_list)
#scan each code manually to see if worth keeping
View(read2_list[["2462."]])

#let's keep the following to minimise bias, these are also the most common:
#246.. O/E BP reading
#2469	O/E - Systolic BP reading
#246A	O/E - Diastolic BP reading

clinical_bp_read2 <- clinical_bp[clinical_bp$read_2=="246A." | clinical_bp$read_2=="2469." | clinical_bp$read_2=="246..",]

#one row for each measurement for each person/date, each measurement separated by ,
clinical_bp_read2 <- clinical_bp_read2 %>%
  group_by(eid,event_dt) %>%
  reframe(value1 = paste(value1,collapse=","),value2=paste(value2,collapse=","),value3=paste(value3,collapse=","),
          data_provider=data_provider,read_2=paste(read_2,collapse=","),read_3=read_3) %>%
  ungroup()

View(clinical_bp_read2)
#replace NA's with empty strings
clinical_bp_read2$value1 <- gsub("NA,","",clinical_bp_read2$value1)
clinical_bp_read2$value1 <- gsub(",NA","",clinical_bp_read2$value1)

clinical_bp_read2$value2 <- gsub("NA,","",clinical_bp_read2$value2)
clinical_bp_read2$value2 <- gsub(",NA","",clinical_bp_read2$value2)

clinical_bp_read2$value3 <- gsub("NA,","",clinical_bp_read2$value3)
clinical_bp_read2$value3 <- gsub(",NA","",clinical_bp_read2$value3)

#remove duplicate rows
clinical_bp_read2 <- unique(clinical_bp_read2)

sum(grepl("[A-Za-z]",clinical_bp_read2$value1)) #check if any non-number characters present in any rows
sum(grepl("[A-Za-z]",clinical_bp_read2$value2)) #check if any non-number characters present in any rows

clinical_bp_read2$value1 <- gsub("NA",NA,clinical_bp_read2$value1)
clinical_bp_read2$value2 <- gsub("NA",NA,clinical_bp_read2$value2)
clinical_bp_read2$value3 <- gsub("NA",NA,clinical_bp_read2$value3)

sum(grepl("[A-Za-z]",clinical_bp_read2$value1)) #check if any non-number characters present in any rows
sum(grepl("[A-Za-z]",clinical_bp_read2$value2)) #check if any non-number characters present in any rows

#remove the 6 rows with strange vals in value1
clinical_bp_read2 <- clinical_bp_read2[!grepl("[A-Za-z]", clinical_bp_read2$value1),]
summary(as.factor(clinical_bp_read2$value2))
summary(as.factor(clinical_bp_read2$value3))
#value 3 column mostly contains units, ignore

library(data.table)
library(dplyr)

#function to clean BP measurements
BP_averages_r2 <- function(data) {
  for (row in 1:nrow(data)) {
    if(is.na(data$value1[row])) {
      next
      value1 <- tstrsplit(data$value1[row],",",fixed=T)
    }
    if (is.na(data$value2[row])) {
      next
      value2 <- tstrsplit(data$value2[row],",",fixed=T)
    }
    #combine measurements into 1 vector
    measurements <- as.numeric(unlist(c(value1,value2)))
    #re-order from largest to smallest on the assumption that systolic values > diastolic values
    measurements <- sort(measurements,decreasing=TRUE)
    n <- length(measurements)
    sys_measurements <- measurements[1:(n/2)]
    dias_measurements <- measurements[((n/2)+1):n]
    diastolic_mean <- mean(dias_measurements)
    systolic_mean <- mean(sys_measurements)
    
    #replace value1 and value 2 column with mean systolic/diastolic BP
    data$value1[row] <- systolic_mean
    data$value2[row] <- diastolic_mean
    
  #rename columns
  }
  data <- rename(data,systolic_mean =value1,diastolic_mean=value2)
  return(data)
}

BP_averages_r2 <- function(data) {
  for (row in 1:nrow(data)) {
    value1 <- NULL
    value2 <- NULL
    
    #if value 1/2 isn't NA, split
    if (!is.na(data$value1[row])) {
      value1 <- tstrsplit(data$value1[row], ",", fixed = TRUE)
    }
    if (!is.na(data$value2[row])) {
      value2 <- tstrsplit(data$value2[row], ",", fixed = TRUE)
    }
    
    #combine measurements into one vector if one isn't NA
    if (!is.null(value1) | !is.null(value2)) {
      measurements <- as.numeric(unlist(c(value1, value2)))
      #remove 0s from vector
      measurements <- measurements[measurements!=0]
      
      #re-order from largest to smallest on the assumption that systolic values > diastolic values
      measurements <- sort(measurements, decreasing = TRUE)
      n <- length(measurements)
      sys_measurements <- measurements[1:(n / 2)]
      dias_measurements <- measurements[((n / 2) + 1):n]
      diastolic_mean <- mean(dias_measurements)
      systolic_mean <- mean(sys_measurements)
      
      #replace values with mean systolic/diastolic BP
      data$value1[row] <- systolic_mean
      data$value2[row] <- diastolic_mean
    }
  }
  
  #rename columns
  data <- rename(data, systolic_mean = value1, diastolic_mean = value2)
  data$systolic_mean <- as.numeric(data$systolic_mean)
  data$diastolic_mean <- as.numeric(data$diastolic_mean)
  return(data)
}

#split data into chunks for parallelisation
nchunks=7

library(tidyr)

#split df into list of smaller dfs
list <- clinical_bp_read2 %>% 
  group_by((row_number()-1) %/% (n()/nchunks)) %>%
  nest %>% pull(data)

library(parallel)
cores <- 7
cl <- makeCluster(7)

#export relevant stuff to cluster
clusterExport(cl, c("list", "BP_averages_r2"))
clusterEvalQ(cl, c(library(data.table),library(dplyr)))

output <- parLapply(cl=cl,X=list, fun=BP_averages_r2)

stopCluster(cl)

output <- bind_rows(output)

saveRDS(output,"~/Summer_project/hypermarker/readv2_bp.rds")

