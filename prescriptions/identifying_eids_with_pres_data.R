#Goal: Identify UKB participants for whom we have prescription data

#get file path of prescription data
pres_file_path <- "/rds/general/project/chadeau_ukbb_folder/live/data/project_data/UKB_669759/gp_data/gp_scripts.txt"

#load first column in gp_scripts.txt (eid column)
pres_eids <- read.table(pres_file_path,
                    colClasses = c(rep("character", 1), rep("NULL",12)),fill=T)

#remove first row (eid)
pres_eids <- pres_eids[-1,]

#remove duplicate eids 
pres_eids <- unique(pres_eids)

#save file 
saveRDS(pres_eids,"~/AI-Respire/prescription_data/eids_with_prescriptions.rds")

