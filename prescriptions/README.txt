identifying_eids_with_pres_data.rds is used to remove hypertensive participants with no prescription data available.

01-med_extraction extracts anti-hypertensive medications of interest from gp_scripts. It produces 3 files cases_meds, containing all anti-hypertensive medications (before and after enrollment with repeated prescriptions), cases_no_meds (cases with no anti-hypertensive prescriptions), and cases_unique_meds (unique anti-hypertensive medications used to screen the drugs). 

02-meds_unique_cleaning cleans cases_unique_meds and produces a list of unique medications, that are then manually screened to produce a csv file 'medications_list_cleaned'.

03-meds_full_cleaning preprocesses cases_meds using medications_list_cleaned to only keep relevant anti-hypertensive medications.
 
04-baseline_meds_creation further preprocesses cases_meds, keeping only medications that are before Biobank recruitment for each person. 2 datasets are created, 'old_meds' which contains participants with medications >6 months before recruitment, and 'six_monthmeds', which keeps people that were prescribed medications <6 months before recruitment. This dataset is further cleaned/transposed into 'transposed_meds'. Finally, transposed_meds is screened to only keep medication combinations that adhere to NICE guidelines, giving rise to NICE_medications.rds.

05-old_meds_analysis does some EDA on the old_meds dataset to try and understand why we had hypertensive people that were not prescribed medication recently.

06-end_of_study_meds extracts the latest medication for each person, grouping them into categories and creates the medication change variable indicating people that stayed on the same medication vs increased dosage etc.