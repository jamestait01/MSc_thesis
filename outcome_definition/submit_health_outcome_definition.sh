#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N definition

cd /rds/general/user/jt1019/home/Summer_project/outcome_definition/Scripts/

module load anaconda3/personal
source activate r413


def_path=/rds/general/user/jt1019/home/Summer_project/outcome_definition/Definitions/hypertension/

app_data_path=/rds/general/project/hda-22-23/live/TDS/General/Data/ukb669759.csv

hes_main_path=/rds/general/project/hda-22-23/live/TDS/General/Data/hesin.txt

hes_diag_path=/rds/general/project/hda-22-23/live/TDS/General/Data/hesin_diag.txt

hes_oper_path=/rds/general/project/hda-22-23/live/TDS/General/Data/hesin_oper.txt

death_main_path=/rds/general/project/hda-22-23/live/TDS/General/Data/death.txt

death_cause_path=/rds/general/project/hda-22-23/live/TDS/General/Data/death_cause.txt

Rscript extract_hes.R $def_path $app_data_path $hes_main_path $hes_diag_path $hes_oper_path

Rscript extract_death.R $def_path $app_data_path $death_main_path $death_cause_path

Rscript extract_baseline.R $def_path $app_data_path
