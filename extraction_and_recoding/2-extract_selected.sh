#PBS -l walltime=5:00:00 
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N extraction

cd /rds/general/user/jt1019/home/Summer_project/extraction_and_recoding/scripts

module load anaconda3/personal
source activate r413

ukb_path=/rds/general/project/hda-22-23/live/TDS/General/Data/ukb669759.csv
Rscript 2-extract_selected.R $ukb_path 

