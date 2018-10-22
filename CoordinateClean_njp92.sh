#!/bin/bash

#---- ABOUT ----
# by Nancy Peterson, NAU
# njp92@nau.edu
# last updated 08/15/2018

# Purpose:
# Clean the unzipped GBIF file occurrences and save them to a new directory

# ---- Inputs ----
# - SLURM - 
#SBATCH --job-name=batch_GBIF_download
#SBATCH --output=/scratch/njp92/GBIF_Download/2_clean/20180815_%A_%a.out
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-20%10
#SBATCH --time=1:00:00
#SBATCH --mem=500M

unzipdir='/scratch/njp92/GBIF_Download/1_Unzipped_Data/'
cleandir='/scratch/njp92/GBIF_Download/2_clean/'

# ----STARTUP----
start=`date +%s`
echo "Started at:"
date
echo

# Load any required modules

echo "SLURM_JOBID: "$SLURM_JOBID
echo "SLURM_ARRAY_JOB_ID: "$SLURM_ARRAY_JOB_ID
echo "SLURM_ARRAY_TASK_ID: "$SLURM_ARRAY_TASK_ID
echo

#----PROCESSING----
cd $unzipdir

# make the file list from the input directory
infiles=(${unzipdir}*/'occurrence.txt')

#associate with a SLURM array task ID
oneinfile=${infiles[$SLURM_ARRAY_TASK_ID - 1]}

# parameter substitution. We want to rename the output files with the 5 degree folder names. So we take the string and (because it's a %) cut from the right and get rid of the /occurrence.txt

v1=${oneinfile%/occurrence.txt} #to keep everything before occurrence
CoordBox=${v1#$unzipdir} #to keep everything after unzipdir(i.e. the 5 degree coordinate boxes)


#activate group GEDI (because that's the version of R I want)
source activate Grp_GEDI 

#load the function which is stored in an R script
Rscript /home/njp92/scripts/CoordinateClean.R $oneinfile $cleandir $CoordBox


# ----- ENDING -----
echo "Ended at:"
date
echo
end=`date +%s`
totTime=$((end-start))
echo Total time: $totTime sec
