#!/bin/bash
#SBATCH --job-name=addclinics	 # create a short name for your job
#SBATCH --nodes=1		      # node count
#SBATCH --ntasks=18	        # total number of tasks across all nodes
#SBATCH --cpus-per-task=1	 # cpu-cores per task (>1 if multithread tasks)
#SBATCH --mem-per-cpu=4500	 # mem per CPU
#SBATCH --time=12:00:00          # total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin        # send mail when process begins
#SBATCH --mail-type=end	        # send email when job ends
#SBATCH --mail-user=mrajeev@princeton.edu

Rscript ./R/04_addclinics/02_ttimes_added.R
