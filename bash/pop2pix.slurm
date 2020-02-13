#!/bin/bash
#SBATCH --job-name=pop2pix	 # create a short name for your job
#SBATCH --nodes=1		      # node count
#SBATCH --ntasks=10	        # total number of tasks across all nodes
#SBATCH --cpus-per-task=1	 # cpu-cores per task (>1 if multithread tasks)
#SBATCH --mem-per-cpu=25000	 # mem per CPU
#SBATCH --time=12:00:00          # total run time limit (HH:MM:SS)

srun Rscript ./R/01_gis/02_pop_to_pix.R
