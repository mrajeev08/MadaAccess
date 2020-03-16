#!/bin/bash
#SBATCH --job-name=bitemods	 # create a short name for your job
#SBATCH --ntasks=18	        # total number of tasks across all nodes
#SBATCH --cpus-per-task=1	 # cpu-cores per task (>1 if multithread tasks)
#SBATCH --mem-per-cpu=4000	 # mem per CPU
#SBATCH --time=12:00:00          # total run time limit (HH:MM:SS)

export OMPI_MCA_btl='tcp,self,sm'
module load openmpi/gcc/2.0.2
srun Rscript ./R/06_sensitivity/02_model_se.R
