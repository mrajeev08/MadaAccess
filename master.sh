#!/bin/bash
# master script for compiling analyses
# if quick is true then skips the cluster steps and relies on existing outputs
# if clean is true removes all files generated from analyses (make them all go to output!)

# QUICK = commandlinearg 1
# CLEAN = commandlinearg 2

# spatial data
Rscript ./R/01_gis/01_process_rasters.R # ok

Rscript ./R/01_gis/03_aggregate_pop.R # ok
Rscript ./R/01_gis/04_run_baseline.R # ok
Rscript ./R/01_gis/05_make_shapefiles.R # ok

# serial (non-parallelized)
# Rscript ./R/01_gis/02_pop_to_pix.R serial
# locally parallelized with doParallel
# Rscript ./R/01_gis/02_pop_to_pix.R local

Rscript ./R/04_addclinics/02_ttimes_added.R local

Rscript ./R/05_predictions/02_vials_incremental.R local


sub -sn -t 12 -n 1 -mem 25000 -sp "./R/01_gis/02_pop_to_pix.R" -jn "pop2pix" -dd
"~/Documents/Projects/MadaAccess/data/processed/rasters/" -re "mrajeev@della.princeton.edu:~/MadaAccess/data/processed/rasters/wp_2015_temp.tif" -wt 1m -n@

sub -t 12 -n 18 -sp "./R/03_bitemodels/01_run_bitemods.R" -jn "bitemods" -dd "~/Documents/Projects/MadaAccess/output/mods/" -re "mrajeev@della.princeton.edu:~/MadaAccess/output/mods/" -wt 2m

sub -sn -t 12 -n 18 -mem 4500 -sp "./R/04_addclinics/02_ttimes_added.R" -jn "addclinics" -dd "~/Documents/Projects/MadaAccess/output/ttimes/" -re "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ttimes/addclinics*" -wt 5m

sub -t 12 -n 10 -mem 6000 -sp "./R/06_sensitivity/03_burden_se.R" -jn "burden_se" -wt 5m -n@

sub -t 12 -n 30 -mem 3000 -sp "./R/05_predictions/02_vials_incremental.R" -jn "vials" -wt 5m -n@

sub -t 12 -n 30 -mem 3000 -sp "./R/06_sensitivity/05_vial_se.R" -jn "vials" -wt 5m -n@

sub -sn -t 12 -n 18 -mem 4500 -sp "./R/04_addclinics/02_ttimes_added.R" -jn "addclinics" -wt 5m


# rsync pull down outputs
rsync -rLvz --update  ~/Documents/Projects/MadaAccess/output
# bite data
Rscript ./R/02_bitedata/01_match_names.R
# put in stop to ask the user whether they've matched the names manually!
Rscript ./R/02_bitedata/02_output_processed.R
Rscript ./R/02_bitedata/03_estimate_biteinc.R

# run mods
# rsync up to della!
# bash? Rscript /R/03_bitemodels/01_run_bitemods.R # if statement here
Rscript ./R/03_bitemodels/02_get_modpreds.R

# add clinics
# rsync up to della!
bash bash/scripts/candidates.sh # if statement here
bash bash/scripts/addclinics.sh # if statement here
Rscript ./R/04_addclinics/03_ttimes_max.R
Rscript ./R/04_addclinics/04_postprocess_ttimes.R

# predictions
# rsync up to della!
R/05_predictions/01_burden_incremental.R
# bash R/05_predictions/02_vials_incremental.R # if statement here
R/05_predictions/03_postprocess_preds.R
R/05_predictions/bites_by_catch.R

# sensitivity analyses (parallelize and send to cluster?)
R/06_sensitivity/01_data_se.R
R/06_sensitivity/02_model_se.R
R/06_sensitivity/03_burden_se.R

# figures

# process out.session

# split & process bib separately

# knit doc with separate bibliographies

# one word doc for sharing with collaborators
# one html for github pages
# submission files per medRvix/PLoS NTDs

# way to process output locations & input locations without having to specify on the command line!
grep "sync_to" ./R/05_predictions/02_vials_incremental.R | cut -f 2 -d '-'
