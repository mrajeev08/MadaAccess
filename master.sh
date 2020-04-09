#!/bin/bash
# master script for compiling analyses
# if quick is true then skips the cluster steps and relies on existing outputs
# if clean is true removes all files generated from analyses (make them all go to output!)

# ACTION=$1 # if empty then will compile it; if 'clean' then will clean it
# ACTION=$2 # if empty then will do it cheap (only cleaning/running things locally & keeping cluster outputs); if 'expensive' then will delete everything & rerun
# --dryrun will just print which files on cluster vs. locally (or which files will be deleted!)

# Step 1: spatial data
Rscript --vanilla ./R/01_gis/01_process_rasters.R # ok
sub -sn -t 12 -n 1 -mem 25000 -sp "./R/01_gis/02_pop_to_pix.R" -jn "pop2pix" -wt 1m -n@
Rscript --vanilla ./R/01_gis/03_aggregate_pop.R # ok
Rscript --vanilla ./R/01_gis/04_run_baseline.R # ok
Rscript --vanilla ./R/01_gis/05_make_shapefiles.R # ok
Rscript --vanilla ./R/01_gis/06_groundtruth_shapefiles.R # ok

# Step 2: bitedata (skip pre-processing of raw data)
Rscript --vanilla ./R/02_bitedata/03_estimate_biteinc.R
Rscript --vanilla ./R/02_bitedata/04_bitedata_se.R

# Step 3: bite models
sub -t 12 -n 18 -sp "./R/03_bitemodels/01_run_bitemods.R" -jn "bitemods" -wt 2m -n@
Rscript --vanilla .R/03_bitemodels/02_test_OD.R
Rscript --vanilla .R/03_bitemodels/03_get_modpreds.R
sub -t 12 -n 18 -sp "./R/03_bitemodels/02_model_se.R" -jn "mod_se" -wt 2m -n@

# Step 4: add clinics
sub -sn -t 12 -n 18 -mem 4500 -sp "./R/04_addclinics/01_ttimes_candidates.R" -jn "candidates" -wt 5m -n@
sub -sn -t 12 -n 18 -mem 4500 -sp "./R/04_addclinics/02_ttimes_added.R" -jn "addclinics" -wt 5m -n@
Rscript --vanilla .R/04_addclinics/03_ttimes_max.R
Rscript --vanilla .R/04_addclinics/04_postprocess_ttimes.R

# Step 5: preds all
Rscript --vanilla .R/05_predictions/01_burden_incremental.R
sub -t 12 -n 30 -mem 3000 -sp "./R/05_predictions/02_vials_incremental.R" -jn "vials" -wt 5m -n@

# Step 6: sensitivity
Rscript --vanilla .R/06_sensitivity/01_se_pars.R
sub -t 12 -n 10 -mem 7000 -sp "./R/06_sensitivity/02_burden_se.R" -jn "burden_se" -wt 5m -n@
Rscript --vanilla .R/06_sensitivity/03_scaling_se.R
sub -t 12 -n 30 -mem 3000 -sp "./R/06_sensitivity/04_vial_se.R" -jn "vials_se" -wt 5m -n@

# Figures
FILES=R/figures/*
for f in $FILES
do
echo "$f"
Rscript --vanilla "$f"
done
