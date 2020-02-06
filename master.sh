#!/bin/bash
# master script

# spatial data
Rscript /R/01_gis/01_process_rasters.R
bash bash/scripts/pop2pix.sh # cluster (TO DO PULL DOWN OUT SESSION TOO!)
Rscript /R/01_gis/03_aggregate_pop.R
Rscript /R/01_gis/04_run_baseline.R
Rscript /R/01_gis/05_make_shapefiles.R


