#!/bin/bash
# pull down all output & data-raw into a zenodo


# And make sure all cluster results are pulled down (may take a bit!) (only will pull down if newer than local ones
# rsync -rLvzt --update --exclude "*.DS_Store*"  mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/* ~/Documents/Projects/MadaAccess/analysis/out

if [ ! -d "zenodo" ];
then
mkdir -p zenodo/analysis
mkdir -p zenodo/data-raw
fi

# clean it out
rm -r zenodo/analysis/out/*
rm -r zenodo/data-raw

# Move over newest files
cp -r analysis/out  zenodo/analysis/

# Do data raw (don't copy as you have to exclude various bits
rsync -rLvzt --update --exclude "*.DS_Store*" --exclude data-raw/raw/ipm_data --exclude data-raw/out --exclude data-raw/src --exclude "data-raw/log*" data-raw zenodo/

