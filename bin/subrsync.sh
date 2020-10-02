#!/bin/bash
## user:=mrajeev
## cluster:=della.princeton.edu
## dirput:=analysis/slurm
## -r recurse through sub-directories
## -L transform symlink into reference file/dir
## -v be verbose about printing messages
## -z compresses data before transfer and decompresses after transfer
## -t pass the timestamp when syncing
## -q for quietly

# Push up the scripts to home
rsync -rLvztq --update --exclude '*.git' --exclude '.gitignore' --exclude "data-raw/raw*" --exclude "*.DS_Store*" --exclude ".Rproj*" --exclude analysis/out --exclude analysis/paper --exclude figs --exclude docs --exclude archive ~/Documents/Projects/MadaAccess mrajeev@della.princeton.edu:~/

# Push up the outputs to scratch (so always working from there for outputs)
rsync -rLvztq --update --exclude "*.DS_Store*" ~/Documents/Projects/MadaAccess/analysis/out mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/

