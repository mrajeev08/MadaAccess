#!/bin/bash
## -r recurse through sub-directories
## -L transform symlink into reference file/dir
## -v be verbose about printing messages
## -z compresses data before transfer and decompresses after transfer

rsync -rLvz --update --exclude '*.git' --exclude '.Rproj*' --exclude archive --exclude '.gitignore' --exclude 'README.md' --exclude sync --exclude 'accessPEP*' --exclude figs --exclude format_docs --exclude lit --exclude 'candidate_matrix.gz' ~/Documents/Projects/MadaAccess mrajeev@della.princeton.edu:~/
ssh della
sbatch slurm/vials

