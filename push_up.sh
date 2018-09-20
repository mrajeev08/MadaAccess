#!/bin/bash
## -r recurse through sub-directories
## -L transform symlink into reference file/dir
## -v be verbose about printing messages
## -z compresses data before transfer and decompresses after transfer

rsync -rLvz --update --exclude '*.git' --exclude '.Rproj*' --exclude archive --exclude '.gitignore' --exclude 'README.md' --exclude 'push_up.sh' --exclude lit ~/Documents/MadaAccess/ ~/Dropbox/MadaPEP/Mal_analyses/
