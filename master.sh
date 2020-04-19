#!/bin/bash
# positional argument 1 is the directory
# positional argument 2 is whether you want to run the cluster jobs
# add "nocl" if you want to skip the cluster jobs, defaults to running them!
# positional argument 3 is whether you want to do a dryrun (i.e. see what will be done rather than run it)
# add "dry" to do a dryrun, defaults to running them!
# example: bash test.sh "R/01_gis/*" "cl" "nocl" # have to quote the args!

FILES=$1

if grep -q "no" <<< $2;
then
    echo "Skipping cluster jobs!"
else
    read -p "Running cluster jobs, are you vpn'd in to della (y/n)?" choice
    case "$choice" in 
        y|Y ) echo "Running cluster jobs!";;
        n|N ) exit;;
        * ) echo "invalid";exit;;
    esac
fi

for f in $FILES
do
    if grep -q "log_cluster" "$f";
    then
        if grep -q "nocl" <<< $2;
        then 
        echo "cluster job not run: $f"
        else
        cmd=$(grep "sub_cmd"  "$f" | cut -f 2 -d =) # sub args from script
            if grep -q "dry" <<< $3;
            then
            echo "cluster cmd: sub $cmd" 
            else
            sub $cmd
            fi
        fi
    else
        if grep -q "dry" <<< $3;
        then 
        echo "local: $f"
        else
        Rscript --vanilla "$f"
        fi
    fi
done
