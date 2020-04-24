#!/bin/bash
usage=$(cat <<-END
    runit [-h] [-d] [-cl] [--dry]

    This utility will run all R scripts in a directory, if numbered
    then in the order they are numbered. Defaults to running all scripts
    in subdirectories of directory R:
        runit -d "R/*/*"

    accepted options are:
    -h, --help       show help message and exit
    -d, --dir        the directory path, quoted
    -cl, --cluster   whether to run the cluster scripts, defaults to not running them,
                     if passed, will ask you whether vpn-ed in to della
    --dry            dryrun, just prints the scripts and whether they are run
                     locally or on cluster
END
)


dryrun=0
cl=0
FILES="R/*/*"

while [ "$1" != "" ]; do
    case $1 in
    -d | --dir )           shift
                            FILES=$1
                            ;;
    --dry )                 dryrun=1
                            ;;
    -cl | --cluster )      cl=1
                            ;;
    -h | --help )           echo "$usage"
                            exit
                            ;;
    * )                     echo "$usage"
                            exit 1
    esac
    shift
done


if [ "$cl" = "1" ] && [ "$dryrun" = "0" ];
then
    read -p "Running cluster jobs, are you vpn'd in to della (y/n)?" choice
    case "$choice" in
        y|Y ) echo "Running cluster jobs!";;
        n|N ) exit;;
        * ) echo "invalid";exit;;
    esac
fi

for f in $FILES
do
    if grep -q "functions" <<< "$f";
    then
    continue
    fi

    if grep -q "log_cluster" "$f";
    then
        if [ "$cl" = "0" ];
        then 
        echo "cluster job not run: $f"
        else
        cmd=$(grep "sub_cmd"  "$f" | cut -f 2 -d =) # sub args from script
            if [ "$dryrun" = "1" ];
            then
                echo "cluster cmd: sub $cmd"
            else
                sub $cmd
            fi
        fi
    else
        if [ "$dryrun" = "1" ];
        then
            echo "local: $f"
        else
            echo "local: $f"
            Rscript --vanilla "$f"
        fi
    fi
done
