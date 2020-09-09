#!/bin/bash
usage=$(cat <<-END
    runit [-h] [-d] [-cl] [--dry]

    This utility will run all R scripts in a directory, if numbered
    then in the order they are numbered. Defaults to running all scripts
    in subdirectories of directory R:
        runit -d "R/*/*"

    accepted options are:
    -h, --help          show help message and exit
    -d, --dir           the directory path, quoted
    -cl, --cluster      whether to run the cluster scripts, defaults to not running them,
                        if passed, will ask you whether vpn-ed in to della
    --dry               dryrun, just prints the scripts and whether they are run
                        locally or on cluster
    -q, --quiet         whether to show messages, warnings, and errors from R; defaults to showing all
                        pass the arg to suppress these
    --printErrors       whether to print errors regardless of quiet argument
END
)

# Set up
quiet=0
dryrun=0
cl=0
FILES="R/*/*.R"
printerrors=0

while [ "$1" != "" ]; do
    case $1 in
    -d | --dir )           shift
                            FILES=$1
                            ;;
    --dry )                 dryrun=1
                            ;;
    -q | --quiet )          quiet=1
                            ;;
    --printErrors )         printerrors=1
                            ;;
    -cl | --cluster )       cl=1
                            ;;
    -h | --help )           echo "$usage"
                            exit
                            ;;
    * )                     echo "$usage"
                            exit 1
    esac
    shift
done

if [ "$quiet" = "0" ];
then
    out=/dev/tty
else
    if [ "$printerrors" = "0" ];
    then
        out=/dev/null
    else
        out=$(mktemp /tmp/log.XXXXXX)
        trap "rm -f $out" 0 3 15 6
    fi
fi

# Colors
Red='\033[0;31m'          # Red
Blue='\033[0;34m'         # Blue
NC='\033[0m' # No Color
BCyan='\033[1;36m' # Bold Cyan
BRed='\033[1;31m' # Bold Red

# Processing stdout & stderr


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
            if Rscript --vanilla "$f" &> $out;
            then
                echo  -e "${BCyan}$f completed.${NC}"
            else
                echo -e "${BRed}$f did not complete!${NC}"
                if [ "$printerrors" = "1" ];
                then
                    grep "Error" $out --color
                fi
            fi
        fi
    fi
done

echo "" # for trap even if you CTRL + C your way out of all the jobs
