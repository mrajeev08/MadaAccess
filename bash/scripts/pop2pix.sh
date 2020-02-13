#!/bin/bash
# write one of these for each cluster script
# execute them in make file

ssh -T mrajeev@della <<HERE
    cd MadaAccess                                  # change to repo
    jid=\$(sbatch bash/slurm/pop2pix | cut -c 21-)
    echo "Here's the job id: \$jid"
    jstat=\$(sacct -j "\$jid" -u mrajeev)
    echo "Here's the job status: \$jstat"

    until grep -q "COMPLETED\|FAILED" <<< \$jstat    # if completed or failed
    do
        sleep 1m                                   # time to sleep for (base it on how long the job should take)
        echo "waiting"                                # updating
        jstat=\$(sacct -j "\$jid" -u mrajeev)
        echo "Here's the job status: \$jstat"
    done
    logout
HERE

sleep 1m    # sleep again as sometimes takes a while to write output

# rsync pull down proccesed raster
rsync -rLvz --update --exclude 'candidate_matrix.gz' mrajeev@della.princeton.edu:~/MadaAccess/data/processed/ ~/Documents/Projects/MadaAccess/data/processed

# also session info
rsync -rLvz --update mrajeev@della.princeton.edu:~/MadaAccess/sessionInfo.csv ~/Documents/Projects/MadaAccess/sessionInfo.csv

# serial (non-parallelized)
if grep 'serial' <<< $1
then
Rscript ./R/03_bitemodels/01_run_bitemods.R serial
fi

# locally parallelized with doParallel
if grep 'local' <<< $1
then
Rscript ./R/03_bitemodels/01_run_bitemods.R local
fi

# Running on cluster
if grep 'remote' <<< $1
then
ssh -T mrajeev@della <<HERE
cd MadaAccess                                  # change to repo

# use utility script to write out slurm script
# -t time, -n nodes, -mem memory in bytes, -sp script path, -jn job name
# add -sn for single node
# also putting in trailing # of nodes!
slr2 -t 12 -n 20 -sp "./R/03_bitemodels/01_run_bitemods.R remote" -jn "bitemods"

jid=\$(sbatch bitemods.slurm | cut -c 21-)
echo "Here's the job id: \$jid"
jstat=\$(sacct -j "\$jid" -u mrajeev)
echo "Here's the job stat: \$jstat"

until grep -q "COMPLETED\|FAILED" <<< \$jstat    # if completed or failed
do
echo waiting                                # updating
jstat=\$(sacct -j "\$jid" -u mrajeev)
echo "Here's the job stat: \$jstat"
sleep 1m                                   # time to sleep for (base it on how long the job should take)
done
logout
HERE # here doc cannot be indented!
# execute rsync script locally
rsync -rLvz --update --exclude 'candidate_matrix.gz' mrajeev@della.princeton.edu:~/MadaAccess/output/mods/ ~/Documents/Projects/MadaAccess/output/mods
# also session info
rsync -rLvz --update mrajeev@della.princeton.edu:~/MadaAccess/sessionInfo.csv ~/Documents/Projects/MadaAccess/sessionInfo.csv
fi

