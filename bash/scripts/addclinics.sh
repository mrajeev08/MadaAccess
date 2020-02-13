#!/bin/bash
# write one of these for each cluster script
# execute them in make file

ssh -T mrajeev@della <<HERE
    cd MadaAccess                                  # change to repo
    jid=\$(sbatch bash/slurm/addclinics | cut -c 21-)
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

# rsync pull down outputs
rsync -rLvz --update mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ ~/Documents/Projects/MadaAccess/output

# also session info
rsync -rLvz --update mrajeev@della.princeton.edu:~/MadaAccess/sessionInfo.csv ~/Documents/Projects/MadaAccess/sessionInfo.csv

