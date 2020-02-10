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
# rsync pull down temp
rsync -rLvz --update --exclude 'candidate_matrix.gz' mrajeev@della.princeton.edu:~/MadaAccess/data/processed/ ~/Documents/Projects/MadaAccess/data/processed

