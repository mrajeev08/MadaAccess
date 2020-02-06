#!/bin/bash
# write one of these for each cluster script
# execute them in make file

ssh -T mrajeev@della <<HERE
    cd MadaAccess                                  # change to repo
    jid=\$(sbatch slurm/test | cut -c 21-)
    echo "Here's the job id: \$jid"
    jstat=\$(sacct -j "\$jid" -u mrajeev)
    echo "Here's the job stat: \$jstat"

    until grep -q "COMPLETED\|FAILED" <<< \$jstat    # if completed or failed
    do
        echo waiting                                # updating
        jstat=\$(sacct -j "\$jid" -u mrajeev)
        echo "Here's the job stat: \$jstat"
        sleep 10s                                   # time to sleep for (base it on how long the job should take)
    done
    logout
HERE

                                                   # execute rsync script locally here
