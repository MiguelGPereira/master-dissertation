#!/bin/bash
cd $PBS_O_WORKDIR
echo We are now in $PBS_O_WORKDIR, running an R script.
R --vanilla '--args grid' < core.R > job.log 2> job.log
echo Done