#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=0-6:00:00
#SBATCH --job-name=arg_ps1
#SBATCH --output=arg_ps1.out
#SBATCH --error=argtest.err
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=1GB

ml R/4.2.0
Rscript ~/TBprisons/scripts/main_script_argentina_sherlock_240617.R 1

