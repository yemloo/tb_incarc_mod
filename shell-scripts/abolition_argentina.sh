#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=1-00:00:00
#SBATCH --job-name=ab_arg
#SBATCH --output=ab_arg.out
#SBATCH --error=ab_arg.err
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=5GB

ml R/4.2.0

Rscript ~/TBprisons/scripts/abolition_sherlock_240605.R 240617 Argentina
