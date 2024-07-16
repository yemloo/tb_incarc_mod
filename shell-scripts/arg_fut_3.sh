#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=1-00:00:00
#SBATCH --job-name=arg_fut_3
#SBATCH --output=arg_fut_3.out
#SBATCH --error=arg_fut_3.err
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=5GB

ml R/4.2.0

Rscript ~/TBprisons/scripts/future_projections_sherlock_240703.R 2001 3000 240617 Argentina