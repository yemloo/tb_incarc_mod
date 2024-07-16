#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=1-00:00:00
#SBATCH --job-name=arg_fut_1
#SBATCH --output=arg_fut_1.out
#SBATCH --error=arg_fut_1.err
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=5GB

ml R/4.2.0

Rscript ~/TBprisons/scripts/future_projections_sherlock_240703.R 1 1000 240617 Argentina