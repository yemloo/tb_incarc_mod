# loop over all FASTA files in the directory, print the filename
# (so we have some visual progress indicator), then submit the
# gzip jobs to SLURM
#
for FILE in ~/TBprisons/scripts/argentina_240617/set1/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done

sleep 8000

for FILE in ~/TBprisons/scripts/argentina_240617/set2/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done

sleep 8000

for FILE in ~/TBprisons/scripts/argentina_240617/set3/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done

sleep 8000

for FILE in ~/TBprisons/scripts/argentina_240617/set4/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done

sleep 8000

for FILE in ~/TBprisons/scripts/argentina_240617/set5/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done

sleep 8000

for FILE in ~/TBprisons/scripts/argentina_240617/set6/arg_ps*.sh; do
echo ${FILE}
sbatch ${FILE}
sleep 2 # pause to be kind to the scheduler
done
