#!/bin/bash
#SBATCH --job-name=TopHat_alignment          # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=imir@ucsc.edu     # Where to send mail      
#SBATCH --nodes=2                     # Use one node
#SBATCH --ntasks=5                    # Run on a single CPU
#SBATCH --cpus-per-task=1             # Number of CPU cores per task
#SBATCH --mem=20G                     # Memory limit
#SBATCH --time=50:00:00               # Time limit hrs:min:sec
#SBATCH --output=serial_test_%j.out   # Standard output and error log
#SBATCH --error=serial_test_%j.err    # 

module load bowtie
module load python-2.7.15
module load samtools

# Index
bowtie2-build -f GRCh38.p13.genome.fa bowtie_index

# align
/hb/home/imir/BME237/finalProject/alignments/tophat-2.1.1.Linux_x86_64/tophat2 -o TOPHATalignment_femaleAligned bowtie_index Human_female_1_rep1.fastq
# align
/hb/home/imir/BME237/finalProject/alignments/tophat-2.1.1.Linux_x86_64/tophat2 -o TOPHATalignment_maleAligned bowtie_index Human_male_1_rep1.fastq
