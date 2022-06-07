#!/bin/bash
#SBATCH --job-name=HISAT_alignment          # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=imir@ucsc.edu     # Where to send mail      
#SBATCH --nodes=1                     # Use one node
#SBATCH --ntasks=5                    # Run on a single CPU
#SBATCH --cpus-per-task=1             # Number of CPU cores per task
#SBATCH --mem=20G                     # Memory limit
#SBATCH --time=24:00:00               # Time limit hrs:min:sec
#SBATCH --output=serial_test_%j.out   # Standard output and error log
#SBATCH --error=serial_test_%j.err    # 

module load hisat

# Index
hisat2-build -f GRCh38.p13.genome.fa hisat_index

# align
hisat2 -q -x hisat_index -U Human_female_1_rep1.fastq -S HISATalignment_femaleAligned.out.sam

# align
hisat2 -q -x hisat_index -U Human_male_1_rep1.fastq -S HISATalignment_maleAligned.out.sam
