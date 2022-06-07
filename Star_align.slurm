#!/bin/bash
#SBATCH --job-name=STAR_alignment     # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=imir@ucsc.edu     # Where to send mail      
#SBATCH --nodes=1                     # Use one node
#SBATCH --ntasks=5                    # Run on a single CPU
#SBATCH --cpus-per-task=1             # Number of CPU cores per task
#SBATCH --mem=20G                     # Memory limit
#SBATCH --time=36:00:00               # Time limit hrs:min:sec
#SBATCH --output=serial_test_%j.out   # Standard output and error log
#SBATCH --error=serial_test_%j.err    # 

module load star

# Index
STAR --runMode genomeGenerate --runThreadN 10 --genomeDir genomeDir --genomeFastaFiles GRCh38.p13.genome.fa --sjdbGTFfile gencode.v37.annotation.gtf --sjdbOverhang 34 

# align
STAR --runThreadN 10 --genomeDir genomeDir --readFilesIn Human_female_1_rep1.fastq --outSAMtype SAM --twopassMode Basic --outFileNamePrefix STARalignment_female

# align
STAR --runThreadN 10 --genomeDir genomeDir --readFilesIn Human_male_1_rep1.fastq --outSAMtype SAM --twopassMode Basic --outFileNamePrefix STARalignment_male
