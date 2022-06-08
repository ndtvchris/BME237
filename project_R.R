hisat.ht.eb <- read.csv('hisat.htseq.ebseq.csv')
hisat.sub.eb <- read.csv('hisat.subread.ebseq.csv')
star.ht.eb <- read.csv('star.ht.ebseq.csv')
star.sub.eb <- read.csv('star.rsub.ebseq.csv')
top.ht.eb <- read.csv('tophat.ht.ebseq.csv')
top.sub.eb <- read.csv('tophat.sub.ebseq.csv')

print(num.ebseq(hisat.ht.eb))
# 138
print(num.ebseq(hisat.sub.eb))
# 190
print(num.ebseq(star.ht.eb))
# 174
print(num.ebseq(star.sub.eb))
# 193
print(num.ebseq(top.ht.eb))
# 161
print(num.ebseq(top.sub.eb))
#50

hisat.ht.ds <- read.csv('hisat.ht.results.csv')
hisat.ht.ds <- cbind(hisat.ht.ds$X, hisat.ht.ds$log2FoldChange, hisat.ht.ds$pvalue)
hisat.sub.ds <- read.csv('hisat.rsub.results.csv')
hisat.sub.ds <- cbind(hisat.sub.ds$X, hisat.sub.ds$log2FoldChange, hisat.sub.ds$pvalue)
star.ht.ds <- read.csv('star.ht.results.csv')
star.ht.ds <- cbind(star.ht.ds$X,star.ht.ds$log2FoldChange, star.ht.ds$pvalue)
star.sub.ds <- read.csv('star.rsub.results.csv')
star.sub.ds <- cbind(star.sub.ds$X,star.sub.ds$log2FoldChange, star.sub.ds$pvalue)
top.ht.ds <- read.csv('tophat.ht.results.csv')
top.ht.ds <- cbind(top.ht.ds$X,top.ht.ds$log2FoldChange, top.ht.ds$pvalue)
top.sub.ds <- read.csv('tophat.rsub.results.csv')
top.sub.ds <- cbind(top.sub.ds$X,top.sub.ds$log2FoldChange, top.sub.ds$pvalue)

print(ncol(hisat.ht.ds))

print(class(type.convert(hisat.ht.ds[1,2])))

num.deseq <- function(infile)
{
  sum <- 0
  for(row in 1:nrow(infile))
  {
    lfc <- type.convert(infile[row, 2])
    if(is.na(lfc))
    {
      sum <- sum + 0
    }
    else
    {
      if(abs(lfc) > 1)
      {
        sum <- sum + 1
      }
    }
  }
  return(sum)
}

print(num.deseq(hisat.ht.ds))
# 511
print(num.deseq(hisat.sub.ds))
# 556
print(num.deseq(star.ht.ds))
# 508
print(num.deseq(star.sub.ds))
# 526
print(num.deseq(top.ht.ds))
# 530
print(num.deseq(top.sub.ds))
#586


num.ebseq <- function(infile)
{
  sum <- 0
  for(row in 1:nrow(infile))
  {
    num <- type.convert(infile[row,'PPDE'])
    if(is.na(num))
    {
      sum <- sum + 0
    }
    else
    {
      if(abs(num) >= 0.95)
      {
        sum <- sum + 1
      }
    }
  }
  return(sum)
}

