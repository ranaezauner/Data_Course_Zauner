##EXAM#: PART 1

library(ShortRead)
library(tidyr)
library(stringr)
library(msa)
library(seqinr)
library(dada2)
setwd("c:/Users/Ranae/Desktop/Data_Course/Exam3/")
fq.files = dir(path = getwd(), full.names = TRUE, pattern = ".fastq")

filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")

dir.create(file.path(getwd(),"filtered"))

for(i in filt.files){
  fastqFilter(fn = i,fout = paste0(getwd(),"/filtered/",i,".filt"),
              truncLen = 100)
}
  
  
# Task 4: Save these trimmed fastq files as "Sample1.fastq.trim" and "Sample2.fastq.trim"  

setwd(file.path(paste0(getwd(),"/filtered")))
filtered = dir(path = getwd(), pattern = ".filt")
  


#EXAM 3: PART 2

library(vegan)

# Task 1: Import these files into R objects

otu_table = read.csv("c:/Users/Ranae/Desktop/Data_Course/Exam3/exam3_otu_table.csv", row.names = 1)
metadata = read.csv("c:/Users/Ranae/Desktop/Data_Course/Exam3/exam3_metadata.csv", row.names = 1)


# Task 2: Subset both objects to include only samples where SampleType is either "Soil" or "rhizosphere"

t_otu = as.data.frame(t(otu_table))
sub_meta <- subset(metadata, SampleType ==c("Soil","rhizosphere"))
sub_otu <- subset(t_otu, metadata$SampleType==c("Soil","rhizosphere"))

# Task 3: Perform a PermANOVA to determine whether there are significant differences in bacterial community composistion between these two sample types



adonis(t_otu ~ metadata$SampleType)

# Task 4: Determine which bacterial genus is the most abundant for each of those sample types as a whole









setwd("c:/Users/Ranae/Desktop/Data_Course/code_examples/Vegan_Example")
ex_meta = read.delim("vegan_example_metadata.tsv",sep = "\t")
ex_otu = read.delim("vegan_example_otu_table.tsv",sep = "\t", row.names = 1)
library(vegan)
