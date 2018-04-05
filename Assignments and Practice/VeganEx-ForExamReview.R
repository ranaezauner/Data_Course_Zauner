setwd("c:/Users/Ranae/Desktop/Data_Course/code_examples/Vegan_Example")
meta = read.delim("vegan_example_metadata.tsv",sep = "\t")
otu = read.delim("vegan_example_otu_table.tsv",sep = "\t", row.names = 1)

library(vegan)


names(otu)
otu$Tax_Name
tax = otu$Tax_Name
otu$Tax_Name = NULL

t_otu = as.data.frame(t(otu))

diversity(t_otu)

levels(meta$Ecosystem)

#check to make sure everything is in same order in both dataframes
identical(row.names(t_otu), as.character(meta$Sample_ID))



adonis(t_otu ~ meta$Ecosystem)

#This tells us there is a difference between ecosystems, ecosystems are important. Lets visualize. 

NMDS = metaMDS(t_otu) #Non Multi Deminsional Scaling
#The stable solution from this is found in the NMDS list under points. 
NMDS$points

MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]

plot(MDS1,MDS2)
library(ggplot2)

df = data.frame(MDS1 = MDS1, MDS2 = MDS2, Ecosystem = meta$Ecosystem)


ggplot(df, aes(x=MDS1,y=MDS2,color=Ecosystem)) +
  geom_point() +
  stat_ellipse()



library(plyr)
colors = as.character(mapvalues(meta$Ecosystem, from = c("Marine","Terrestrial"), to = c("Blue","Red")))

heatmap(as.matrix(t(t_otu)), ColSideColors = colors, col = gray.colors(100))
#This shows that some OTU that are abundant in marine are sometimes present in terrestrial. 
