setwd("C:/Users/Ranae/Desktop/Data_Course/Exam_1")
DNA = read.csv("C:/Users/Ranae/Desktop/Data_Course/Exam_1/DNA_Conc_by_Extraction_Date.csv.gz")
str(DNA)
?str


#I.
#Once you get the file loaded into an R object as a data frame, feel free to do some exploratory visualizations or summaries
#to get a feel for the data if you like. Your first task, though, is to create separate histograms of the DNA concentrations for 
#Katy and Ben. Make sure to add nice labels to these (x-axis and main title).

#Histogram for Katy
hist(DNA$DNA_Concentration_Katy, main="Katy's DNA Concentrations", xlab="DNA Concentration", col="lightgray", las = 1, breaks = 5)

#Histogram for Ben
hist(DNA$DNA_Concentration_Ben, main="Ben's DNA Concentrations", xlab="DNA Concentration", col="lightgreen", las = 1, breaks = 5)


#II. 	
#Your second task is to look at DNA concentrations from the different extraction years. 
#One way to do this in a single figure for each student is demonstrated in those two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
#Open those files in some image viewing program and take a look. I'd like you to re-create these exactly, including the labels.
#This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.

DNA$Year_Collected <- factor(DNA$Year_Collected) #Convert Year_Collected column to factor

#Katy's Plot
plot(DNA$Year_Collected,DNA$DNA_Concentration_Katy, main = "Katy's Extractions", xlab = "Year", ylab = "DNA Concentration")

#Ben's Plot
plot(DNA$Year_Collected,DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab = "Year", ylab = "DNA Concentration")



# III.
#Once you have your code for creating the figures correctly, you need to save those two images in your PERSONAL github repository. 
#Name them similarly to how I named mine.Make sure your code is saving the files. Don't do it manually with the mouse!

#Katy's Plot (Zauner_Plot1.jpeg)
jpeg(filename = "C:/Users/Ranae/Desktop/Data_Course_Zauner/Exam_1/Zauner_Plot1.jpeg") 
plot(DNA$Year_Collected,DNA$DNA_Concentration_Katy, main = "Katy's Extractions", xlab = "Year", ylab = "DNA Concentration")
dev.off()

#Ben's Plot (Zauner_Plot2.jpeg)
jpeg(filename = "C:/Users/Ranae/Desktop/Data_Course_Zauner/Exam_1/Zauner_Plot2.jpeg")
plot(DNA$Year_Collected,DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab = "Year", ylab = "DNA Concentration")
dev.off()


#IV.
#Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... with a plot or with summary stats or both.
#It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, right? With some samples, he only 
#had a marginal improvement over Katy.With other samples, he had a relatively massive improvement over her.
#Your task here is to write some code that tells us in which extraction YEAR, was Ben's performance the lowest relative to Katy's performance?

summary(DNA)
DNA$DNA_Concentration_Ben > DNA$DNA_Concentration_Katy

levels(DNA$Year_Collected)


library(plyr)

Ben_Mean_Year <- print(tapply(X=DNA$DNA_Concentration_Ben, INDEX=list(DNA$Year_Collected), FUN=mean , na.rm = TRUE))
Katy_Mean_Year <- print(tapply(X=DNA$DNA_Concentration_Katy, INDEX=list(DNA$Year_Collected), FUN=mean , na.rm = TRUE))


Ben_Mean_Year > Katy_Mean_Year

Ben_Mean_Year <- data.frame(tapply(X=DNA$DNA_Concentration_Ben, INDEX=list(DNA$Year_Collected), FUN=mean , na.rm = TRUE))
Katy_Mean_Year <- data.frame(tapply(X=DNA$DNA_Concentration_Katy, INDEX=list(DNA$Year_Collected), FUN=mean , na.rm = TRUE))

Ben_Mean_Year - Katy_Mean_Year


#V.
#For this final problem, let's just look at Ben's DNA concentration values. I think Katy messed up her PCRs, and at any rate, we can't use them for
#sequencing.Besides, our original purpose for this experiment was to see if DNA extractions sitting in a freezer degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) that has one column containing the years that DNA extractions 
#were made, and another column that contains the AVERAGE of the values within that year.  Just to be clear, this data frame should have 
#only 12 rows (one for each year). Once you have this new data frame of averages by year, write some code that shows which extraction year has 
#the highest average DNA concentration (and what that concentration is)

levels(DNA$Year_Collected)
Ben_Mean_Year <- data.frame(tapply(X=DNA$DNA_Concentration_Ben, INDEX=list(DNA$Year_Collected), FUN=mean , na.rm = TRUE))
max(Ben_Mean_Year)


#VI.
#Upload the following to your github web page:
#  1. Boxplot of DNA concentration values by year for Katy
#  2. Boxplot of DNA concentration values by year for Ben
#  3. Your .csv file of a data frame with YEAR as column 1 and Ben_Average_Conc as column 2 (named "Ben_average_by_year.csv")
#  4. Your complete R script file, saved as LASTNAME_Skills_Test_1.R

