############ YOUR HOMEWORK ASSIGNMENT ##############
setwd("C:/Users/Ranae/Desktop/Data_Course/data/")
dat = read.csv("thatch_ant.csv")
a = which(dat$Headwidth == "41mm")
dat$Headwidth[a] = "41.000"
dat$Headwidth = as.numeric(dat$Headwidth)
which(dat$Headwidth == NA)
bad = which(dat$Headwidth == 1)
dat$Headwidth[bad] = NA
dat2 = na.omit(dat)


# 1.  Make a scatterplot of headwidth vs mass. See if you can get the points to be colored by "Colony"

dat2$Colony <- factor(dat2$Colony)
class(dat2$Colony)
levels(dat2$Colony)
plot(dat2$Headwidth, dat2$Mass, main = "Thatch Ants: Headwidth vs Mass", xlab = "Headwidth (mm)", ylab = "Mass (unit)", col = dat2$Colony, cex =0.8, pch = 20)
legend("bottomright", legend = levels(dat2$Colony), title = "Colony", col = 1:11, cex = 0.8, pch = 20)


# 2.  Write the code to save it (with meaningful labels) as a jpeg file
jpeg(filename = "C:/Users/Ranae/Desktop/Data_Course_Zauner/Thatch-Ant.jpeg") #opens window and any compands fed go into that window

plot(dat2$Headwidth, dat2$Mass, main = "Thatch Ants: Headwidth vs Mass", xlab = "Headwidth (mm)", ylab = "Mass (unit)", col = dat2$Colony, cex =0.8, pch = 20)
legend("bottomright", legend = levels(dat2$Colony), title = "Colony", col = 1:11, cex = 0.8, pch = 20)

dev.off() 

# 3.  Subset the thatch ant data set to only include ants from colony 1 and colony 2

dat2subset = subset(dat2, Colony == 1 | Colony == 2)

# 4.  Write code to save this new subset as a .csv file

write.csv(dat2subset, "C:/Users/Ranae/Desktop/Data_Course_Zauner/Thatch_Ant_Col1_2.csv")

# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas