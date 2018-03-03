rm(list = ls())
#Started at 1:17 pm
# Load the libraries that you use here:
library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(tidyr)


############# Part 1 - Preparing wide data ################## ---------------- (30 points possible)

# read in salaries.csv
# This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university tier

df_sal = read.csv("c:/Users/Ranae/Desktop/Data_Course/exam2/salaries.csv")

# convert to usable format so we can look at salaries as a dependent variable (10 points)

df_sal_long = gather(df_sal, key = "Rank", value = "Salary", c("AssistProf", "AssocProf",
                                                    "FullProf"))
glimpse(df_sal_long)

df_sal_long$Rank = as.factor(df_sal_long$Rank)


# create boxplot of salary by University Tier, colored by Faculty Rank (10 points)
# x-axis = Tier
# y-axis = Salary
# Boxplot fill color = Rank
# Title = "Faculty Salaries - 1995"

ggplot(df_sal_long, aes(x=Tier, y=Salary,col=Rank)) +
  geom_boxplot() +
  labs(title = "Faculty Salaries - 1995") +
  theme_bw()

# export this boxplot to a file in your personal repository named "LASTNAME_exam2_plot1.jpeg" (10 points)
jpeg(filename = "C:/Users/Ranae/Desktop/Data_Course_Zauner/Exam 2/Zauner_exam2_plot1.jpeg")

ggplot(df_sal_long, aes(x=Tier, y=Salary,col=Rank)) +
  geom_boxplot() +
  labs(title = "Faculty Salaries - 1995") +
  theme_bw()

dev.off()

################# PART 2 ################### ------------ (70 points possible)

# read in atmosphere.csv
# this data frame has microbial diversity values over time found in atmospheric observation station air filters
# sampling date and two environmental variables [CO2] and [Aerosols] are reported for each measurement
# "Diversity" is the dependent variable

df_at = read.csv("c:/Users/Ranae/Desktop/Data_Course/exam2/atmosphere.csv")

glimpse(df_at)

# First, check whether your response variable is normally distributed (5 points)
plot(fitdist(df_at$Diversity, "norm"))

# Next, convert "Year" to a factor...just because (5 points)
df_at$Year = as.factor(df_at$Year)

# Create a simple ANOVA model with "Year" as the only explanatory variable (5 points)
mod1 = aov(Diversity ~ Year, data = df_at)
summary(mod1)

# Now, create an ANOVA model that incorporates "Year", "Aerosol_Density", and their interaction (5 points)
mod2 = aov(Diversity ~ Year*Aerosol_Density, data = df_at)
summary(mod2)

# Compare the two models mean-squared difference method to see which is better at making predictions 
# (20 points)
anova(mod1,mod2)

df_at = add_predictions(df_at,mod1,var = "mod1")  
df_at = add_predictions(df_at,mod2,var = "mod2")

ggplot(df_at) +
  geom_point(aes(x=Aerosol_Density, y=Diversity,color=Year)) +
  geom_smooth(aes(x=Aerosol_Density, y=mod1,col=Year),method="lm") +
  geom_smooth(aes(x=Aerosol_Density, y=mod2,col=Year),method="lm",linetype=2)


sqrt(mean((df_at$mod1 - df_at$Diversity)^2)) 
sqrt(mean((df_at$mod2 - df_at$Diversity)^2)) #Clearly the better model


# Export the summary ANOVA table of the better model to a text file in your repository named:
# "LASTNAME_exam2_table1.txt" (10 points)

summary(mod2)
capture.output(summary(mod2),file="c:/Users/Ranae/Desktop/Data_Course_Zauner/Exam 2/Zauner_exam2_table1.txt")

# use this model to predict what diversity should be for the following hypothetical conditions:
# note: only include the conditions that are part of your chosen model! (10 points)

# Year = 2007
# Quarter = "Q4"
# Month = August
# Mday = 10
# BarcodeSequence = "CTCTCTATCAGTGAGT"
# Aerosol_Density = 1000,
# CO2_Concentration = 384

predict(mod2, data.frame(Year = "2007", Aerosol_Density = 1000))

# Now, make a pretty plot to the following specifications:
# x-axis = Day
# y-axis = Aerosol_Density
# point transparency based on values of "Diversity"
# Title: "Decadal Aerosol Density"
# Subtitle: "More aerosols contribute to greater microbial diversity in the atmosphere"

ggplot(df_at, aes(x=Day, y=Aerosol_Density,col=Diversity)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Decadal Aerosol Density") +
  labs(subtitle = "More aerosols contribute to greater microbial diversity in the atmosphere",
       y= "Aerosol Density") +
  theme_bw()


# Save this plot in your repository as "LASTNAME_exam2_plot2.jpeg" (10 points)

jpeg(filename="c:/Users/Ranae/Desktop/Data_Course_Zauner/Exam 2/Zauner_plot2.jpeg")

ggplot(df_at, aes(x=Day, y=Aerosol_Density,col=Diversity)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Decadal Aerosol Density") +
  labs(subtitle = "More aerosols contribute to greater microbial diversity in the atmosphere",
       y= "Aerosol Density") +
  theme_bw()

dev.off()

#### When you are all finished, push the files, including this R script, onto your GitHub repo
#### I will look at your script and look for the three properly named files that you generated

#Finished at 2:42 pm
