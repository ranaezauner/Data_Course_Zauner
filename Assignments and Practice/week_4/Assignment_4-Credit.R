# Import...
mr <-read.csv("c:/Users/Ranae/Desktop/Data_Course/data/mushroom_growth.csv")

#Load Some Libraries...
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)
library(modelr)

#Convert some data into some factors...
glimpse(mr)
mr$Light = as.factor(mr$Light)
mr$Nitrogen = as.factor(mr$Nitrogen)
mr$Temperature = as.factor(mr$Temperature)
glimpse(mr)

mr_original = mr #a backup in case I mess something up

#Analyze some factorial data and make some meaningful plots... 
plot(mr)
plot(fitdist(mr$GrowthRate, distr = "norm"))
plot(fitdist(mr$GrowthRate, distr = "lnorm"))
plot(fitdist(mr$GrowthRate, distr = "logis"))
plot(fitdist(mr$GrowthRate, distr = "gamma"))
?fitdist


denscomp(fitdist(mr$GrowthRate, distr = "gamma"))

# To be honest, I am having a hard time figure out what exactly a gamma distribution is at this point. 
# Using the example given for gamma in the help file for fitdist...

data(mr) #This gae an error, not sure if I need it 
?data
GrowthRate <- mr$GrowthRate #Gives a vector, num [1:216]
GrowthRate_gamma <- fitdist(GrowthRate, "gamma") #Gives data, List of 17. Not sure what this is... 
summary(GrowthRate_gamma) #Not sure what shape or rate is, so this isn't making this process anymore clear to me.
plot(GrowthRate_gamma)  #These charts look pretty great. 
plot(GrowthRate_gamma, demp = TRUE)
plot(GrowthRate_gamma, histo = FALSE, demp = TRUE)
cdfcomp(GrowthRate_gamma, addlegend=FALSE)
denscomp(GrowthRate_gamma, addlegend=FALSE)
ppcomp(GrowthRate_gamma, addlegend=FALSE)
qqcomp(GrowthRate_gamma, addlegend=FALSE)

# so again, gamma distribution looks pretty good. Still not sure what is means. 
boxplot(mr$GrowthRate ~ mr$Species*mr$Nitrogen, col = c("Lightgrey","White"))
?boxplot
?ggplot

p1 = ggplot(mr, aes(x=GrowthRate, y=Nitrogen, col=Species)) +
  geom_point() +
  stat_smooth() + ggtitle("Growth Rate vs. Nitrogen")
p1

#It is safe to say I am still lost.... 


# Make a predictive model...
mod1 = aov(GrowthRate ~ Nitrogen*Light*Temperature*Humidity, data = mr)
summary(mod1)

plot(mod1)

tidy(mod1)

