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

glimpse(mr)

mr_original = mr #a backup in case I mess something up

#Analyze some factorial data and make some meaningful plots... 
plot(mr)
plot(fitdist(mr$GrowthRate, distr = "norm"))
plot(fitdist(mr$GrowthRate, distr = "lnorm"))
plot(fitdist(mr$GrowthRate, distr = "logis"))
plot(fitdist(mr$GrowthRate, distr = "gamma"))
?fitdist


denscomp(fitdist(mr$GrowthRate, distr = "lnorm"))

plot(fitdist(log10(mr$GrowthRate), distr = "norm")) #Like our CO2 data, this looks pretty decent. 

# I think gamma worked better before because I converted everything to factors. 

mr$log10_GrowthRate = log10(mr$GrowthRate)

boxplot(mr$GrowthRate ~ mr$Nitrogen*mr$Humidity)
boxplot(mr$GrowthRate ~ mr$Nitrogen*mr$Light)
boxplot(mr$GrowthRate ~ mr$Nitrogen*mr$Temperature)

#Some Plots
Plot_1 = ggplot(mr, aes(x=Nitrogen, y=GrowthRate, col=Species)) +
  geom_point() +
  stat_smooth() + ggtitle("Growth Rate vs. Nitrogen")

Plot_1

Plot_2 = ggplot(mr, aes(x=Nitrogen, y=log10_GrowthRate, col=Species)) +
  geom_point() +
  stat_smooth() + ggtitle("Log 10 of Growth Rate vs. Nitrogen")

Plot_2

Plot_3 = ggplot(mr, aes(x=Light, y=log10_GrowthRate, col=Species)) +
  geom_point() +
  stat_smooth() + ggtitle("Log 10 of Growth Rate vs. Light")

Plot_3


#Models: ANOVA
mod1 = aov(log10_GrowthRate ~ Temperature*Nitrogen*Light*Humidity, data = mr)
summary(mod1)
plot(mod1)
tidy(mod1)

mod1b = aov(GrowthRate ~ Temperature*Nitrogen*Light*Humidity, data = mr)
summary(mod1b)
plot(mod1b)
tidy(mod1b)

mod2 = aov(GrowthRate ~ Nitrogen*Light*Humidity, data = mr)
summary(mod2)
plot(mod2)
tidy(mod2)

mod3 = aov(GrowthRate ~ Light*Humidity, data = mr)
summary(mod3)
plot(mod3)
tidy(mod3)



# check model predictions (mod 1)
mr = add_predictions(mr,model = mod1)

plot(mr$log10_GrowthRate, mr$pred)
abline(lm(mr$pred ~ mr$log10_GrowthRate))

mean(sum((mr$log10_Growth - mr$pred)^2))

TukeyHSD(mod1b)
warnings()


# check model predictions (mod 3)
mr = add_predictions(mr,model = mod3)

plot(mr$GrowthRate, mr$pred)
abline(lm(mr$pred ~ mr$GrowthRate))

mean(sum((mr$GrowthRate - mr$pred)^2))


TukeyHSD(mod1)
?TukeyHSD
