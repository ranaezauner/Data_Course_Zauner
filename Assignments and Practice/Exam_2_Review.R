# Review for Exam 2 

library(tidyr)


#Make our own dataset
x = rnorm(10, mean = 10)
y = rnorm(10,mean = 100)
z = rnorm(10)
obs = 1:10

df = data.frame(obs = obs,
           x=x,
           y=y,
           z=z)
#scenario, x,y,z are stocks, with a list of values at given points... change this to long format would be better

df_long = gather(df, key = "stock", value = "PriceChange", c("x","y","z"))


?spread
spread(df_long, Stock, PriceChange)

#does day or stock have effect on price change? 
aov1 = aov(PriceChange ~ stock*obs, data = df_long)
summary(aov1)

#No interaction because the way we set this up has mean set to zero. go back and change y = rnorm(10, mean = 10)
#after changing means, we can see that stock matters. 
x
y
z
#anova is running a little linear regression on factors in that model, looks at obs and compares with stocks,
#If we wanted to know which is bigger, use tukey

TukeyHSD(aov1)
#spits out several things, and compares against all things that are factors. so gives difference between stocks.
#This gives p-values as well to tell there are differences and whether those differences are significant enough to
#come from different sets (p-value)


library(dplyr)
# Pipes come from this library 
#We can Mutate is all about creating a new column. 
df %>% mutate(total = x+y+z, logx = log10(x))

apply(df[,2:4],1,min)
#this gives min of rows for columns 2 through 4. More info in help file. 

?select
select(df, c(x,y))
df %>% select(c(x,y)) #Does exactly the same as above. 

df %>% filter(x>9.5&x<10.5)
# select is picking columns, filter is picking rows

?group_by
df$group = c(rep("A",5), rep("B",5))

df %>% group_by(group) %>%
  summarise(meanx = mean(x))
#this could be useful for the utah lake data. 

df_long %>% group_by(stock) %>%
  summarise(mean = mean(PriceChange), 
            sum = sum(PriceChange), 
            min = min(PriceChange),
            Nonsense = n(),
            StDev = sd(PriceChange))


#Comparing models 
Iris = read.csv("c:/Users/Ranae/Desktop/Data_Course/Data/iris.csv")

plot(Iris)

mod1 = aov(Petal.Length ~ Petal.Width*Species, data = Iris)
mod2 = aov(Petal.Length ~ Petal.Width+Species, data = Iris)

summary(mod1)
summary(mod2)

#We have two models, are they statistically different from each other? 
anova(mod1,mod2)
#This shows us that these are different, and that they do predict the outcome differently, Does not say which is best. 
# to tell, we could plot them, we could also plot predictions... 

library(modelr)
#wants data, then model, then variable
df2 = add_predictions(Iris,mod1,var = "mod1") #Spits out a column of predictions based on mod1. 
df2 = add_predictions(df2,mod2,var = "mod2")

#Now we can plot our predictions
library(ggplot2)
p1 = ggplot(df2) +
  geom_point(aes(x=Petal.Width, y=Petal.Length,color=Species)) +
  geom_smooth(aes(x=Petal.Width, y=mod1,col=Species),method="lm") +
  geom_smooth(aes(x=Petal.Width, y=mod2,col=Species),method="lm",linetype=2)

#Still difficult to tell, so 

sqrt(mean((df2$mod1 - df2$Petal.Length)^2)) #squares the sums of distrance from point to line, then gives mean
sqrt(mean((df2$mod2 - df2$Petal.Length)^2)) 



df2[(df2$Petal.Length <5.3),] # <-- Keep anything less than 5.3, could also do this with mean:
df2[(df2$Petal.Length < mean(df2$Petal.Length)),]

#
##
###
#### For the exam:
###### wide data set convert to long
###### build models
###### see which is better
###### make some plots
###### anova
####
###
##
#

# eclipse draws an ellipse with a 95% confidence level
p1 +
  stat_ellipse(aes(x=Petal.Width, y=Petal.Length,color=Species))
#geom_violin is also useful (did this with my assign. 5.)
ggplot(df2, aes(x=Species,y=Petal.Length,fill=Species)) +
  geom_violin()
