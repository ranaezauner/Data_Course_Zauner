df = read.csv("c:/Users/Ranae/Desktop/Data_Course/Data/BioLog_Plate_Data.csv")
levels(df$Sample.ID)
unique(df$Rep)
unique(df$Dilution)
levels(df$Substrate)


#Plot of D-Cellobiose, absorbance over time
sub1 = subset(df, Substrate == "D-Cellobiose")

#Time needs to be a column

Absorbance = c(df$Hr_24,df$Hr_48,df$Hr_144)

#tack this on df, repeated 3 times

df[,-c(6,7,8)] #gives dataframe minus 3 columns we don't want. 
?gather() #in tidyr library

Sub1_Long = gather(sub1, key = "Time", value = "Abs", c("Hr_24", "Hr_48", "Hr_144"))

#Name of new column (key = "time")
#value ="abs" is going to take all the values from the hour columns and place them in a new one called abs
#Tell it what columns do to this with





#Now we can plot absorbance over time. 
ggplot(Sub1_Long, aes(x=Time, y=Abs,col=Sample.ID)) +
  geom_point() +
  stat_smooth()
#Doesn't give line because of the format time is entered in. 


library(plyr)

revalue(Sub1_Long$Time, c("Hr_24"="24","Hr_48"="48", "Hr_144"="144"))

#So that didn't work (would have to save this somewhere and replace), try this: 

h24 = which(Sub1_Long$Time == "Hr_24")
h48 = which(Sub1_Long$Time == "Hr_48")
h144 = which(Sub1_Long$Time == "Hr_144")

Sub1_Long$Time[h24] = 24
Sub1_Long$Time[h48] = 48
Sub1_Long$Time[h144] = 144

Sub1_Long$Time = as.numeric(Sub1_Long$Time)

#A more simple version of above could look like this:

Sub1_Long$Time = as.numeric(mapvalues(Sub1_Long$Time, from = c("Hr_24","Hr_48", "Hr_144"),
          to = c(24,48,144)))


#Try plotting again:

ggplot(Sub1_Long, aes(x=Time, y=Abs,col=Sample.ID)) +
  geom_point() +
  stat_smooth()



#Do this for the entire df


df_long = gather(df, key = "Time", value = "Abs", c("Hr_24", "Hr_48", "Hr_144"))

df_long$Time = as.numeric(mapvalues(df_long$Time, from = c("Hr_24","Hr_48", "Hr_144"),
                                      to = c(24,48,144)))


df_long$Time = as.numeric(df_long$Time)

ggplot(df_long, aes(x=Time, y=Abs,col=Sample.ID)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(facets = ~Substrate) +
  labs(y = "Absorbance",
       title = "Absorbance over time per substrate") 

?ggplot


#Can we do this using a for loop

for(i in levels(df_long$Substrate)){
  sub = subset(df_long, Substrate == i)
  print(ggplot(sub, aes(x=Time, y=Abs,col=Sample.ID)) +
    geom_point() +
    stat_smooth() +
    ggtitle(i)) 
  }




#Steps
#Read in
#Change to long format
#Convert factors to numeric values
#save as new file (write.csv)


#so in bash, Rscript file.R
#That will run the rscript in the background any file that would be 
#The point of this is a script that will take ugly data and make a new clean file of it while preserving the original. 
# That's ALL that file would do so that you could share it with collaborators.
# If packages are used, include a line for install in case collaborator does not have that package.