setwd("C:/Users/Ranae/Desktop/Data_Course/Data")
list.files()[1:5]
dataframe = read.csv("Fake_Grade_Data.csv")
names(dataframe)
DForiginal = dataframe
dataframe <- dataframe [,-1]
dataframe$NEW = rowSums(dataframe[,-1]) 
dataframe$Total = rowSums(dataframe[,-1]) 
dataframe <- dataframe [,-17]
dataframe$Total = rowSums(dataframe[,-1])
A = dataframe$Total > 699 #This gives a true/false list
A = dataframe[dataframe$Total > 699,] #this gives only the rows in which the total column is 700+


order(A$Total) #giving which row number is the first, which is the second, etc. so, row 6 is the least, row 4 is the greatest
A$Total

A[order(A$Total, decreasing = TRUE),]
order = A[order(A$Total, decreasing = TRUE),] 


sort(A$Total)


sample(dataframe$Total, 3) #gives a random sample of 3
