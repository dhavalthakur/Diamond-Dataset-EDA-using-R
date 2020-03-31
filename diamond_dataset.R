"This is the diamond dataset R File
Note:- All the commands we used or the graphs we plotted are present in this file.

Author: Dhaval Thakur, Rushi Bhuva,Tejas Pandit"

library(tidyverse)
library(ggplot2)
library(Rmisc)
library(Hmisc)
library(GGally) #Using this new library to print a comprehensive graph for corelation
library(RColorBrewer)

dataset <- data("diamonds")
str(diamonds)
diamonds$color <- ordered(diamonds$color, levels = c("J", "I", "H", "G", "F", "E", "D"))
levels(diamonds$color)
diamonds$clarity <- ordered(diamonds$clarity,levels = c("I1", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2", "IF"))
qplot(x = price, data = diamonds)
#To get the correlation of the different variables so that we can decide from where to start the analysis
ggpairs(diamonds)
#Gives the summary 
summary(diamonds$cut)
diamonds %>%ggplot(aes(x=(price))) +  geom_histogram(stat="bin",binwidth= 500) + facet_wrap(~cut, scales = "free")
ggplot(data = diamonds, mapping = aes(x = x)) + geom_density() + geom_rug() + labs(title = 'Distribution of x(length)') 
ggplot(data = diamonds, mapping = aes(x = y)) + geom_density() + geom_rug() + labs(title = 'Distribution of y(width)')
ggplot(data = diamonds, mapping = aes(x = z)) + geom_density() + geom_rug() + labs(title = 'Distribution of z(depth)')
diamonds %>%ggplot(aes(x=cut,y=price, fill=cut)) + geom_boxplot() 

#Number of observation that has x or y or z as 0
dim(diamonds[diamonds$x == 0 | diamonds$y == 0 | diamonds$z == 0, ])[1]

#Interesting Observation on the diamonds Price 
ggplot(filter(diamonds), aes(x = price)) +
  geom_histogram(binwidth = 100, center = 0)

"Cut distribution for diamonds which shows that their distribution is similar for
for all the types of cuts present in the dataset"
diamonds %>%
  ggplot(aes(x=(price))) +
  geom_histogram(stat="bin",binwidth= 500) +
  facet_wrap(~cut, scales = "free")


#Plotting various graphs to see the relationship between different variables
diamonds %>%  ggplot(aes(x=carat,fill=color))+geom_density(alpha=0.2)+facet_grid(rows = vars(color))
diamonds %>% ggplot(aes(x=cut,y=price, fill=color)) + geom_boxplot()
diamonds %>% filter(color=='D') %>% ggplot(aes(x=carat,y=price, color=cut)) + geom_point()+geom_smooth()
diamonds%>% ggplot(aes(x=carat,y=price)) + geom_point()+facet_grid(rows = vars(color))
diamonds %>% ggplot(aes(x=carat,fill=color))+geom_density(alpha=0.2)+facet_grid(rows = vars(color))
diamonds %>% ggplot(aes(price,carat, col= color)) + geom_point()+geom_smooth()
diamonds %>% ggplot(aes(price,carat, col= clarity)) + geom_point()+geom_smooth()
diamonds %>% ggplot(aes(price,carat, col= cut)) + geom_point()+geom_smooth()
diamonds %>% ggplot(aes(x=cut,y=carat, color=cut)) + geom_boxplot()

qplot(x = color, y = price, data = diamonds,
      geom = 'boxplot') + coord_cartesian(ylim = c(0, 8000))