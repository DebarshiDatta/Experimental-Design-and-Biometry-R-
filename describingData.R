#Remember to change the working directory to wherever you saved the data files.
#Use the menu bar, or something like this:
#setwd("c:/erik/stats/biometryFall2018/ABD_all_data/chapter03")

#Read the data file.
snakeData=read.csv("chap03e1GlidingSnakes.csv")
head(snakeData)

#Take a look at the data.
hist(snakeData$undulationRateHz)

#Sample statistics.
mean(snakeData$undulationRate)
sd(snakeData$undulationRate)
var(snakeData$undulationRate)

