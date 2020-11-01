#Tell R where to look for the data files. Or use the menu bar "File/Change dir".
#setwd("c:/erik/stats/biometryFall2018/ABD_all_data/chapter02")

#One categorical variable: frequencies in a barplot
head(tigerData) #Rows are individuals (not as tabulated in the book)
tigerTable=sort(table(tigerData$activity), decreasing = TRUE)
tigerTable
barplot(tigerTable, ylab = "Frequency")
barplot(tigerTable, ylab = "Frequency",las=2)

#One numerical variable: frequencies in a histogram.
birdAbundanceData=read.csv("chap02e2bDesertBirdAbundance.csv")
head(birdAbundanceData)
hist(birdAbundanceData$abundance)
hist(birdAbundanceData$abundance,xlab="Abundance",main="Bird species in Organ Pipe Cactus N.P.")

###Histogram bin size: Salmon size data
salmonSizeData=read.csv("chap02f2_5SalmonBodySize.csv")
head(salmonSizeData)
par(mfcol=c(3,1))
hist(salmonSizeData$massKg, breaks = seq(1,4,by=0.1),xlab="Body mass (kg)",main="")
hist(salmonSizeData$massKg, breaks = seq(1,4,by=0.3),xlab="Body mass (kg)",main="")
hist(salmonSizeData$massKg, breaks = seq(1,4,by=0.5),xlab="Body mass (kg)",main="")

#Two categorical variables: contingency table, grouped barplot, mosaic plot.
birdMalariaData=read.csv("chap02e3aBirdMalaria.csv")
head(birdMalariaData)
birdMalariaTable=table(birdMalariaData$response, birdMalariaData$treatment)
birdMalariaTable
barplot(as.matrix(birdMalariaTable), beside = TRUE,legend=TRUE,xlab="Treatment",ylab="Frequency")
mosaicplot(t(birdMalariaTable),sub = "Treatment", ylab = "Relative frequency",main = "",col = c("red", "yellow"))

#Two numerical variables: scatterplot.
guppyFatherSonData=read.csv("chap02e3bGuppyFatherSonAttractiveness.csv")
head(guppyFatherSonData)
plot(guppyFatherSonData$fatherOrnamentation,guppyFatherSonData$sonAttractiveness,xlab="Father's ornamentation",ylab="Son's attractiveness")

#Numerical vs. categorical variable: stripchart, boxplot.
hemoglobinData=read.csv("chap02e3cHumanHemoglobinElevation.csv")
head(hemoglobinData)
stripchart(hemoglobinData$hemoglobin ~ hemoglobinData$population, method = "jitter",vertical = TRUE,ylab="Hemoglobin concentration (g/dL)")
boxplot(hemoglobinData$hemoglobin ~ hemoglobinData$population,ylab="Hemoglobin concentration (g/dL)")

