###Ex. 8.4
worldCup=read.csv("chap08q05WorldCup.csv")
head(worldCup)

woldCup.table=table(worldCup,dnn="Score")
#Display the table vertically.
data.frame(woldCup.table) #***Note that the scores skip from 5 to 8.
#Create a barplot.
barplot(woldCup.table,names.arg=c("0","1","2","3","4","5",">5"),xlab="Score",ylab="Frequency")

#Calculate the mean, which you will need to parameterize the (null) Poisson distribution
meanScore=mean(worldCup$score)
meanScore

#Calculate Poisson probabilities
pScore=dpois(x=0:5,lambda=meanScore)
#Tack on P(score>5)=1-P(score<=5)
pScore=c(pScore,1-sum(pScore))

#Use the Poisson probabilities to calculate the expected frequencies:
###Ei=Pi*samplesize
expected.frequencies=pScore*length(worldCup$score)
#Create a barplot for the expected frequencies.
dev.new()
barplot(expected.frequencies,names.arg=c("0","1","2","3","4","5",">5"),xlab="Score",ylab="Frequency")

#To be fancy, put observed and expected frequencies in a single barplot with color-coded bars.
observed.and.expected=matrix(nrow=2,ncol=7)
observed.and.expected[1,]=as.vector(woldCup.table)
observed.and.expected[2,]=expected.frequencies
barplot(observed.and.expected,beside=TRUE,legend.text=c("Observed","Expected"),names.arg=c("0","1","2","3","4","5",">5"),xlab="Score",ylab="Frequency")

###Check assumptions for Chi-squared test: low expected frequencies for high scores.
expected.frequencies #Expected frequencies for 4,5,>5 are low...
#So we should lump together 4,5 and >5
observed.frequencies=c(observed.and.expected[1,1:4],sum(observed.and.expected[1,5:7]))
expected.proportions=c(pScore[1:4],sum(pScore[5:7]))

#Run chisq.test to find the value of Chi-squared
#This will give a warning, because the expected frequency for >=4 goals is 4.99, but let's ignore it for now. 
#***If we reject the null, we can go back and try the test with >=3 combined, to see what happens.
chisq.test(x=observed.frequencies,p=expected.proportions)
#Use the value of Chi-squared=0.42317 to calculate the p-value with one less degree of freedom.
pchisq(q=0.42317,df=3,lower.tail=FALSE)

###Another way to get a quick/rough idea of the deviation is to check the ratio of variance/mean.
varScore=var(worldCup$score) #If scores are distributed at random, the variance will be approximately equal to the mean.
varScore
varScore/meanScore





