#Example 16.1
birds=read.csv("chap16e1FlippingBird.csv")
head(birds)
#Create a scatterplot.
plot(birds$nVisitsNestling,birds$futureBehavior,xlab="Number of visits",ylab="Future aggressive behavior")
#Calculate the covariance.
cov(birds$nVisitsNestling,birds$futureBehavior)
#Calculate the correlation coefficient.
cor(birds$nVisitsNestling,birds$futureBehavior)

#Test H0: correlation coefficient=0.
cor.test(birds$nVisitsNestling,birds$futureBehavior)

#Check normality.
par(mfcol=c(2,1))
hist(birds$nVisitsNestling)
hist(birds$futureBehavior)
shapiro.test(birds$nVisitsNestling)
shapiro.test(birds$futureBehavior)

#A non-parametric alternative, the Spearman rank correlation coefficient.
cor.test(birds$nVisitsNestling,birds$futureBehavior,method="spearman")

