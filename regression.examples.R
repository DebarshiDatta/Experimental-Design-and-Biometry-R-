#####Practice problem 17.10
silene=read.csv("chap17q10HybridPollenSterility.csv")
head(silene)
plot(silene$geneticDistance,silene$proportionSterile,xlab="Genetic distance",ylab="Proportion sterile")
silene.lm=lm(silene$proportionSterile~silene$geneticDistance)
plot(silene$geneticDistance,resid(silene.lm))

plot(silene$geneticDistance,asin(sqrt(silene$proportionSterile)),xlab="Genetic distance",ylab="Proportion sterile")
silene.angular.lm=lm(asin(sqrt(silene$proportionSterile))~silene$geneticDistance)
plot(silene$geneticDistance,resid(silene.angular.lm))


#####Practice problem 17.14
hsd=read.csv("chap17q14HypoxanthineTimeOfDeath.csv")
head(hsd)
plot(hsd$hypoxanthine,hsd$hours,xlab="Hypoxanthine concentration",ylab="Hours since death")

#Run the linear regression.
hsd.lm=lm(hsd$hours~hsd$hypoxanthine)
abline(hsd.lm)
summary(hsd.lm)
#How to interpret the intercept?

#What to do about the extreme point? Try running the regression without the point.
#Which row number is it?
extreme.point=which(hsd$hours==max(hsd$hours),arr.ind=TRUE) #Or just look at the data.
hsd.subset=hsd[-extreme.point,]
plot(hsd.subset$hypoxanthine,hsd.subset$hours,xlab="Hypoxanthine concentration",ylab="Hours since death")
#Run the linear regression.
hsd.subset.lm=lm(hsd.subset$hours~hsd.subset$hypoxanthine)
abline(hsd.subset.lm)
summary(hsd.subset.lm)
#How much difference does the extreme point make?

#Re-plot the original regression and add confidence & prediction intervals.
plot(hsd$hypoxanthine,hsd$hours,xlab="Hypoxanthine concentration",ylab="Hours since death")
attach(hsd)
hsd.lm=lm(hours~hypoxanthine)
abline(hsd.lm)
#Create an array of points at which you will calculate/plot confidence and prediction intervals.
new.points=data.frame(hypoxanthine=seq(0,350,by=1))
#The predict() function calculates the interval at each of the points in newdata.
confidence.interval=predict(hsd.lm,newdata=new.points,interval="confidence")
#Add the intervals to the scatterplot.
lines(new.points$hypoxanthine,confidence.interval[,2],lty=2)
lines(new.points$hypoxanthine,confidence.interval[,3],lty=2)
#Now calculate and plot prediction intervals.
prediction.interval=predict(hsd.lm,newdata=new.points,interval="prediction")
lines(new.points$hypoxanthine,prediction.interval[,2],lty=3)
lines(new.points$hypoxanthine,prediction.interval[,3],lty=3)

#What is the prediction interval for a new case with hypoxanthine concentration =100?
#Look it up in prediction.interval, or
new.case=data.frame(hypoxanthine=100)
predict(hsd.lm,new.case,interval="prediction")







