#Example 17.1: use regression for prediction.
lion=read.csv("chap17e1LionNoses.csv")
plot(x=lion$proportionBlack,y=lion$ageInYears,xlab="Proportion black",ylab="Age (years)")
#Example 17.3: use regression for hypothesis testing.
diversity=read.csv("chap17e3PlantDiversityAndStability.csv")
plot(x=diversity$nSpecies,y=diversity$biomassStability,xlab="Species number",ylab="Biomass stability (mean/sd)")

#Run the regression.
lion.lm=lm(lion$ageInYears~lion$proportionBlack)
#Print parameter estimates.
coef(lion.lm)
#Add the best fit line to an existing sctterplot.
plot(x=lion$proportionBlack,y=lion$ageInYears,xlab="Proportion black",ylab="Age (years)")
abline(lion.lm)
#"Hypothesis" testing.
anova(lion.lm)
summary(lion.lm)

#Check residuals
hist(resid(lion.lm),xlab="Residual")
plot(lion$proportionBlack,resid(lion.lm),xlab="Proportion black",ylab="Residual")

#####Ex. 17.3: Log-transformation?
#First run the regression on the untransformed data.
plot(x=diversity$nSpecies,y=diversity$biomassStability,xlab="Species number",ylab="Stability")
diversity.lm=lm(diversity$biomassStability~diversity$nSpecies)
#Check the residuals.
hist(resid(diversity.lm),xlab="Residual")
qqnorm(resid(diversity.lm))
#Log-transform the response variable.
plot(x=diversity$nSpecies,y=log(diversity$biomassStability),xlab="Species number",ylab="ln(Stability)")
diversity.lm=lm(log(diversity$biomassStability)~diversity$nSpecies)
abline(diversity.lm)
hist(resid(diversity.lm),xlab="Residual")
qqnorm(resid(diversity.lm))
#Hypothesis testing after transformation.
anova(diversity.lm)
summary(diversity.lm)

#####Confidence interval and prediction interval.
#Plot the lion data.
plot(x=lion$proportionBlack,y=lion$ageInYears,xlab="Proportion black",ylab="Age (years)")
#This is necessary to make the variable names match up in lm() and predict().
attach(lion)
#Re-run the regression.
lion.lm=lm(ageInYears~proportionBlack)
#Create an array of points at which you will calculate/plot confidence and prediction intervals.
new.points=data.frame(proportionBlack=seq(0.1,0.8,by=0.01)) #
#The predict() function calculates the interval at each of the points in newdata.
confidence.interval=predict(lion.lm,newdata=new.points,interval="confidence")
head(confidence.interval) #The columns contain predicted values with lower and upper confidence intervals.
#Add the intervals to the scatterplot.
lines(new.points$proportionBlack,confidence.interval[,2],lty=2)
lines(new.points$proportionBlack,confidence.interval[,3],lty=2)
#Now calculate and plot prediction intervals.
prediction.interval=predict(lion.lm,newdata=new.points,interval="prediction")
lines(new.points$proportionBlack,prediction.interval[,2],lty=3)
lines(new.points$proportionBlack,prediction.interval[,3],lty=3)




