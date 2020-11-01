#####Regression through the origin.
data("cars") #This data set is included in the base R installation.
head(cars)
plot(cars$speed,cars$dist,xlab="Speed (mph)",ylab="Stopping distance (feet)")

#First try a simple linear regression.
cars.lm=lm(cars$dist~cars$speed)
summary(cars.lm)
abline(cars.lm)

#Then try forcing the regression through the origin.
cars.lm0=lm(cars$dist~cars$speed-1) #The -1 removes (i.e., sets =0) the first parameter, which is the intercept.
summary(cars.lm0) #The p-value is smaller because there is one more df.
abline(cars.lm0,col="red")

#Both are "significant", but neither is great.
plot(cars$speed,resid(cars.lm))
plot(cars$speed,resid(cars.lm0))
par(mfcol=c(2,1))
hist(resid(cars.lm))
hist(resid(cars.lm0))

#####Polynomial regression.
#Add a quadratic term, but keep the intercept=0.
speed.squared=cars$speed^2
cars.lm2=lm(cars$dist~cars$speed+speed.squared-1)
#cars.lm2=lm(cars$dist~cars$speed+I(cars$speed^2)-1) #Or, use I() to force lm to treat the term as a simple squared term.
summary(cars.lm2)
#Plot the fitted qudratic with the data.
speed.values=seq(0,25,by=0.1)
new.points=1.23903*speed.values+0.09014*speed.values^2 #Use the coefficients from the regression output.
plot(cars$speed,cars$dist,xlim=c(0,25),xlab="Speed (mph)",ylab="Stopping distance (feet)") #Extend the x-axis to zero so you can see the intercept=0.
lines(speed.values,new.points) #Add the regression line.

#####Polynomial (quadratic) regression to detect a hump-shaped relationship.
#Figure 17.8-2
plants=read.csv("chap17f8_2PondPlantsAndProductivity.csv")
head(plants)
plot(plants$productivity,plants$species,xlab="Productivity (g/15 days)",ylab="Number of plant species")
plants.lm=lm(plants$species~plants$productivity)
abline(plants.lm)
summary(plants.lm) #P-value>0.05 for the simple linear regression.
#Plot the residuals (peak in the middle).
plot(plants$productivity,resid(plants.lm))

#Try adding a quadratic term.
productivity2=plants$productivity^2
plants.lm2=lm(plants$species~plants$productivity+productivity2)
summary(plants.lm2) #Both linear and quadratic terms have p-value<0.05, and the Anova p-value<0.05.

#Plot the fitted quadratic with the data.
plot(plants$productivity,plants$species,xlab="Productivity (g/15 days)",ylab="Number of plant species")
productivity.values=seq(0,100,by=1)
species.values=1.7535477+0.2083549*productivity.values-0.0020407*productivity.values^2 #Use the coefficients from the regression output.
lines(productivity.values,species.values,col="red")

#####Nonlinear regression.
#Figure 17.8-1
iron=read.csv("chap17f8_1IronAndPhytoplanktonGrowth.csv")
head(iron)
plot(iron$ironConcentration,iron$phytoGrowthRate,xlab=expression(paste("Iron concentration (",mu," mol)")),ylab=expression(paste("Specific growth rate (",d^-1,")")))
#Fit the (nonlinear) Michaelis-Menten function by least-squares minimization. 
iron.nls=nls(iron$phytoGrowthRate~a*iron$ironConcentration/(b+iron$ironConcentration),data=iron,start=list(a=1,b=1))
summary(iron.nls)
#Add the fitted curve to the plot.
conc.values=seq(0,2.5,by=0.1)
growth.values=1.94164*conc.values/(0.07833+conc.values) #Use the parameter values from the regression.
lines(conc.values,growth.values)


