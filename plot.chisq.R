#Plot the Chi-squared distribution
n.categories=7 #For Example 8.1, the days of the week. Try changing this to a smaller number.
chisq.values=seq(0,20,by=0.01) #The range of values for which you want to plot the distribution.
prob.density=dchisq(x=chisq.values,df=n.categories-1) #Create an array of values of the Chi-squared distribution.
plot(chisq.values,prob.density,type="l",xlab=expression(chi^2),ylab="Probability density")


#########################################
#####Example 8.1
birthDay=read.csv("chap08e1DayOfBirth.csv")
#The next line just forces R to put the days in chronological order, rather than alphabetical order (the default).
birthDay$day=factor(birthDay$day,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
birthDayTable=table(birthDay$day)
birthDayTable
barplot(birthDayTable,xlab="Day of birth",ylab="Frequency")

#Run the Chi-squared test with H0: births are equally likely on each day of the week.
chisq.test(x=birthDayTable,p=c(1/7,1/7,1/7,1/7,1/7,1/7,1/7))

#Does the probability of birth on weekends differ from 2/7?
weekend=sum(birthDay$day=="Saturday")+sum(birthDay$day=="Sunday")
binom.test(x=weekend,n=length(birthDay$day),p=2/7)
#You could also use a Chi-squared test with only two categories (but the binomial test is better).
chisq.test(x=c(weekend,length(birthDay$day)-weekend),p=c(2/7,5/7))

#########################################
#####Practice problem #2
shad=read.csv("chap08q02ShadParasites.csv")

#####create table
shadTable=table(shad$numberOfParasites,dnn="Number of nematodes")
data.frame(shadTable) #Create data frame to display table vertically (as in book).
barplot(shadTable,xlab="Number of parasites",ylab="Frequency",main="Shad data")

#####Simulate random distribution of parasites among hosts, just to see what it might look like.
total.n.fish=length(shad$numberOfParasites)
total.n.parasites=sum(shad$numberOfParasites)
simulated.data=array(0,dim=total.n.fish) #Create an array to store the (simulated) number of parasites on each fish.
for(i in 1:total.n.parasites){#For each parasite, draw a random fish to parasitize.
  pick.a.fish=sample(1:total.n.fish,size=1)
  simulated.data[pick.a.fish]=simulated.data[pick.a.fish]+1 #Add the parasite to the selected fish.
}
#plot the simulated results
fishTable=table(simulated.data)
dev.new()
barplot(fishTable,xlab="Number of parasites",ylab="Frequency",main="Simulated data")

#####Poisson distribution
mean.parasites=mean(shad$numberOfParasites) #This is the only parameter in the Poisson distribution.

propns.0to5=dpois(x=seq(0,5,by=1),lambda=mean.parasites)
propn.6up=1-sum(propns.0to5)
expected.propns=c(propns.0to5,propn.6up)
expected.frequencies=expected.propns*total.n.fish
barplot(expected.frequencies,names.arg=c("0","1","2","3","4","5",">=6"),xlab="Number of parasites",ylab="Frequency",main="Expected from the Poisson distribution")

#####Test for deviation from Poisson
chisq.test(x=shadTable,p=expected.propns) #Note the warning: some bins have too few expected.

#***Above test is not valid because an additional degree of freedom is lost (mean estimated from sample).
pchisq(q=12.034,df=5,lower.tail=FALSE) #Lower df by 1.

#To deal with low frequencies in right tail, combine bins so that all have expected frequencies >5.
expected.propns.combined=c(expected.propns[1:3],sum(expected.propns[4:7]))
expected.frequencies.combined=expected.propns.combined*total.n.fish
shadTable.combined=c(shadTable[1:3],sum(shadTable[4:7]))
par(mfcol=c(2,1))
barplot(shadTable.combined,names.arg=c("0","1","2",">=3"),xlab="Number of parasites",ylab="Frequency",main="Observed")
barplot(expected.frequencies.combined,names.arg=c("0","1","2",">=3"),xlab="Number of parasites",ylab="Frequency",main="Expected")

chisq.test(x=shadTable.combined,p=expected.propns.combined)
pchisq(q=4.384,df=2,lower.tail=FALSE) #Lower df by 1.




