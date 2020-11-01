#Plot the normal distribution, with some mean and sd.
popn.mean=8
popn.sd=2
variable=seq(0,16,by=0.001)
normal.prob.density=dnorm(x=variable,mean=popn.mean,sd=popn.sd)
plot(variable,normal.prob.density,type="l",xlab="X",ylab="Probaility density, f(X)")

#Take a large random sample from a normally distributed population, and plot a histogram.
sample.size=1000000
large.sample=rnorm(n=sample.size,mean=popn.mean,sd=popn.sd)
dev.new()
hist(large.sample,xlab="X",ylab="Frequency")
#Try reducing the sample size...

####################################################
#Standard normal distribution (default mean=0, sd=1 in R)
z.values=seq(-4,4,by=0.001)
z.prob.density=dnorm(z.values)
plot(z.values,z.prob.density,type="l",xlab="Z",ylab="Probability density, f(Z)")

#Find P(Y<5)=P(Z<-1.5)
pnorm(q=5,mean=8,sd=2,lower.tail=TRUE)
#or...
pnorm(q=-1.5,mean=0,sd=1,lower.tail=TRUE)

####################################################
#Distribution of sample means
par(mfcol=c(2,1))
#Plot the distribution of individuals again.
plot(variable,normal.prob.density,type="l",xlab="X",ylab="Probaility density, f(X)")
#Take a large number of samples from this population, and draw a histogram of their means.
n.samples=10000
sample.size=10
sample.means=array(0,dim=n.samples)
for(i in 1:n.samples){
  sample.values=rnorm(n=sample.size,mean=popn.mean,sd=popn.sd)
  sample.means[i]=mean(sample.values)
}
hist(sample.means,breaks=seq(0,16,by=0.1),xlab=expression(bar(Y)),ylab="Frequency",main="")

###What is P(sample mean <=5) if n=10?
#Calculate standard error of the mean.
SE=popn.sd/sqrt(sample.size)
#Then caculate Z for standard normal distribution.
z=(5-popn.mean)/SE
pnorm(q=z,mean=0,sd=1,lower.tail=TRUE)
#...and compare this to P(X<=5 for one individual)

####################################################
#If the population distribution is not normal...
#Ex. 10.6, age at death in Switzerland in 1918.

age.at.death=read.csv("chap10e6AgesAtDeathSpanishFlu1918.csv")
#Plot the population distribution.
par(mfcol=c(2,1))
hist(age.at.death$age,breaks=seq(0,100,by=1),xlab="Age at death",ylab="Frequency",main="Distribution of individuals") #draw a histogram for all individuals in the data set

n.samples=10000 #replicate the sampling process
sample.size=5
store.means=array(0,dim=n.samples) #create an array to store the mean of each sample
for(i in 1:n.samples){
  draw=sample(x=age.at.death$age,size=sample.size,replace=TRUE) #draw a random sample from the data
  store.means[i]=mean(draw) #calculate the sample mean and store it
}
hist(store.means,breaks=seq(0,100,by=1),xlab="Sample mean age at death",ylab="Frequency",main="Distribution of sample means") #draw a histogram of the sample means
#Try increasing the sample size.







