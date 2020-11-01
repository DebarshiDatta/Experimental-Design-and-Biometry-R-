#Random draws from the normal distribution often don't look nomal...
sample.size=20
x=rnorm(sample.size,mean=5,sd=1)
hist(x,xlab="Value of x",ylab="Frequency",main="")

#Quantile-quantile plot for a random sample from the normal distribution.
par(mfcol=c(1,2))
sample.size=20
x=rnorm(sample.size,mean=5,sd=1)
hist(x,xlab="Value of x",ylab="Frequency",main="")
qqnorm(x,datax=TRUE) #This quantile-quantile plot will be approximately linear if the sample is approximately normal.

#Ex. 13.1
marine.reserve=read.csv("chap13e1MarineReserve.csv")
par(mfcol=c(1,2))
hist(marine.reserve$biomassRatio,xlab="Biomass ratio",ylab="Frequency",main="")
qqnorm(marine.reserve$biomassRatio,datax=TRUE)
#Run the Shapiro-Wilk test for departure from normality
shapiro.test(marine.reserve$biomassRatio)

#Log tranformation
ratio.log=log(marine.reserve$biomassRatio)
par(mfcol=c(1,2))
hist(ratio.log,xlab="ln(Biomass ratio)",ylab="Frequency",main="")
qqnorm(ratio.log,datax=TRUE)
t.test(ratio.log,mu=0)
#Confidence intervals on tranformed data (back-calculation)
mean.log=mean(ratio.log)
ci.log=t.test(ratio.log,mu=0)$conf.int[1:2]
print(c(ci.log[1],mean.log,ci.log[2])) #The CI will be symmetric on the logarithmic scale.
#Back-transform to the arithmetic scale (geometric mean).
print(exp(c(ci.log[1],mean.log,ci.log[2]))) #The CI will not be symmetric on the arithmetic scale.

#angular transformation
n.individuals=20
prob.survive=0.1
n.replicates=100
x=rbinom(n=n.replicates,size=n.individuals,prob=prob.survive)
proportions=x/n.individuals
par(mfcol=c(1,2))
hist(proportions,xlab="Proportion survived",ylab="Frequency",main="Untransformed proportions")
angular.transformed.proportions=asin(sqrt(proportions))
hist(angular.transformed.proportions,xlab=expression(paste("arcsin(",sqrt(Proportion),")")),ylab="Frequency",main="Angular transformed proportions")


#Square root transformation
control.counts=rpois(n=20,lambda=1)
treatment.counts=rpois(n=20,lambda=5) #Variance=mean, so this group has greater variance.
par(mfcol=c(1,2))
hist(control.counts)
hist(treatment.counts)
print(c(sd(control.counts),sd(treatment.counts)))
sqrt.control.counts=sqrt(control.counts+0.5)
sqrt.treatment.counts=sqrt(treatment.counts+0.5)
hist(sqrt.control.counts)
hist(sqrt.treatment.counts)
print(c(sd(sqrt.control.counts),sd(sqrt.treatment.counts)))

#Sign test, and Wilcoxon signed-rank test
#Ex. 13.4
speciation=read.csv("chap13e4SexualConflict.csv")
hist(speciation$difference,breaks=seq(-5000,25000,by=500),xlab="Difference",ylab="Frequency",main="")
#Sign test (just a binomial test on >0 vs. <0
n.pairs=length(speciation$difference!=0) #Drop pairs if difference=0.
n.positive=sum(speciation$difference>0)
binom.test(x=n.positive,n=n.pairs,p=0.5)
#***The Wilcoxon test is not valid for this data set, because the distribution of differences is not symmetric around the median. But this is how to run it in R:
wilcox.test(speciation$difference)
#Or,
wilcox.test(x=speciation$nSpeciesMultipleMating,y=speciation$nSpeciesSingleMating,paired=TRUE)

#Mann-Whitney U test, for two samples
#Ex. 13.5
times=read.csv("chap13e5SagebrushCrickets.csv")
par(mfcol=c(2,1))
hist(times$timeToMating[times$feedingStatus=="starved"],xlab="Time to mating (hours)",ylab="Frequency",main="Starved")
hist(times$timeToMating[times$feedingStatus=="fed"],xlab="Time to mating (hours)",ylab="Frequency",main="Fed")
#
qqnorm(times$timeToMating[times$feedingStatus=="starved"])
qqnorm(times$timeToMating[times$feedingStatus=="fed"])
#
#The wilcox.test() function does the Mann-Whitney U test if there are two samples (x,y) and paired=FALSE
wilcox.test(x=times$timeToMating[times$feedingStatus=="starved"],y=times$timeToMating[times$feedingStatus=="fed"],paired=FALSE)
#Or, use the model formula syntax:
wilcox.test(times$timeToMating~times$feedingStatus)

#Permutaion test with time to mating data.
sample.difference=mean(times$timeToMating[times$feedingStatus=="starved"])-mean(times$timeToMating[times$feedingStatus=="fed"])
n.randomizations=10000
difference.between.means=array(0,dim=n.randomizations)
for(i in 1:n.randomizations){
  randomized.data=sample(x=times$timeToMating,size=length(times$timeToMating),replace=FALSE)
  mean.starved=mean(randomized.data[times$feedingStatus=="starved"])
  mean.fed=mean(randomized.data[times$feedingStatus=="fed"])
  difference.between.means[i]=mean.starved-mean.fed
}
x.axis.bins=seq(-40,50,by=1)
color.bars=array(dim=length(x.axis.bins))
color.bars[x.axis.bins<sample.difference]="blue"
color.bars[x.axis.bins>=sample.difference]="white"
hist(difference.between.means,breaks=x.axis.bins,col=color.bars,xlab="Difference between means",ylab="Frequency",main="")
p.value=2*sum(difference.between.means<sample.difference)/n.randomizations
print(p.value)


