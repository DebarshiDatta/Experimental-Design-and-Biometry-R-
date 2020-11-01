#####Simulate binomial draws under the null hypothesis (p=0.5)

replicates=10000 #Simulate the sampling process this many times
right.handed=array(0,dim=19) #Create an array to store the results. It has n+1 values because a sample of n=18 can have one of 19 outcomes: 0-18 right-handed.

for(i in 1:replicates){
  toads=sample(x=c(0,1),size=18,prob=c(0.5,0.5),replace=TRUE) #Create a random sample under true H0: p=0.5
  right.handed[sum(toads)+1]=right.handed[sum(toads)+1]+1 #Record the number of 1's (=right-handed toads). Use sum(toads)+1 because the array index starts at 1, but a sample can have 0 right-handed.
}

fraction.right.handed=right.handed/replicates #Divide by the number of replicates to get the fraction
barplot(fraction.right.handed,names.arg=0:18,ylim=c(0,0.2),xlab="# right-handed",ylab="Relative frequency")

#####Calculate the P-value by summing the fractions for outcomes at least as extreme as x=14
test.statistic=14
upper.tail=sum(fraction.right.handed[(test.statistic+1):19])
lower.tail=sum(fraction.right.handed[1:(18-test.statistic+1)])
print(lower.tail+upper.tail)

############################################################
#####Plot exact binomial distribution

prob.x=dbinom(x=0:18,size=18,prob=0.5)
barplot(prob.x,names.arg=0:18,ylim=c(0,0.2),xlab="x=# right-handed",ylab="p(x)=probability of x")

#####Calculate the P-value by summing the fractions for outcomes at least as extreme as x=14
test.statistic=14
p.upper.tail=sum(prob.x[(test.statistic+1):19])
p.lower.tail=sum(prob.x[1:(18-test.statistic+1)])
print(p.lower.tail+p.upper.tail)

############################################################
#####Binomial test

binom.test(x=1,n=10,p=0.07)

