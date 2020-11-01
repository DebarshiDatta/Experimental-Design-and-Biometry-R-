wasps=read.csv("chap20e3UnrulyPassengers.csv")
table(wasps) #23 of 32 chose the mated butterfly.
observed.proportion=23/32
observed.proportion

#Likelihood=L(parameter|data)=P(data|parameter), which is binomial.
dbinom(23,size=32,p=0.5) #Null hypothesis: no preference.
dbinom(23,size=32,p=observed.proportion) #This is the maximum likelihood.
#On log scale:
maxLL=log(dbinom(23,size=32,p=observed.proportion))
nullLL=log(dbinom(23,size=32,p=0.5))

#Plot the log-likelihood function over a range of values of the parameter.
proportion=seq(0.1,0.9,by=0.01)
logLike=log(dbinom(23,size=32,p=proportion))
plot(proportion,logLike,type="l",xlab="Proportion",ylab="Log-likelihood")
#Which value of the parameter gives the largest value of log-likelihood?
proportion[which(logLike==max(logLike),arr.ind=TRUE)]
#Or just look at the arrays.

#Confidence interval with chi-squared approximation.
chisq.value=qchisq(p=0.95,df=1,lower.tail=TRUE)/2 #Divide by 2.
proportion=seq(0.4,0.9,by=0.001) #Re-plot over narrower range of p so it's easier to see. 
logLike=log(dbinom(23,size=32,p=proportion))
plot(proportion,logLike,type="l",xlab="Proportion",ylab="Log-likelihood")
segments(x0=0.4,y0=maxLL-chisq.value,x1=0.9,y1=maxLL-chisq.value,lty=2)

#95% CI is the range of values of the parameter (X-axis) between the points where the line crosses the log-likehihood function.
lower.index=which(abs(logLike[1:250]-(maxLL-chisq.value))==min(abs(logLike[1:250]-(maxLL-chisq.value))),arr.ind=TRUE)
upper.index=250+which(abs(logLike[251:501]-(maxLL-chisq.value))==min(abs(logLike[251:501]-(maxLL-chisq.value))),arr.ind=TRUE)
lower=proportion[lower.index]
upper=proportion[upper.index]
arrows(x0=lower,y0=logLike[lower.index],x1=lower,y1=-8.5,col="red")
arrows(x0=upper,y0=logLike[upper.index],x1=upper,y1=-8.5,col="red")
print(c(lower,upper))

###########################################
#Likelihood ratio test, with H0:p=0.5.

#These were calculated above.
maxLL=log(dbinom(23,size=32,p=observed.proportion))
nullLL=log(dbinom(23,size=32,p=0.5))
G.value=2*(maxLL-nullLL) #This is the test statistic.
#Find the p-value from the chi.squared distribution with degrees of freedom =1.
pchisq(q=G.value,df=1,lower.tail=FALSE)


