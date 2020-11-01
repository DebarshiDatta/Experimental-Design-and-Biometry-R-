###One-sample t test.
bt=read.csv("chap11e3Temperature.csv")
head(bt)
mean(bt$temperature) 
#H0: mean body temp. does not differ from 98.6F
t.test(bt$temperature,mu=98.6)
#Take a look at the distribution of sample values.
hist(bt$temperature)

###Paired-sample t test.
rwbb=read.csv("chap12e2BlackbirdTestosterone.csv")
head(rwbb) #Use the log-transformed data in the last two columns.
t.test(rwbb$logBeforeImplant,rwbb$logAfterImplant,paired=TRUE)
#Was the test for (before-after) or (after-before)? Check the mean difference:
mean(rwbb$logBeforeImplant-rwbb$logAfterImplant)
#You could also use the one-sample t test on the differences:
diffs=rwbb$logBeforeImplant-rwbb$logAfterImplant
t.test(diffs) #This should give the same answer.
#Also check the distribution of *differences*:
hist(diffs)

###Two-sample t test
hl=read.csv("chap12e3HornedLizards.csv")
head(hl)
#Subsetting (the hard way):
live=hl$squamosalHornLength[hl$Survival=="living"]
dead=hl$squamosalHornLength[hl$Survival=="killed"]
t.test(live,dead)
#Or use the model statement (the easy way)
t.test(hl$squamosalHornLength~hl$Survival)
#Check the distribution in each group.
par(mfcol=c(2,1))
hist(live)
hist(dead)
sd(live)#One NA in the sample...(check the documentation for sd).
sd(live,na.rm=TRUE)
sd(dead)


