timeshift=read.csv("chap15e1KneesWhoSayNight.csv")
stripchart(timeshift$shift~timeshift$treatment,vertical=TRUE,method="jitter",xlab="Treatment",ylab="Shift (h)")
###Run an Anova
#Create a linear model first.
timeshift.lm=lm(timeshift$shift~timeshift$treatment)
#Then create the Anova table.
anova(timeshift.lm)

###Planned (a priori) comparisons: In Ex. 15.1, compare the mean of the knee group to the mean of the control group
###using the equation at the top of page 473.
mean.knee=mean(timeshift$shift[timeshift$treatment=="knee"])
mean.control=mean(timeshift$shift[timeshift$treatment=="control"])
difference=mean.knee-mean.control
###Calculate the sample sizes.
n.knee=sum(timeshift$treatment=="knee")
n.control=sum(timeshift$treatment=="control")
MS.error=0.4955445 #Copy from the Anova table, or try anova(timeshift.lm)[[3]][2]
SE.difference=sqrt(MS.error*(1/n.knee+1/n.control))
t.difference=difference/SE.difference
df.difference=length(timeshift$shift)-3 #df=N-#groups
p.value=2*pt(q=t.difference,df=df.difference,lower.tail=TRUE)
print(p.value)

###Alternatively...
summary(timeshift.lm) #This extracts different information from the lm. "Intercept" is the mean for the first (alphabetical) group.
#The p-values compare the intercept (control) to 0 and each of the other 2 to the intercept.
#If you want to compare to another group, rearrange the order using the levels() function.

###Also, note the R-squared value in the output of summary().

###Unplanned (post-hoc) comparisons: Tukey's Honestly Significant Difference test
#Use the package multcomp (see WHitlock & Schluter's website),
#OR use aov instead of lm:
timeshift.aov=aov(timeshift$shift~timeshift$treatment)
summary(timeshift.aov) #This should be identical to the output from anova(timeshift.lm) above.
TukeyHSD(timeshift.aov) #All (post-hoc) pairwise comparisons.

################################
###Check the assumptions of normality and equal variance.
#Normality test (also try qqnorm):
shapiro.test(timeshift$shift[timeshift$treatment=="control"])
shapiro.test(timeshift$shift[timeshift$treatment=="knee"])
shapiro.test(timeshift$shift[timeshift$treatment=="eyes"])
#Equal variance (if you have the car package installed):
library(car)
leveneTest(timeshift$shift,group=timeshift$treatment,center=mean)

###Non-parameteric test: Kruskal-Wallis. (Not necessary in this case, but this is the command.)
kruskal.test(timeshift$shift~timeshift$treatment)

################################
###Practice problem chap. 15, #1
caffeine=read.csv("chap15q01HoneybeeCaffeine.csv")
head(caffeine)
stripchart(caffeine$consumptionDifferenceFromControl~caffeine$ppmCaffeine,vertical=TRUE,xlab="Caffeine concentration (ppm)",ylab="Difference in amount of nectar taken (g)")
caff.lm=lm(caffeine$consumptionDifferenceFromControl~caffeine$ppmCaffeine)
anova(caff.lm) #***What's wrong with this?
###Try this:
caff.lm=lm(caffeine$consumptionDifferenceFromControl~factor(caffeine$ppmCaffeine))
anova(caff.lm)
###Which groups differ from each other? (Post-hoc comparisons with the Tukey test require aov())
caff.aov=aov(caffeine$consumptionDifferenceFromControl~factor(caffeine$ppmCaffeine))
summary(caff.aov) #This should be the same as anova(caff.lm).
TukeyHSD(caff.aov)






