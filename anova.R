###Random assignment of 3 treatments to 3 groups with n1=n2=n3=10 -> N=30 individuals total
random.numbers=sample(x=seq(1,30,by=1),size=30,replace=FALSE)
group1=random.numbers[1:10]
group2=random.numbers[11:20]
group3=random.numbers[21:30]

###Ex. 15.1
timeshift=read.csv("chap15e1KneesWhoSayNight.csv")
#Look at the data 
head(timeshift)
stripchart(timeshift$shift~timeshift$treatment,vertical=TRUE,method="jitter",xlab="Treatment",ylab="Shift (h)")
#Calculate the sample mean for each group.
mean(timeshift$shift[timeshift$treatment=="control"])
mean(timeshift$shift[timeshift$treatment=="eyes"])
mean(timeshift$shift[timeshift$treatment=="knee"])
###Run an Anova
#Create a linear model first.
timeshift.lm=lm(timeshift$shift~timeshift$treatment)
#Then create the Anova table.
anova(timeshift.lm)


