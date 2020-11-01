humanGeneLengths=read.csv("chap04e1HumanGeneLengths.csv")
head(humanGeneLengths)
hist(humanGeneLengths$geneLength,xlab="Gene length (base pairs)",main="")
#A few extremely long genes make it difficult to see:
max(humanGeneLengths$geneLength)
#Look at only genes with length<=15000:
geneLengthsUpTo15K=subset(humanGeneLengths, geneLength <= 15000)
hist(geneLengthsUpTo15K$geneLength,xlab="Gene length (base pairs)",main="Subset length<=15000")

#Population mean
meanLength=mean(humanGeneLengths$geneLength)
meanLength
#Population variance: divide by N (not by n-1)
N=length(humanGeneLengths$geneLength)
varLength=sum((humanGeneLengths$geneLength-meanLength)^2)/N
varLength
#Population standard deviation
sqrt(varLength)

#Take a sample of n=100
geneSample100=sample(humanGeneLengths$geneLength, size = 100, replace = FALSE)
hist(geneSample100,xlab="Gene length",main="")
mean(geneSample100)
#Try it again (re-enter the three previous commands to take a new sample)

#Repeat the process and save the sample means
reps=10000
sampleSize=100
sampleMeans=array(dim=reps)
for(i in 1:reps){
  newSample=sample(humanGeneLengths$geneLength, size = sampleSize, replace = FALSE)
  sampleMeans[i]=mean(newSample)
}
hist(sampleMeans,xlab="Sample mean length",main="")
#Calculate the mean and standard deviation of the sample means
mean(sampleMeans)
sd(sampleMeans)
#Repeat with sampleSize=10

