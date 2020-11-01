#Ex. 9.2
cancer=read.csv("chap09e2AspirinCancer.csv")
head(cancer) #Note how the data are recorded...

#Put the frequencies in a 2 by 2 contingency table.
cancerTable=table(cancer)
#Create a mosaic plot.
mosaicplot(cancerTable)

#Risk ratio
Pcancer.aspirin=cancerTable[1,1]/(cancerTable[1,1]+cancerTable[1,2])
Pcancer.placebo=cancerTable[2,1]/(cancerTable[2,1]+cancerTable[2,2])
RR=Pcancer.aspirin/Pcancer.placebo

#Odds ratio
Oaspirin=Pcancer.aspirin/(1-Pcancer.aspirin)
Oplacebo=Pcancer.placebo/(1-Pcancer.placebo)
OR=Oaspirin/Oplacebo

#################

#Use a Chi-squared test, which automatically recognizes a table.
chisq.test(cancerTable)

#If you are given only a table with the frequeuncies, enter them in a matrix.
cancer.data=matrix(c(1438,18496,1427,18515),nrow=2,ncol=2,byrow=TRUE)
cancer.data
chisq.test(cancer.data)

#Or, use the raw data from the file:
chisq.test(cancer$aspirinTreatment,cancer$cancer)

##################
#Fisher's exact test doesn't use the Chi-squared approximation. 
fisher.test(cancerTable)

