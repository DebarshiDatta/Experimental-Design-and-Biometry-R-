#Independent
peas=matrix(c(900,300,300,100),nrow=2,ncol=2)
dimnames(peas)=list(c("Tall","Short"),c("Green","Yellow"))
mosaicplot(peas)

#Not independent
peas=matrix(c(900,100,300,300),nrow=2,ncol=2)
dimnames(peas)=list(c("Tall","Short"),c("Green","Yellow"))
mosaicplot(peas)
