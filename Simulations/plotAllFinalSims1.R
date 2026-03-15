
rm(list=ls())

files=dir("AllFinalSims1/")

files=c(files[1:3],files[5],files[4])

print(files)

allModels=length(files)

inputSums=c(3,4,5,6,7,8,9)
nInputSums=length(inputSums)

allACCs=array(NA,c(nInputSums,allModels))
allMCRTs=array(NA,c(nInputSums,allModels))
allMRTs=array(NA,c(nInputSums,allModels))
allMisses=array(NA,c(nInputSums,allModels))

for (i in 1:length(files)) {
  load(paste("AllFinalSims1/",files[i],sep=""))
  allACCs[,i]=unlist(lapply(data,function(x)mean(x$Resp==1)))
  allMisses[,i]=unlist(lapply(data,function(x)mean(x$Resp==-1)))
  allMCRTs[,i]=unlist(lapply(data,function(x)mean(x$Time[x$Resp==1])))
  allMRTs[,i]=unlist(lapply(data,function(x)mean(x$Time)))
}




pdf(file=paste("PLOT_AllFinalSims1.pdf",sep=""),
    width=6.9,height=3.4)
m=matrix(1:2,nrow=1,byrow=F)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3:4])
#m=rbind(m[1:2,],0,m[1,],0,m[4,],0,m[5:6,])
layout(mat=m,
       widths=c(0,1,0,1,0),
       heights=c(0,1,0))
par(mar=c(4.5,4.5,1,1))
par(xpd=TRUE)


plot(x=inputSums,y=allACCs[,1],col=1,
     ylim=c(0.7,1),type="l",xlab="Input Sum",ylab="Accuracy")
for (i in 2:length(files)) {
  lines(x=inputSums,y=allACCs[,i],col=i)
}


plot(x=inputSums,y=allMCRTs[,1],col=1,
     ylim=c(0.6,1.7),type="l",xlab="Input Sum",ylab="MCRT")
for (i in 2:length(files)) {
  lines(x=inputSums,y=allMCRTs[,i],col=i)
}

legend(6.875,1.725,#inset=c(-0.2,0),
       c("No Leak","No Inhibition", "No Collapse", "CB-LCA 1", "CB-LCA 2"),
       col=1:5,lty=1,cex=0.9)


dev.off()


