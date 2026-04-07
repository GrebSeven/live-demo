
rm(list=ls())

jobnum=4

useJob=jobnum

source("simulate_allModels.R")
dyn.load("allModels.so")

load("latestSim.Rdata")

load(paste("simInfo_",folderName,".Rdata",sep=""))

newSeed=Sys.time()
set.seed(as.numeric(newSeed))



trackerFileName=paste("useJobTracker_",folderName,".Rdata",sep="")

cores = howManyCores
allJobs = nSim

if (jobnum==1) {
  useJobTracker=1:cores
  save(useJobTracker,file=trackerFileName)
}



while (useJob <= allJobs) {
  cat("\n",useJob)
  
  use.curr.LHS=useJob
  
  data=list(Time=NULL,Resp=NULL,OV=NULL,DIFF=NULL)
  
  genParams=use.LHS[use.curr.LHS,]
  
  for (j in 1:length(allOV)) {
    for (k in 1:length(allDiff)) {
      v1=(allOV[j]/2)+(allDiff[k]/2)
      v2=(allOV[j]/2)-(allDiff[k]/2)
      I_t=c(v1,v2)
      
      if (freeRespOrInterrogation=="freeResp") {
        if (fixedOrCollapseThresholds=="linearCollapse") {
          a.slope=as.numeric(genParams["a.slope"])
        } else {
          a.slope=0
        }
        a.intercept=as.numeric(genParams["a.intercept"])
        a1=a.intercept-a.slope*(1:maxCounter)*stepSize
        a1[a1<0]=0
        a2=a.intercept-a.slope*(1:maxCounter)*stepSize
        a2[a2<0]=0
      } else if (freeRespOrInterrogation=="interrogation") {
        a1=rep(0,maxCounter)
        a2=rep(0,maxCounter)
      }
      a=c(a1,a2)
      
      if (inhibition=="lateral") {
        beta=as.numeric(genParams["beta"])
      } else {
        beta=0
      }
      
      if (leakage=="leak") {
        lambda=as.numeric(genParams["lambda"])
      } else {
        lambda=0
      }
      
      stoch.s=1
      sv=0
      sz=0
      t0=0
      
      useParams=c(beta=beta,lambda=lambda,stoch.s=stoch.s,sv=sv,sz=sz,t0=t0)
      
      tmp=simulate.allModels(N=nTrials,nRespAlt=2,I_t=I_t,a=a,params=useParams,
                             useTrunc=useTrunc,isInterrogate=isInterrogate,isFFI=isFFI,
                             maxCounter=maxCounter,stepSize=stepSize,
                             use.table=use.table,n.table.options=n.table.options)
      
      data$Time=c(data$Time,tmp$rt)
      data$Resp=c(data$Resp,tmp$resp)
      data$OV=c(data$OV,rep(allOV[j],length(tmp$rt)))
      data$DIFF=c(data$DIFF,rep(allDiff[k],length(tmp$rt)))
    }
  }
  
  save(file=paste(folderName,"/LHS_",use.curr.LHS,".Rdata",sep=""),data,genParams,allOV,allDiff)
  
  load(trackerFileName)
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  useJobTracker=c(useJobTracker,jobnum)
  save(useJobTracker,file=trackerFileName)
  useJob = length(useJobTracker)
}