
rm(list=ls())

howManyCores = 3 # number of cores to use for the simulations. 

folderName="Datasets_CollapsingNoTruncatedLCA_freeResp" #Change this to change the folder where info about the simulations is saved. Use a different folder name for each set of simulations you want to run, for example, if you wanted to run another set of simulations with no leakage, you could set folderName="Datasets_CollapsingTruncatedLCA_FreeResp_NoLeak" or something like that.

freeRespOrInterrogation=c("freeResp","interrogation") [2]
fixedOrCollapseThresholds=c("fixed","linearCollapse") [2]
inhibition=c("none","lateral","FFI") [2]
leakage=c("leak","noLeak") [1]
truncAtZero=c("trunc","noTrunc") [2]

nSim=40 # number of simulations, will be bigger for the actual simulations, but set to 10 here for testing purposes.

nTrials=10000 # number of trials, could increase this for the actual simulations, but set to 10000 here for testing purposes.
maxCounter=2000 # number of time steps to simulate for. If a response is not made by the end of the time steps, then that trial is ended and marked as a non-response.
stepSize=0.005 # size of the time step. So, for example, if stepSize=0.005 and maxCounter=2000, then the maximum RT that can be simulated is 10 seconds (0.005*2000).

allOV=c(3,4,5,6,7,8,9) #Overall values, could be x axis
allDiff=c(0,0.5,1,1.5,2,2.5,3) #differences between the alternative values. different lines in the graph, for example.



useParamNames=NULL

if (freeRespOrInterrogation=="freeResp") {
  useParamNames=c(useParamNames,"a.intercept")
  a.intercept.range=c(3,6)
  isInterrogate=0
} else if (freeRespOrInterrogation=="interrogation") {
  isInterrogate=1
}

if (fixedOrCollapseThresholds=="linearCollapse") {
  useParamNames=c(useParamNames,"a.slope")
  a.slope.range=c(1,4)
}

if (inhibition=="lateral") {
  useParamNames=c(useParamNames,"beta")
  beta.range=c(1,8)
  isFFI=0
} else if (inhibition=="none") {
  isFFI=0
} else if (inhibition=="FFI") {
  isFFI=1
}

if (leakage=="leak") {
  useParamNames=c(useParamNames,"lambda")
  lambda.range=c(1,8)
}

if (truncAtZero=="trunc") {
  useTrunc=1
} else if (truncAtZero=="noTrunc") {
  useTrunc=0
}

useNumPars=length(useParamNames)



library(lhs)



if (useNumPars>0) {
  use.LHS=randomLHS(n=nSim,k=useNumPars)
  colnames(use.LHS)=useParamNames
  
  use.range=array(NA,c(useNumPars,2))
  rownames(use.range)=useParamNames
  colnames(use.range)=c("Min","Max")
  
  if (freeRespOrInterrogation=="freeResp") {
    use.range["a.intercept",]=a.intercept.range
  }
  if (fixedOrCollapseThresholds=="linearCollapse") {
    use.range["a.slope",]=a.slope.range
  }
  if (inhibition=="lateral") {
    use.range["beta",]=beta.range
  }
  if (leakage=="leak") {
    use.range["lambda",]=lambda.range
  }
  
  for (useParam in useParamNames) {
    use.LHS[,useParam]=use.range[useParam,"Min"]+
      use.LHS[,useParam]*(use.range[useParam,"Max"]-use.range[useParam,"Min"])
  }
  
  save(file="latestSim.Rdata",folderName)
  
  save(file=paste("simInfo_",folderName,".Rdata",sep=""),
       howManyCores,folderName,freeRespOrInterrogation,
       fixedOrCollapseThresholds,inhibition,leakage,
       truncAtZero,nSim,nTrials,maxCounter,stepSize,
       allOV,allDiff,useParamNames,useNumPars,
       isInterrogate,isFFI,useTrunc,use.LHS,
       use.range)
  
  tmpCall1.1='"jobnum=1"'
  tmpCall1.2='"jobnum=$i"'
  
  useCall1=paste("for i in $(seq 1 ",howManyCores,"); do sed -s s/",tmpCall1.1,"/",tmpCall1.2,"/g <simStuffForMultiCore.R> ",folderName,"_$i.R; done",sep="")
  useCall2=paste("for i in $(seq 1 ",howManyCores,"); do nohup R CMD BATCH --no-restore ",folderName,"_$i.R & done",sep="")
  system(useCall1)
  system(useCall2)
  
  
} else {
  source("simulate_allModels.R")
  dyn.load("allModels.so")
  data=list(Time=NULL,Resp=NULL,OV=NULL,DIFF=NULL)
  for (j in 1:length(allOV)) {
    for (k in 1:length(allDiff)) {
      v1=(allOV[j]/2)+(allDiff[k]/2)
      v2=(allOV[j]/2)-(allDiff[k]/2)
      I_t=c(v1,v2)
      a1=rep(0,maxCounter)
      a2=rep(0,maxCounter)
      a=c(a1,a2)
      beta=0
      lambda=0
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
  save(file=paste(folderName,"/LHS_",i,".Rdata",sep=""),data,allOV,allDiff)
}

q(save="no")


