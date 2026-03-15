
use.interval=0.0001
use.table=qnorm(seq(use.interval,1-use.interval,use.interval))
n.table.options=length(use.table)


simulate.allModels=function(N,nRespAlt,I_t,a,params,useTrunc,isInterrogate,isFFI,
                         maxCounter,stepSize,use.table,n.table.options) {
  
  tmp=.C("allModels",nRespAlt=as.integer(nRespAlt),z=rep(params["sz"],2),
         I_t=I_t,a=a,
         beta=rep(params["beta"],2),lambda=rep(params["lambda"],2),
         s=rep(params["stoch.s"],2),h=stepSize,
         sv=params["sv"],
         resp=rep(0,N),rt=rep(0,N),
         n=N,maxiter=maxCounter,
         useTrunc=useTrunc,isInterrogate=isInterrogate,isFFI=isFFI,
         rangeLow=as.integer(0),rangeHigh=as.integer(n.table.options-1),
         randomTable=as.double(use.table))
  
  tmp$rt=tmp$rt + params["t0"]
  
  out=list(rt=tmp$rt,resp=tmp$resp)
}


