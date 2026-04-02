#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>



int allModels(int *nRespAlt,double *z,
double *I_t,double *full_a,
double *beta,double *lambda,
double *s,double *h,
double *sv,
double *resp,double *rt,
double *n,double *maxiter,
double *useTrunc, double *isInterrogate, double *isFFI,
int *rangeLow, int *rangeHigh, double *randomTable)
{
  int numRespAlt = (int) *nRespAlt;
  double oldx[numRespAlt],keepArray,x[numRespAlt],randNum,isResp,v[numRespAlt];
  int N,i,iter,Maxiter,j,blah,k,index,trunc,interro,FFI,whichMax;
  
  N=(int) *n;
  Maxiter =(int) *maxiter;
  trunc=(int) *useTrunc;
  interro=(int) *isInterrogate;
  FFI=(int) *isFFI;
  GetRNGstate();
  double rhs[numRespAlt];
  
  for (j=0;j<numRespAlt;j++) {
    rhs[j]=sqrt(*h)*s[j];
  }
  
  double a[Maxiter][numRespAlt];
  
  for (i=0;i<Maxiter;i++) {
    for (j=0;j<numRespAlt;j++) {
      index=(int) (i + (j*Maxiter));
      a[i][j] = full_a[index];
    }
  }
  
  
  for (i=0;i<N;i++) {
    isResp=0;
    
    if (FFI==1) {
      oldx[0]=(rand()/(1.0 + RAND_MAX))*z[0];
      oldx[1]=(rand()/(1.0 + RAND_MAX))*z[1];

      x[0]=oldx[0]-oldx[1];
      x[1]=oldx[1]-oldx[0];

      randNum = rand()/(1.0 + RAND_MAX);
      blah = (*rangeHigh) - (*rangeLow) + 1;
      blah = (randNum * blah) + (*rangeLow);
            
      randNum = randomTable[blah];
            
      v[0]=I_t[0]-I_t[1]+randNum*(*sv);
      v[1]=I_t[1]-I_t[0]-randNum*(*sv);
    } else {
      for (j=0;j<numRespAlt;j++) {
        x[j]=(rand()/(1.0 + RAND_MAX))*z[j];
      }

      for (j=0;j<numRespAlt;j++) {
        randNum = rand()/(1.0 + RAND_MAX);
        blah = (*rangeHigh) - (*rangeLow) + 1;
        blah = (randNum * blah) + (*rangeLow);
            
        randNum = randomTable[blah];
            
        v[j]=randNum*(*sv)+I_t[j];
      }
    }
    
    
    iter=0;
    resp[i]=(double) -1.0 ;
    
    do 
{
  iter = iter+1;
  for (j=0;j<numRespAlt;j++) {
    oldx[j]=x[j];
  }

  if (FFI==1) {
    randNum = rand()/(1.0 + RAND_MAX);
    blah = (*rangeHigh) - (*rangeLow) + 1;
    blah = (randNum * blah) + (*rangeLow);
      
    randNum = randomTable[blah];
  }
    
    for (j=0;j<numRespAlt;j++) {
      keepArray = 0;
      for (k=0;k<numRespAlt;k++) {
        if (j==k) {
          keepArray -= oldx[k] * (lambda[j]);
        } else if (FFI==0) {
          keepArray -= oldx[k] * (beta[j]);
        }
      }
      
      if (FFI==0) {
        randNum = rand()/(1.0 + RAND_MAX);
        blah = (*rangeHigh) - (*rangeLow) + 1;
        blah = (randNum * blah) + (*rangeLow);
        
        randNum = randomTable[blah];
      } else if (j==1) {
        randNum=randNum*(-1);
      }
      
      x[j] = oldx[j] + (keepArray + v[j])*(*h) + randNum*rhs[j];
      
      if (trunc==1) {
        if (x[j] < 0) {
          x[j] = 0;
        }
      }
      
      if (interro==0) {
        if (x[j]>=a[iter-1][j]) {
          resp[i]=(double) (j+1) ; 
          isResp=1;
        }
      }
    }

} while (iter<Maxiter & isResp==0) ; 
    if (interro==1) {
      resp[i]=(double) 1;
      whichMax=(int) 0;
      for (j=1;j<numRespAlt;j++) {
        if (x[j]>x[whichMax]) {
          resp[i]=(double) (j+1) ; 
          whichMax=(int) j;
        } else if (x[j]==x[whichMax]) {
          resp[i]=(double) -1.0 ;
        }
      }
    }
rt[i]=((double) iter)*(*h) - (*h)/((double) 2.0);
  }
PutRNGstate();
}

