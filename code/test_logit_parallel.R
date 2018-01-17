####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# test_logit_parallel.R - alternative parallelized implementation for
#                         significance testing between logit curves
# Note that these functions replace the original non-parallelized version.
#      Results should be identical when used with the same random seed.
#
# Copyright (C) 2017  Jesse Llop, modified by Adam Cornwell. Samuelson Lab, URMC.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
####################################################################################

library(doParallel)

# given two logit curve fits, use monte carlo sampling
# to test for a statistically significant difference between them
test_logit <-function(g1,g2,
                      k=10000,
                      p.only=TRUE,
                      prop=FALSE, # proportional?
                      quiet=FALSE,
                      start=NULL,
                      epsilon=1E-4,
                      maxit=25,
                      trace=FALSE){
  
  tab1=prepData.byWorm(g1)
  tab2=prepData.byWorm(g2)
  tab=rbind(tab1,tab2)
  nr1=dim(tab1)[1]
  nr2=dim(tab2)[1]
  if(is.null(nr1)|is.null(nr2)){
    cat('Skipped resampling: nr==null\n')
    if(p.only){
      return(NA)
    }else{
      return(list(condition=NULL,resamp=NULL,p=NA))
    }
  }

  l=labFT(nr1,nr2)
  
  f=function(g){
    cof=g$coefficients
    ld50=0-cof[1]/cof[2]
    names(ld50)=NULL
    return(ld50)
  }
  
  cp<-function(x1,x2,y){
    countTwoSided(abs(x1-x2),y,center=0)
  }
  if(prop){
    cp<-function(x1,x2,y){
      countTwoSided(x1/x2,y)
    }
  }
  g1=logitmodel(tab1)
  g2=logitmodel(tab2)
  obs.b=list(coef(g1),coef(g2),coef(logitmodel(tab))  )
  results=matrix(NA,k,2)
  condition=c(ld1=f(g1),ld2=f(g2))
  
  ld_rel=abs(condition[1]-condition[2])
  if(prop){
    ld_rel=abs(condition[1]/condition[2])
  }
  p=0
  
  # parallel version of logit curve statistical testing
  results <- foreach(i = icount(k), .combine = "rbind")%dopar%{
    if(!quiet){cat('\r',i)}
    lsamp=sample(l)
    gx=logitmodel(tab[!lsamp,],
                  start=obs.b[[3 ]],
                  epsilon=epsilon,
                  maxit=maxit,trace=trace)
    gy=logitmodel(tab[lsamp,],
                  start=obs.b[[3 ]],
                  epsilon=epsilon,maxit=maxit,trace=trace)
    resxy=c(ld1=f(gx),ld2=f(gy))
    return(resxy)
  }
  
  if(!quiet){cat('\n')}
  if(p.only){
    l=	cp(results[,1],results[,2],ld_rel)
    p=sum(l)/k
    return(p)
  }else{
    rownames(results)=paste('iter',1:k,sep='')
    l=	cp(results[,1],results[,2],ld_rel)
    p=sum(l)/k
    return(list(condition=condition,resamp=results,p=p))
  }
}

# This function was reimplemented to
# fix a few issues compared to original version
prepData <- function(x){
  if('wormlogit' %in% class(x) ){
    x=x$glm$data$tab
  }
  if('logitmodel' %in% class(x) ){
    x=x$tab
  }
  if(!('data.frame' %in% class(x)) ){
    x=as.data.frame(x,stringsAsFactors=FALSE)
  }
  return(cleanLogit(x) )
}
