####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# logit.R - functions for modeling survival data through logit curve fits
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

logit_data_columns_default =c(time="time",dead="dead",total="total")

wormlogit <- function(
  tab,
  columns=logit_data_columns_default,
  trial=NULL,
  color="red",
  symbol="cs",
  ID=deparse(substitute(tab)),
  alive0=FALSE,
  annotation=NULL,
  name=ID,...){
  
  tab = data.frame(tab,stringsAsFactors=FALSE)
  mytab=logitModelTable(tab,columns,ID=ID)
  Try({
    g=logitModel(mytab)
  },errorMessage=paste("fit on",ID))
  out = list(
    glm=g,
    color=color,
    symbol=symbol,
    table=tab,
    fun=getLogitLine(g),
    ID=ID,
    name=name,
    b=getLogitCoef(g),
    alive0=alive0,
    annotation=parseAnnotation(tab,annotation=annotation,ID=ID),
    num_worms=countWorms(mytab),
    points=my.points,
    pch=symToPch(symbol),
    trials=NULL,
    ...)
  class(out) = c("wormlogit","wormplot",class(out))
  return(out)
}

countWorms <- function(tab){
  n_worms = sum(as.numeric(tab$alive),as.numeric(tab$dead),na.rm=TRUE)
  return(n_worms)
}

glmCall<-function(tab){
  time = as.numeric(tab[,1])
  alive = as.numeric(tab[,2])
  dead = as.numeric(tab[,3])
  g = glm(cbind(alive, dead) ~ time, family=binomial(logit))
  return(g)
}

#this function assumes three columns, the first has time, the second alive, the third dead
logitGLM<-function(tab,alive0=FALSE){
  l=apply(tab,1,sum)
  l=!is.na(l)
  if(alive0){
    tab= rbind(c(0,1000,0),tab)
  }
  if(sum(l)>=2){
    suppressWarnings({
      g = glmCall(tab)
    })
  }else{
    g = NULL
  }
  return(g)
}

logitModel<-logitGLM
logitmodel<-logitGLM

getLogitCoef <- function(g){
  if(!is.null(g)){
    b = g$coefficients
    names(b) = c("intercept","slope")
    f = function(x){1/(exp(-(b[1]+b[2]*x))+1)}
  }else{
    b =  c(intercept=NA,slope=NA)
    f = function(x){
    }
  }
  return(b)
}

getLogitLine <- function(g){
  if(!is.null(g)){
    b = g$coefficients
    names(b) = c("intercept","slope")
    f = function(x){1/(exp(-(b[1]+b[2]*x))+1)}
  }else{
    b =  c(intercept=NA,slope=NA)
    f = function(x){
      rep(NA,length(x))
    }
  }
  return(f)
}

wormlogit.old <- function(
  tab,
  columns=data_columns_default,
  trial=NULL,
  color="red",
  symbol="cs",
  ID=deparse(substitute(tab)),
  alive0=FALSE,
  annotation=NULL,
  name=ID,...){
  tab = data.frame(tab,stringsAsFactors=FALSE)
  ann=list()
  annotation=self.names(annotation)
  for(i in names(annotation)){
    annot = tab[[annotation[[i]]]]
    ann[[i]] = paste(sort(unique(annot)),collapse="|")
  }
  n_worms = sum(as.numeric(tab[[columns["total"]]]),na.rm=TRUE)
  
  x = as.numeric(tab[[columns["time"]]])
  d = as.numeric(tab[[columns["dead"]]])
  a = as.numeric(tab[[columns["total"]]])-d
  
  if(alive0){
    x = c(0,x)
    a = c(1000,a)
    d = c(0,d)
  }
  if(sum(!is.na(d))>=2){
    suppressWarnings({
      g = glm(cbind(a, d) ~ x, family=binomial(logit))
    })
    b = g$coefficients
    names(b) = c("b","c")
    f = function(x){1/(exp(-(b[1]+b[2]*x))+1)}
  }else{
    g = NULL
    b =  c(b=NA,c=NA)
    f = function(x){
      rep(NA,length(x))
    }
  }
  symbol=sub('^ *','',symbol)
  symbol=sub(' *$','',symbol)
  color=sub('^ *','',color)
  color=sub(' *$','',color)
  out = list(
    glm=g,
    color=color,
    symbol=symbol,
    table=tab,
    fun=f,
    ID=ID,
    name=name,
    b=b,
    alive0=alive0,
    annotation=ann,
    num_worms=n_worms,
    points=my.points,
    pch=mypch[[symbol]],
    ...)
  class(out) = c("wormlogit","wormplot",class(out))
  return(out)
}

getlogit <- function(
  tab,
  ID,
  col='black',
  sym='cs',
  sirname='',...){
  
  g = wormlogit(tab,
                ID=ID,
                name=paste(sirname,sep='',ID),
                color=col,
                symbol=sym,
                points=my.points
                ,...)
  return(g)
}

