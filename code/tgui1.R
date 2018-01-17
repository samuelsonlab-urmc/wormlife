####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# tgui1.R - plot-related functions for series and trialview
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

# Builds trialview plots
doTview <- function(graph.list,trialCols,modelfun){
  tlists=list()
  xnames=names(graph.list)
  ts=character(0)
  for(i in 1:length(graph.list)){
    xg=graph.list[[i]]
    tlists[[i]]=trial.graphs(xg$table,trialCols=trialCols,modelfun=modelfun,col=xg$col,sym=xg$sym,gname=xnames[i])
    ts=sort(unique(c(names(tlists[[i]]),ts)))
  }
  names(tlists)=xnames
  print(names(tlists))
  print(ts)
  ntrials=length(ts)
  par(mfrow=c(1,ntrials))
  for(t in ts){
    mygraphs=list()
    for(g in names(tlists)){
      gt=tlists[[g]][[t]]
      if(!is.null(gt)){
        mygraphs[[paste(g,t,sep='_')]]=gt
      }
    }
    multiplot(mygraphs,main=paste("Trial",t),myscale=.6)
    glm_legend(mygraphs,myscale=.5)
    
  }
}

controls=c('L4440','DAF-16')
trialcols='trial'
mydata=read.tsv('data/lifespan/AS22_25.tsv');idc=c('LibPlate','LibWell')
mytablist=getTablist(mydata,IDcols=idc)
mylist=list()
for(i in names(mytablist)){mylist[[i]]=getlogit(mytablist[[i]],col='red',sym='cs',i=i)}

mycons=list()
for(ic in controls){
  mycons[[ic]]=getlogit(mytablist[[ic]],col='black',sym='oc',i=ic)
}
mymod=getlogit

# 
showX<-function(x){
  xlist=mycons
  xnames=names(mylist)
  
  xlist[[xnames[x]]]=mylist[[x]]
  doTview(xlist,trialCols='trial',mymod)
}

start_list=mycons
start_list[['condition']]=mylist[[1]]

require(tkrplot)

# 
getPlotFun <- function(tkenv){
  f<-function(){
    x=tkenv$index
    xlist=tkenv$mycons
    xnames=names(tkenv$mylist)
    
    xlist[[xnames[x]]]=tkenv$mylist[[x]]
    doTview(xlist,trialCols='trial',tkenv$getmod)
  }
  return(f)
}

# Demo of trialview?
trialDemo <- function(mycons,mylist){
  tkenv = new.env()
  tkenv$index=1
  myData = NULL
  tkenv$tt <- tktoplevel() 
  tkenv$loaded=FALSE
  tkenv$draw.legend=TRUE
  tkenv$landscape=FALSE
  tkenv$getmod=getlogit
  tkwm.title(tkenv$tt,paste("Trialview Demo"))
  topMenu <- tkmenu(tkenv$tt)
  tkconfigure(tkenv$tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  
  tkenv$mycons=mycons
  tkenv$mylist=mylist
  tkwm.geometry(tkenv$tt,paste("=",480*2,"x",500,"+100+50",sep=''))
  
  # go to previous plot in series
  goLeft<-function(){
    tkenv$index=max(tkenv$index-1,1)
    tkrreplot(tkenv$img)
  }
  
  # go to next plot in series 
  goRight<-function(){
    tkenv$index=tkenv$index+1
    tkrreplot(tkenv$img)
  }
  
  tkbind(tkenv$tt, "<Left>",goLeft)
  tkbind(tkenv$tt, "<Right>",goRight)
  tkbind(tkenv$tt, "<Down>",goLeft)
  tkbind(tkenv$tt, "<Up>",goRight)
  tkadd(topMenu,
        "command",
        label="left",
        command=goLeft)
  tkadd(topMenu,
        "command",
        label="right",
        command=goRight)
  tkenv$graph.list=start_list
  tkenv$plotFun <- getPlotFun(tkenv)
  tkadd(topMenu,
        "command",
        label="Quit",
        command=function() 
          tkdestroy(tkenv$tt))
  
  tkfocus(tkenv$tt)
  tkenv$img <- tkrplot(tkenv$tt,fun=tkenv$plotFun,hscale=2,vscale=1)
  
  tkgrid(tkenv$img)
  tkwait.window(tkenv$tt)
  return(tkenv)
}

