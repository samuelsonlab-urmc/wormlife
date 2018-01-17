####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# wld.R - save or load the current state of the workspace
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

# 
myPare <-function(x){
  l=grepl('tkenv',x)
  y=x[l]
  z=sub('tkenv\\$','',y)
  return(z)
}

# objects in workspace to persist to disk
saveables= c(
  #State variables
  "tkenv$draw.legend",
  "tkenv$legendScale",
  "tkenv$series",
  "tkenv$seriesCounter",
  
  #need more code...
  "tkenv$columns",
  "tkenv$data_columns",
  
  "tkenv$landscape",
  "tkenv$myTable",
  "tkenv$inFile",
  
  #Data variables
  "tkenv$tablist",
  "tkenv$model",
  "tkenv$getmod",
  "tkenv$graph.list",
  "x")

# there are not used now, but might be in the future
future=	c(
  "tkenv$ann_columns",
  "tkenv$trial_columns",
  "x")

SAVEABLES=myPare(saveables)

# these are workspace objects
# NOT to persist to disk
nosaves=c(
  #GUI components, defined on startup
  "tkenv$tt",
  "tkenv$lbutton",
  "tkenv$rbutton",
  "tkenv$img",
  
  # no uncommented code for these
  "tkenv$km",
  "tkenv$wm_models",

  "tkenv$index",
  "tkenv$mycons",
  "tkenv$mylist",
  "tkenv$plotFun",
  "tkenv$loaded",
  "x")

# save persistence object
saveWld <- function(tkenv,fileName=NULL){
  
  if(is.null(fileName)){
    fileName <- tclvalue(tkgetSaveFile(initialfile = "mysession.wld",
                                       filetypes = "{{WormLife data} {.wld}} {{All files} *}"))
  }
  if(!is.null(fileName)){
    wldS(tkenv,fileName)
  }else{
    tkmessageBox(message="No file was selected!")
  }
}

# load previously saved workspace data
loadWld <- function(tkenv,fileName=NULL){
  fileName<-tclvalue(tkgetOpenFile(parent=tkenv$tt,
                                   filetypes = "{{WormLife data} {.wld}} {{All files} *}"))
  if (!nchar(fileName))
    tkmessageBox(message="No file was selected!")
  else{
    wldL(tkenv,fileName)
    tkrreplot(tkenv$img)
  }
}

# 
wldS <-function(tkenv,fileName){
  save(list=SAVEABLES,envir=tkenv,file=fileName)
}

# 
wldL <-function(tkenv,fileName){
  e=new.env()
  load(envir=e,file=fileName)
  elist=ls(envir=e)
  if(congruent(elist,SAVEABLES)){
    for(i in elist){
      tkenv[[i]]=e[[i]]
    }
  }else{
    tkmessageBox(message=paste("Could not load",fileName,"not a proper .wld file!"))
  }
}

# 
congruent<- function(x,y){
  a=sum(!(x %in% y)) 
  b=sum(!(y %in% x)) 
  return(!a & !b)
}
