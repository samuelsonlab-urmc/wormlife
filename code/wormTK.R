####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# wormTK.R - Handles TK-based GUI "wizards" for data import and adding lines to plots
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

require(tcltk)
MODEL_TYPES=c('Logit','Kaplan-Meier')
MODEL_COLUMNS=list()
MODEL_COLUMNS[[1]]=list(time=c('time','day','hour'),dead='dead',total='total')
MODEL_COLUMNS[[2]]=list(time=c('time','day','hour'),dead='dead',live='live',censor='censor')
names(MODEL_COLUMNS)=MODEL_TYPES
MODEL_FUNCTION=list(logit=getlogit,km=getKM)

# 
userDefineColumns <-function(tkenv,myColumns){
  selCol = list(data=c("dead","total",'live','censor','time','hour','day'),
                name=c("libPlate","libWell","ID","day","temp",'strain','clone','condition','RNAi','treatment'),
                trial=c("trial","rep",'plate'),
                annotation=c("temp","strain","food","conc","set","incubator","note","notes","date","experiment")
  )
  for (i in names(selCol)){
    multi=FALSE
    if(i %in% c("data","name","annotation","trial")){multi=TRUE}
    selCol[[i]] = getSelection(tkenv,myColumns,selCol[[i]],msg=selMSG[[i]],multi=multi)
    cat(i,"\t",selCol[[i]],"\n")
    myColumns = myColumns[!(myColumns %in% selCol[[i]])]
  }
  return(selCol)
}

# 
userSelectModel <-function(tkenv,selCol){
  ncolumns=length(selCol$data)
  default_model=MODEL_TYPES[[1]]
  if(ncolumns==4){
    default_model=MODEL_TYPES[[2]]
  }
  model=which(MODEL_TYPES ==
                getSelection(
                  tkenv,
                  MODEL_TYPES,
                  default_model,
                  msg="What type of study is this? Highthroughput (logit) or traditional (Kaplan-Meier)")
  )
  return(model)
}

# 
getData <- function(tkenv){
  inFile <- getFile(tkenv)
  myTable = getRawData(inFile)
  myColumns <- colnames(myTable)
  
  # Make the user define the data, annotation, identification and trial columns
  selCol=userDefineColumns(tkenv,myColumns)
  # get the type of model
  model=userSelectModel(tkenv,selCol)
  # define the data columns more specifically
  data_columns=whichData(tkenv,selCol$data,MODEL_COLUMNS[[model]])
  
  if(!is.null(data_columns)){
    # TODO: This next line needs fixing?
    colnames(myTable)=replace.string(colnames(myTable),data_columns)
    
    tkenv$model=model
    tkenv$getmod = MODEL_FUNCTION[[tkenv$model]]
    
    # TODO: These are unused so far
    tkenv$inFile = inFile
    tkenv$myTable = myTable
    tkenv$columns = selCol
    tkenv$data_columns = data_columns
    Try({
      clearGraph(tkenv)
      tkenv$tablist = getTablist(myTable, selCol$name)
      tkenv$loaded=TRUE
    })
    return(tkenv)
  }
}

# 
selMSG = list(
  data = "Please select columns that \ncontain the data \n(i.e. time/day, dead, total/live, censored.",
  dead =	"Please select column for number of dead worms",
  total =	"Please select column for total number of worms",
  time =	"Please select column for time of measurement",
  name =	"Please select columns which identify \nexperimental condition (RNAi clone, chemicals added, strain, etc).",
  trial =	"Please select columns which define different trials.",
  annotation = "Please select columns which hold\n data annotating this experiment."
)

# 
getSelection <- function(
  tkenv,
  x,
  varname,
  msg=paste("Please select your choice for",varname[1],".",sep=""),multi=FALSE){
  defaultCol=c()
  for(i in varname){
    grp = grep(i,x,ignore.case=TRUE)
    if(length(grp)){
      if(multi){
        defaultCol=c(defaultCol,grp-1)
      }else{
        defaultCol=grp[1]-1
        break
      }
    }
  }
  
  # window setup
  tdata<-tktoplevel(tkenv$tt)
  tkwm.deiconify(tdata)
  tkgrab.set(tdata)
  tkwm.title(tdata,"Data Helper")
  tkwm.geometry(tdata,"350x400+100+100")
  
  my = new.env()
  # widgets
  smode = "single"
  if(multi){
    smode = "multiple"
  }
  scr <- tkscrollbar(tdata, repeatinterval=5,
                     command=function(...)tkyview(tl,...))
  tl<-tklistbox(tdata,height=4,selectmode=smode,yscrollcommand=function(...)tkset(scr,...),background="white")
  my$choice <- NULL
  for (i in x)
  {
    tkinsert(tl,"end",i)
  }
  for (i in defaultCol){
    tkselection.set(tl,i)  # Default column. Indexing starts at zero.
  }
  #OK button block
  OnOK <- function()
  {
    my$choice <- x[as.numeric(tkcurselection(tl))+1]
    if(!(length(my$choice) | multi)){
      
    }
    print(my$choice)
    tkgrab.release(tdata)
    tkdestroy(tdata)
  }
  OK.but <-tkbutton(tdata,text="OK",command=OnOK)
  tkbind(tdata, "<Return>",OnOK)
  tkbind(tdata, "<Double-Button-1>",OnOK)
  
  # Layout block
  scrollwidth=15
  labelheight=50
  buttonheight=20
  tkplace(tklabel(tdata,text=msg),height=labelheight)
  tkplace(OK.but,
          height=buttonheight,
          x=0,
          y=labelheight,
          relwidth=1.0,
          width=-scrollwidth)
  tkplace(tl,
          relwidth=1.0,
          width=-scrollwidth,
          relheight=1.0,
          height=-(labelheight+buttonheight),
          y=labelheight+buttonheight)
  tkplace(scr,relheight=1.0,anchor="ne",width=scrollwidth,relx=1.0,x=0,y=0)
  
  tkfocus(tdata)
  tkbind(tdata, "<Destroy>", function() {tkgrab.release(tdata);tkfocus(tkenv$tt)})
  tkwait.window(tdata)
  
  return(my$choice)
}

# 
getFile <- function(tkenv){
  fileName<-tclvalue(tkgetOpenFile(parent=tkenv$tt,
                                   filetypes = "{{Tab separated} {.tsv}} {{Tab separated} {.txt}} {{Comma separated} {.csv}} {{Excel} {.xls}} {{All files} *}"
  ))
  
  if (!nchar(fileName))
    tkmessageBox(message="No file was selected!")
  else{
    input = fileName
    return(fileName)
  }
}

# TODO: change the name of this function to something less close
#         to a reserved word.
Try <- function(expr,errorMessage=NULL)
{
  if (data.class(result<-try(expr,TRUE))=="try-error"){
    if(is.null(errorMessage)){
      errorMessage = as.character(result)
    }
    tkmessageBox(title="An error has occured!",message=errorMessage,icon="error",type="ok")
  }else{
    return (result)
  }
}

# 
TryTcl <- function(expr,errorMessage=NULL)
{
  if (data.class(result<-try(expr,TRUE))=="try-error")
  {
    if(is.null(errorMessage)){
      
      errorMessage <- grep("[tcl]",as.character(result),value=TRUE,fixed = TRUE)
    }
    tkmessageBox(title="An error has occured!",message=errorMessage,icon="error",type="ok")
    
  }else{
    return (result)
  }
}

# 
getColor <- function(old.color='black',caption='')
{
  color <- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=old.color,title=paste("Choose a color",caption)))))
  if (nchar(color)>0){
    return(color)
  }else{
    return(old.color)
  }
}

# 
getSymbol <- function(tkenv,old.symbol=oc){
  y=getSelection(tkenv,pchnames,varname=pchnames[old.symbol],msg="Please choose your symbol.")
  l=pchnames==y
  return(names(mypch[l]))
}
