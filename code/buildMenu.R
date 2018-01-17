####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# buildMenu.R defines the UI menu structure and associates function calls
#
# Copyright (C) 2017 Jesse Llop, modified by Adam Cornwell. Samuelson Lab, URMC.
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

fmenu=list(
  "Import .csv/tsv"='getData',
  "Load .wld"='loadWld',
  "Save .wld"='saveWld',
  "Save Plot"='saveGraph',
  "Save Landscape Plot"='saveLandscapeGraph',
  "Save Series Plots"='printSeries'
)
gmenu=list(
  'Add Line'='addGraph',
  'Delete Line'='removeGraph',
  'Modify Line'='changeGraph',
  "Define Series"='defineSeries',
  'Clear Line'='clearGraph',
  'Set X scale'='setXscale'
)
smenu=list(
  "Print TrialViews"='printAllTrialViews',
  "Summary table"='saveTable'
)
buildMenus <- function(tkenv){
  topMenu <- tkmenu(tkenv$tt)
  tkconfigure(tkenv$tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  statsMenu <- tkmenu(topMenu,tearoff=FALSE)
  graphMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(topMenu,
        "cascade",
        label="File",
        menu=fileMenu)
  tkadd(topMenu,
        "cascade",
        label="Graphs",
        menu=graphMenu)
  tkadd(topMenu,
        "cascade",
        label="Data",
        menu=statsMenu)
  xmenu=fmenu;ymenu=fileMenu
  for(i in names(xmenu)){
    f=xmenu[[i]]
    tkadd(ymenu,
          "command",
          label=i,
          command=eval(parse(text=paste("function(){",f,"(tkenv)}")))
    )
  }
  xmenu=gmenu;ymenu=graphMenu
  for(i in names(xmenu)){
    f=xmenu[[i]]
    tkadd(ymenu,
          "command",
          label=i,
          command=eval(parse(text=paste("function(){",f,"(tkenv)}")))
    )
  }
  
  xmenu=smenu;ymenu=statsMenu
  for(i in names(xmenu)){
    f=xmenu[[i]]
    tkadd(ymenu,
          "command",
          label=i,
          command=eval(parse(text=paste("function(){",f,"(tkenv)}")))
    )
  }
  
  tkadd(topMenu,
        "command",
        label="Legend",
        command=function(){
          tkenv$draw.legend=!tkenv$draw.legend
          tkrreplot(tkenv$img)
        })
  tkadd(topMenu,
        "command",
        label="<",
        command=function()goLeft(tkenv))
  tkadd(topMenu,
        "command",
        label=">",
        command=function()goRight(tkenv))	
  
  doError <-function(x,y="series not defined."){
    
    f<-function(){
      
      errorMessage <- paste("error in function: ",x,y)
      tkmessageBox(title="An error has occured!",message=errorMessage,icon="error",type="ok")
    }
    return(f)
  }
  
  tkadd(topMenu,
        "command",
        label="Quit",
        command=function() 
          tkdestroy(tkenv$tt))
  
}
