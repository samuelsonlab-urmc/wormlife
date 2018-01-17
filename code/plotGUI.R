####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# plotGUI.R - Sets up the main WormLife plotting window
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

# Check for components necessary for plotting in the GUI
require(tcltk) || stop("tcl/tk library not available\n")
require(tkrplot) || stop("tkrplot library not available\n")
res = ""
source(paste(res,"plottool.R",sep=""))
input = ""
version = "0.4"

TKGRAPHSCALE=.5
INIT_LEGEND_SCALE=1

# 
getPlotFun <- function(tkenv){
	f<-function(){
		multiplot(tkenv$graph.list,myscale=TKGRAPHSCALE,days=tkenv$setDay)
		if(tkenv$draw.legend){
			glm_legend(tkenv$graph.list,myscale=TKGRAPHSCALE)
		}
	}
	return(f)
}


# Setup TK variables/parameters
buildTKenv <- function(){
	tkenv = new.env()

	myData = NULL
	
	tkenv$loaded=FALSE
	tkenv$draw.legend=TRUE
	tkenv$landscape=FALSE
	tkenv$tablist=start_tab()
	tkenv$model=1
	tkenv$getmod=getlogit
	tkenv$legendScale=INIT_LEGEND_SCALE
	tkenv$series = NULL
	tkenv$seriesCounter = NA
	tkenv$inFile=""
	tkenv$myTable=NULL
	tkenv$columns=NULL
	tkenv$data_columns = NULL
	tkenv$setDay = NULL
	return(tkenv)
}

# Initialize and open the main GUI window
openMain <- function(){
	tkenv = buildTKenv()
	tkenv$tt <- tktoplevel() 
	tkwm.title(tkenv$tt,paste("Wormlife","version",version))
	tkwm.geometry(tkenv$tt,"=560x640+100+50")
	buildMenus(tkenv)

	tkenv$graph.list=start_list()
	tkenv$plotFun <- getPlotFun(tkenv)
		
	tkfocus(tkenv$tt)

	tkenv$lbutton=tkbutton(tkenv$tt,text="<",command=function()goLeft(tkenv))
	tkenv$rbutton=tkbutton(tkenv$tt,text=">",command=function()goRight(tkenv))
		
	tkenv$img <- tkrplot(tkenv$tt,fun=tkenv$plotFun,hscale=1,vscale=1.33)

	tkgrid(tkenv$lbutton,tkenv$img,tkenv$rbutton)
	tkgrid.configure(tkenv$lbutton,sticky='w')
	tkgrid.configure(tkenv$rbutton,sticky='e')
	
	tkwait.window(tkenv$tt)
	return(tkenv)
}

