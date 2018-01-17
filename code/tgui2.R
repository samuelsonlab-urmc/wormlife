####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# tgui2.R - includes the function to build and show the main GUI window
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

require(tkrplot)

# build main GUI window
openMain <- function(){
	tkenv = new.env()
	myData = NULL
	tkenv$tt <- tktoplevel() 
	tkenv$loaded=FALSE
	tkenv$draw.legend=TRUE
	tkenv$landscape=FALSE
	tkenv$tablist=start_tab()
	tkenv$getmod=getlogit
	tkwm.title(tkenv$tt,paste("Wormplots","version",version))
	topMenu <- tkmenu(tkenv$tt)
	tkconfigure(tkenv$tt,menu=topMenu)
	fileMenu <- tkmenu(topMenu,tearoff=FALSE)
	tkwm.geometry(tkenv$tt,"=480x640+100+50")
	tkadd(topMenu,
		"command",
		label="left",
		command=function(){
			tkenv$draw.legend=!tkenv$draw.legend
			tkrreplot(tkenv$img)
			})
	tkadd(topMenu,
		"command",
		label="right",
		command=function(){
			tkenv$draw.legend=!tkenv$draw.legend
			tkrreplot(tkenv$img)
			})
	tkenv$graph.list=start_list()
	tkenv$plotFun <- getPlotFun(tkenv)
	tkadd(topMenu,
		"command",
		label="Quit",
		command=function() 
		tkdestroy(tkenv$tt))
		
	tkfocus(tkenv$tt)

	tkenv$img <- tkrplot(tkenv$tt,fun=tkenv$plotFun,hscale=1,vscale=1.33)
	
	tkgrid(tkenv$img)
	tkwait.window(tkenv$tt)
	return(tkenv)
}

# 
addGraph <- function(tkenv){
	Try({
		xnames=names(tkenv$tablist)
		ynames=names(tkenv$graph.list)
		l=!(xnames %in% ynames)
		print(xnames)
		print(ynames)
		mySymFun=getSymbol
		if(tkenv$model==2){
			mySymFun = function(tkenv,x){return(x)}
		}
		if(sum(l)){# & length(xnames) & length(ynames)){
			gname=getSelection(tkenv,xnames[l],varname='XXX',msg="Select graph to add.")
			tkenv$graph.list[[gname]]=
				tkenv$getmod(
					tkenv$tablist[[gname]],
					gname,
					col=getColor("black"),
					sym=mySymFun(tkenv,'cc'))
		}
	})
	tkrreplot(tkenv$img)
}

# 
getPlotFun <- function(tkenv){
	f<-function(){
		multiplot(tkenv$graph.list,myscale=.33)
		if(tkenv$draw.legend){
			glm_legend(tkenv$graph.list,myscale=.33)
		}
	}
	return(f)
}
