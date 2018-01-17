####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# graphCommands.R - functions for manipulating the GUI plot window
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

# add a line
addGraph <- function(tkenv){
	Try({
		xnames=names(tkenv$tablist)
		ynames=names(tkenv$graph.list)
		l=!(xnames %in% ynames)
		mySymFun=getSymbol
		if(tkenv$model==2){
			mySymFun = function(tkenv,x){return(x)}
		}
		if(sum(l)){
			gname=getSelection(tkenv,xnames[l],varname='XXX',msg="Select graph to add.")
			tkenv$graph.list[[gname]]=
				tkenv$getmod(
					tkenv$tablist[[gname]],
					gname,
					col=getColor("black"),
					sym=mySymFun(tkenv,'cc'),
					annotation=tkenv$columns$annotation
					)
		}
	})
	tkrreplot(tkenv$img)
}

# remove a line
removeGraph <- function(tkenv){
	Try({
	
		ynames=names(tkenv$graph.list)
		print(ynames)
		if(length(ynames)){
			gname=getSelection(tkenv,ynames,'XXX',msg="Select graph to remove.")
			if(gname=='series'){
				tkenv$series=NULL
				tkenv$seriesCounter=NA
			}
			tkenv$graph.list[[gname]]=NULL
		}
	})
	tkrreplot(tkenv$img)
}

# set the x-axis scale
setXscale <- function(tkenv){
	Try({
		newDays=getSelection(tkenv,seq(0,120,by=5),'XXX',msg="Select manual scale for x axis, 0 for automatic scaling.")
		if (!newDays){newDays=NULL}
		tkenv$setDay = newDays
	})
	tkrreplot(tkenv$img)
}

# modify a line
changeGraph <- function(tkenv){
	Try({
		ynames=names(tkenv$graph.list)
		print(ynames)
		gname=getSelection(tkenv,ynames,'XXX',msg="Select graph to modify.")
		og=tkenv$graph.list[[gname]]
		glist=tkenv$graph.list
		glist[[gname]]=tkenv$getmod(
				tkenv$tablist[[gname]],
				gname,
				col=getColor(og$col),
				sym=getSymbol(tkenv,og$sym),
				annotation=tkenv$columns$annotation
				)
		tkenv$graph.list=glist
	})
	tkrreplot(tkenv$img)
}

# clear all lines
clearGraph <- function(tkenv){

	tkenv$graph.list=list()
	tkrreplot(tkenv$img)
}

# define a series of plots
defineSeries <- function(tkenv){
	mySymFun=getSymbol
	if(tkenv$model==2){
		mySymFun = function(tkenv,x){return(x)}
	}
	xnames=names(tkenv$tablist)
	gname=getSelection(tkenv,xnames,varname='XXX',msg="Select graph to display first.")
	mycol=getColor("red")
	mysym=mySymFun(tkenv,'cs')
	
	Try({
		
		tkenv$series=list()
		for(i in xnames){
			tkenv$series[[i]] =
				tkenv$getmod(
					tkenv$tablist[[i]],
					i,
					col=mycol,sym=mysym,
					annotation=tkenv$columns$annotation
					)
		}
		tkenv$graph.list[['series']]=NULL
		tkenv$graph.list[['series']]=tkenv$series[[gname]]
		tkenv$seriesCounter = which(xnames==gname)		
	})
	tkrreplot(tkenv$img)
}

# go to the previous plot in the series
goLeft <- function(tkenv){
	if(is.null(tkenv$series)){return(FALSE)}
	if(tkenv$seriesCounter<=1){return(FALSE)}
	tkenv$seriesCounter = tkenv$seriesCounter - 1
	tkenv$graph.list[['series']]=tkenv$series[[tkenv$seriesCounter]]
	tkrreplot(tkenv$img)
	return(TRUE)
}

# go to the next plot in the series
goRight <- function(tkenv){
	if(is.null(tkenv$series)){return(FALSE)}
	if(tkenv$seriesCounter >= length(tkenv$series)){return(FALSE)}
	tkenv$seriesCounter = tkenv$seriesCounter + 1
	tkenv$graph.list[['series']]=tkenv$series[[tkenv$seriesCounter]]
	tkrreplot(tkenv$img)
	return(TRUE)
}

# jump to the specified plot in the series
seriesJump <- function(tkenv, k=stop("Error in seriesJump: k not specified!")){
	if(is.null(tkenv$series)){return(FALSE)}
	if(k >= length(tkenv$series)){return(FALSE)}
	if(k <= 1){return(FALSE)}
	tkenv$seriesCounter = k
	tkenv$graph.list[['series']]=tkenv$series[[tkenv$seriesCounter]]
	tkrreplot(tkenv$img)
	return(TRUE)
}



