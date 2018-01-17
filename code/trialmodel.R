####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# trialmodel.R - functions for working with data in a trial-centric view
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
trialList <- function(x,trialCols){
	tab=x$table
	tablist=getTablist(tab,trialCols)
	tnames=names(tablist)
	out=list()
	modelfun=wormlogit
	if('km_line' %in% class(x)){
		modelfun=km_line
	}
	for(i in tnames){
		out[[i]]=modelfun(
			tablist[[i]],
			ID=i,
			color=x$col,
			symbol=x$sym,
			name=paste(x$ID,"_",i,sep='')
			)
		
	}
	return(out)
}

# TODO: NOT IMPLEMENTED YET
# takes x, a list of trials, and controls, a list of lists of trials, then return the trials x has.
matchTrials <- function(x,controls){}

# 
printAllTrialViews <-function(tkenv){
	if(is.null(tkenv$series)){
		return()
	}
	
	foldFileName <- tclvalue(tkgetSaveFile(initialfile = "folder",
		filetypes = "{{All files} *}"))
	
	if(!is.null(foldFileName)){
		dir.create(foldFileName,recursive=TRUE,showWarnings=FALSE)
		x=tkenv$seriesCounter
		while(goLeft(tkenv)){}
		y=tkenv$seriesCounter;
		printTrialView(tkenv,paste(foldFileName,tkenv$series[[y]]$ID,sep="/"))
		while(goRight(tkenv)){
			y=tkenv$seriesCounter;
			printTrialView(tkenv,paste(foldFileName,tkenv$series[[y]]$ID,sep="/"))
		}
	}

}

# 
printTrialView <- function(tkenv, file){
	x=tkenv$graph.list[["series"]]
	y=tkenv$graph.list
	gcon=list()
	for(i in names(y)){
		gcon[[i]]=trialList(y[[i]],tkenv$columns$trial)
	}
	stlist=trialList(x,tkenv$columns$trial)
	ts=names(stlist)
	ntrials=length(ts)
	jpeg(paste(file,'.jpg',sep=''),height=960,width=720*(ntrials+1))
	par(mfrow=c(1,ntrials+1))
	multiplot(tkenv$graph.list,main=paste("Pooled",x$ID))
	text(labels=paste("Pooled",x$ID),x=maxday(tkenv$graph.list)/2,y=1.1,cex=2)
	
	for(t in ts){
		mygraphs=list()
		for(g in names(gcon)){
			gt=gcon[[g]][[t]]
			if(!is.null(gt)){
				mygraphs[[paste(g,t,sep='_')]]=gt
			}
		}
		mygraphs[[paste(x$ID,t,sep='_')]]=stlist[[t]]
		multiplot(mygraphs,main=paste("Trial",t))
		text(labels=paste("Trial",t),x=maxday(mygraphs)/2,y=1.1,cex=2)
		
	}
	dev.off()
}
