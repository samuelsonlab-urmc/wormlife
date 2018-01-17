####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# outputstuff.R - utility functions for producing output files
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

get.line = function(...){
	UseMethod("get.line")
}

get.line.wormLogit <- function(g){
	ID = g$ID
	bb = g$b[1]
	cc = g$b[2]
	LD_50 = - (bb/cc)
	out = c(ID=ID,b=bb,c=cc,LD_50=LD_50)
	return( out)
}

printSeries <-function(tkenv){
	foldFileName <- tclvalue(tkgetSaveFile(initialfile = "folder",
	filetypes = "{{All files} *}"))
	if(!is.null(foldFileName)){
		x=tkenv$seriesCounter
		while(goLeft(tkenv)){}
		y=tkenv$seriesCounter;
		saveGraph(tkenv,paste(foldFileName,tkenv$series[[y]]$ID,sep="/"))
		while(goRight(tkenv)){
			y=tkenv$seriesCounter;
			saveGraph(tkenv,paste(foldFileName,tkenv$series[[y]]$ID,sep="/"))
		}
	}
}


saveGraph <-function(tkenv,jpegFileName=NULL){
	if(is.null(jpegFileName)){
		jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "foo.jpg",
			filetypes = "{{JPEG Files} {.jpg .jpeg}} {{All files} *}"))
	}
	if(!is.null(jpegFileName)){
		glist=tkenv$graph.list
		myjpeg({
				multiplot(glist,days=tkenv$setDay)#
				if(tkenv$draw.legend){
					glm_legend(glist,legendScale=tkenv$legendScale)
				}
			},
			file=jpegFileName
			,height=960,width=720
		)
	}

}

# saves graph from GUI, in landscape format
saveLandscapeGraph <-function(tkenv){
	jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "foo.jpg",
	filetypes = "{{JPEG Files} {.jpg .jpeg}} {{All files} *}"))
	if(!is.null(jpegFileName)){
		glist=tkenv$graph.list
		myjpeg({
				multiplot(glist,days=tkenv$setDay)#,days=35)#,days=days)#,days=35)
				if(tkenv$draw.legend){
					glm_legend(glist,legendScale=tkenv$legendScale)
				}
			},
			file=jpegFileName
			,height=720,width=960
		)
	}
}

# saves table of data, in tsv format
saveTable <- function(tkenv){
	if(is.null(tkenv$series)){return(FALSE)}
	fileName <- tclvalue(tkgetSaveFile(initialfile = "results.tsv",
	filetypes = "{{All files} *}"))
	if(!is.null(fileName)){
		mytab=numeric(0)
		for(i in 1:length(tkenv$series)){
			y=tableLine(tkenv$series[[i]])
			print(y)
			mytab=rbind(mytab,y)
		}
		write.tsv(mytab,file=fileName)
	}
}
