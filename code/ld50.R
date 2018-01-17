####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# ld50.R - calculate median lifespan - NOTE: "LD50" values are median lifespan
#  TODO: Change references from "LD50" to "median lifespan"
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

ld50 <- function(g){
	UseMethod('ld50')
}

ld50.default <- function(g){
	cat("Error in ld50.default: proper classname not found. See ld50.R\n")
	return(666)
}

ld50.km_line <- function(g){
	x=list()
	x$x=g$curve[,'x']
	x$y=g$curve[,'y']
	n=length(x$x)
	lt=x$y<=.5
	gt=x$y>=.5
	if(!sum(gt)){return(NA)}
	ld=which.max((1:n)[gt])
	return(x$x[ld])
}

ld50.wormlogit <-function(g){
	ld50 = -g$b[1]/g$b[2]
	return(round(ld50,2))
}

ld50.list <- function(g){
	a=numeric(0)
	for(i in names(g)){
		a[[i]]=ld50(g[[i]])
	}
	return(a)
}

ld50.wormGraph <-function(g){
	ld50 = ld50(g$wormModel)
	return(ld50)
}

dump.ld50 <-function(x,name,outputdir='tables'){
	dir.create(outputdir,showWarnings=FALSE)
	myfile=paste(outputdir,'/',name,".tsv",sep="")
	write.tsv(file=myfile,ld50(g=x),row.names=TRUE,col.names=FALSE)
}
