####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# tabline.R - functions for processing strings from model data for tabular output
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
tableLine <- function(x,...){
	UseMethod("tableLine")
}

# 
basicTabLine <- function(x){
	out=character(0)
	out[["ID"]]=x$ID
	out[["number.worms"]]=x$num_worms
	out[["LD50"]]=ld50(x)
	return(out)
}

# 
tableLine.default <- function(x){
	out=basicTabLine(x)
	ann=x$annotation
	for(i in names(ann)){
		out[[i]]=paste(ann[[i]],sep="|")
	}
	return(out)
}

# 
tableLine.wormlogit <- function(x){
	out=basicTabLine(x)
	ann=x$annotation
	for(i in names(ann)){
		out[[i]]=paste(ann[[i]],sep="|")
	}
	return(out)
}

# 
tableLine.km_line <- function(x){
	out=basicTabLine(x)
	ann=x$annotation
	for(i in names(ann)){
		out[[i]]=paste(ann[[i]],sep="|")
	}
	return(out)
}
