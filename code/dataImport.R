####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# dataImport.R has functions for reading the survival data files,
#  and running the UI data import wizard to match file columns with expected arguments
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

# read in input file (with survival data)
getRawData <- function(x){
	if(grepl("\\.tsv$",x,ignore.case=TRUE,perl=TRUE)){
		out = read.tsv(x)#,sep="\t",stringsAsFactors=FALSE,header=TRUE)
	}else if(grepl("\\.csv$",x,ignore.case=TRUE,perl=TRUE)){
		out = read.csv(x,stringsAsFactors=FALSE)
		
	}else{
	
		stop("Unable to parse input file")
	}
	
	return (out)
}

# set any instances of inf or NaN to NA
cleanTable <- function(tab,data_columns,km=FALSE){

	for(i in data_columns){
		x=tab[,i]
		x[is.nan(x)] = NA
		tab[,i] = x
	}
	return(tab)
}

# This function matches up the columns in provided data with columns needed for model building
whichData <-function(tkenv,data_columns,standard_columns){
	out=character(0)
	if(length(data_columns) != length(standard_columns)){
		tkmessageBox(
			message=paste(
				"I'm expecting a different number of columns than you've provided me.\nI expect columns holding data for:\n",
				paste(standard_columns,collapse=", "),
				"\nYou gave me:\n",
				paste(data_columns,collapse=", ")
				)
			)
		return(NULL)
	}
	for(i in names(standard_columns)){
		out[[i]] = getSelection(tkenv,data_columns,standard_columns[[i]],msg=paste("Please indicate which column\n indicates measurements for",i))
		data_columns = data_columns[!(data_columns %in% out)]
	}
	return(out)
}
getTablist <-function(tab,IDcols){
	IDtab=tab[,IDcols]
	if(length(IDcols)>1){
		cond=apply(IDtab,1,paste,sep="_",collapse="_")
	}else{
		cond=IDtab
	}
	cond=sub('^_','',cond,perl=TRUE)
	cond=sub('_$','',cond,perl=TRUE)
	cond=gsub('_+','_',cond,perl=TRUE)
	xnames=sort(unique(cond))
	tablist=list()
	for(i in xnames){
		tablist[[as.character(i)]]=tab[cond==i,]
	}
	return(tablist)
}
