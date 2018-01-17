####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# modelData.R - Transforms survival data into a table suitable for use in the
#                 later modeling steps
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

getDataTable <- function(tab,dcols,addRowInd=FALSE){
	x=tab
	xcols = colnames(x)
	for(i in names(dcols) ){
		if(!sum(grepl(dcols[[i]],xcols,ignore.case=TRUE))){
			print(dcols)
			print(xcols)
			stop("Column ",i," not found!!!")
		}else{
			dcols[[i]]=grep(dcols[[i]],xcols,ignore.case=TRUE,value=TRUE)[1]
		}
	}
	
	y = data.frame(x[,dcols],stringsAsFactors=FALSE)
	colnames(y) = names(dcols)
	if(addRowInd){
		nr=dim(y)[1]
		y$rowind=1:nr
	}
	return(y)
}

# turns a table and specified columns into a annotation string
parseAnnotation <- function(tab, annotation=stop('Specify annotation!'), ID='unspecified'){
	ann=list()
		annotation=self.names(annotation)
		for(i in names(annotation)){
			annot = tab[[annotation[[i]]]]
			annot2=sort(unique(annot))
			ann[[i]] = paste(annot2,collapse="|")
		}
	return(ann)
}

#turns a table and specified columns into a simple table fit for logit modeling
logitModelTable <- function(tab,columns,ID='unspecified'){
	tab = data.frame(tab,stringsAsFactors=FALSE)
	x = as.numeric(tab[[columns["time"]]])
	d = as.numeric(tab[[columns["dead"]]])
	a = as.numeric(tab[[columns["total"]]])-d

	out=data.frame(time=x,alive=a,dead=d,stringsAsFactors=FALSE)
	l.na = is.na(out)
	l=apply(l.na,1,sum)
	if(sum(a[!l]<0)){
		warning("Logit data Error: more dead than total found for table ",ID)
	}
	if("rowind" %in% colnames(tab)){
		out$rowind=tab$rowind
	}
	return(out)
}

# turns a table and specified columns into a simple table fit for traditional modeling
logitModelTable <- function(tab,columns,ID='unspecified'){
	tab = data.frame(tab,stringsAsFactors=FALSE)
	x = as.numeric(tab[[columns["time"]]])
	d = as.numeric(tab[[columns["dead"]]])
	a = as.numeric(tab[[columns["total"]]])-d

	out=data.frame(time=x,alive=a,dead=d,stringsAsFactors=FALSE)
	l.na = is.na(out)
	l=apply(l.na,1,sum)
	if(sum(a[!l]<0)){
		warning("Logit data Error: more dead than total found for table ",ID)
	}
	if("rowind" %in% colnames(tab)){
		out$rowind=tab$rowind
	}
	return(out)
}
