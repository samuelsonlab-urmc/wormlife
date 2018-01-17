####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# generic_tools.R - Helper functions for various purposes.
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

# takes an expression and evaluates it in the context of an open jpeg device
myjpeg <- function(expr,file="xyz.jpg",width=720,height=960,verbose=FALSE){
	file=sub(".jpe?g$","",file, perl=TRUE,ignore.case=TRUE)
	file = paste(file,".jpg",sep='')
	if(grepl('/',file)){
		fpath = sub("[^/]*$","",file)
		if(verbose){
			cat("Plotting",file,"\n")
		}
		dir.create(fpath,recursive=TRUE,showWarnings=FALSE)
	}
	jpeg(file=file,width=width,height=height)
	expr
	dev.off()
}

# takes a character vector and returns a nicely formatted scalar with a path - no extra slashes
mypath <- function(...){
	a = character(0)
	for (i in list(...)){
		b = paste(i,collapse="/")
		a = c(a,b)
	}
	a = paste(a,collapse="/")
	a = gsub("\\/+","\\/",a,perl=TRUE)
	a = gsub("\\/$","",a,perl=TRUE)
	return(a)
}

#Adjusts a color as a percentage between itself and black(negative) and white(positive)
lighten <- function(col,p){
	f=colorRamp(c("black",col,"white"))
	x=(p/100 + 1)/2
	x[x>1]=1
	x[x<0]=0
	return(rgb(f(x)/255))
}
darkPlight <- lighten

self.list <- function(...){
	x = list(...)
	nam = names(x)
	xnames = as.character(substitute(expression(...)))[-1]
	xnames = sub('[\\{\\[\\]\\} ].*','',xnames)
	if(is.null(nam)){
		nam = xnames
	}
	for(i in 1:length(nam)){
		if (nam[i]==""){
			nam[i] = xnames[i]
		}
	}
	names(x) = nam
	return(x)
}

self.names <- function(x){
	if(length(x)==0){return(NULL)}
	n = names(x)
	if(is.null(n)){
		n=as.character(x)
	}
	for(i in 1:length(x)){
		if(n[[i]]==""){n[i]=as.character(x[i])}
	}
	names(x) = n
	return(x)
}

# take a numeric and rounds it to round_len decimal places, then formats it to align via monospace font
myformat <- function(x,round_len=2){
	x = round(x,digits=round_len)
	x = format(x,nsmall=round_len)
}

# Alias to paste function with fewer vowels.
pst <- function(...,sep=""){paste(...,sep=sep)}

# Given a table and a column of that table, returns a copy of the table
# with its rows ordered according to the value of the given column
reorder <- function(tab, column){
	y = order(tab[,column])
	tab = tab[y,];
	return(tab)
}

# Truncates numeric values to fall in the [0,1] interval
adjustTo01 = function(x){
	x[is.na(x)] = 0;
	x[x>1]=1;
	x[x<0]=0;
	return(x);
}

# Kills all open graphics devices
dev.kill <- function(){
	while(dev.cur()!=1){
		dev.off()
	}
}

# Sort unique elements
su <-function(...){
	sort(unique(...))
}

nudge <- function(col,p){
	col= col2rgb(col)/255
	rc = runif(3,-p/100,p/100)
	col = col*(1+rc)
	col[col>1]=1
	col[col<0]=0
	return(rgb(t(col)))
}

# Reads a .tsv file using different defaults from those used by read.table
read.tsv <- function(file,header=TRUE,sep="\t",stringsAsFactors=FALSE,row.names=NULL,...){
	out = read.table(file=file,header=header,sep=sep,row.names=row.names,stringsAsFactors=stringsAsFactors,...)
}

# Writes a .tsv file using different defaults from those used by write.table
write.tsv <- function(x, file = "", quote = FALSE, sep = "\t", row.names=FALSE, ...){
	out = write.table(x=x,file=file,sep=sep,quote=quote,row.names=row.names,...)
}

# This returns the first non-null argument it is given. Not used.
pick <- function(...){
	a=list(...)
	for(i in a){
		if(!is.null(i)){
			return(i)
		}
	}
}

# Prepends classes to the S3 classes of x
addClass <- function(x,...){
	myClasses = class(x)
	for(i in list(...)){
		myClasses = c(i,myClasses)
	}
	return(myClasses)
}

# Given a vector or table, turns all NaN entries into NA
noNaN <- function(x){
	if(!is.null(colnames(x))){
		for(i in colnames(x)){
			x[is.nan(x[[i]]),i] <- NA
		}
	}else{
		x[is.nan(x)] <- NA
	}
	return(x)
}


replace.string <- function(x,rep){
	nr=names(rep)
	y=x
	for(i in 1:length(rep)){
		y[y==rep[i]]=nr[i]
	}
	return(y)
}

#reverses the boolean state of x
toggle <-function(x){
	return(!x)
}

testMonotonic <- function(x){
	n=length(x)
	if(n<=1){return(TRUE)}
	y1=x[1:(n-1)]
	y2=x[2:n]
	y=y2-y1
	lneg = y<=0
	lpos = y>=0
	if(sum(lneg)==(n-1)){return(TRUE)}
	if(sum(lpos)==(n-1)){return(TRUE)}
	return(FALSE)
}









