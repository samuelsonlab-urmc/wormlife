####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# points.R - functions for handling plotting of points on survival plots
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

# map plot symbol IDs to descriptive names
# TODO: why not just use a named vector for this?
mypch=c(oc=21,os=22,od=23,ot=24,cc=19,cs=15,cd=18,ct=17)
pchnames=c(
	oc="open circle",
	os="open square",
	od="open diamond",
	ot="open triangle",
	cc="closed circle",
	cs="closed square",
	cd="closed diamond",
	ct="closed triangle"
	)

symToPch <-function(x){
	y=c(oc=21,os=22,od=23,ot=24,cc=19,cs=15,cd=18,ct=17)
	return(y[x])
}

# 
my.points <- function(g,color,sym,myscale=1,...){
	merge.points(g=g,color=color,sym=sym,myscale=myscale,span=0,...)
}

# 
merge.points <- function(g,color,sym,span,myscale=1,...){
	if (is.null(g)){return()}
	x = sort(unique(g$model[,2]))

	y = rep(NA,length(x))
	d = rep(NA,length(x))
	names(y) = x
	names(d) = x
	n = max(x,na.rm=TRUE)
	for(a in x){
		select = (g$model[,2]==a)
		ms = g$model[select,]
		y[paste(a)] = sum(ms[,1][,1])
		d[paste(a)] = sum(ms[,1][,2])
	}
	if(span){
		m=xvar.merge(cbind(x,y,d),span=span)#span=days/20)
		x = m[,1]; p = m[,2]
	}else{
		x = x; p = y/(y+d)
	}
	points(x,p,
		col=color,
		pch=mypch[[sym]],
		bg="white",
		cex=4*myscale,
		...)
}

# 
raw.points <- function(g,color,sym,myscale=1){
	if (is.null(g)){return()}
	x = g$model[,2]

	y = g$model[,1][,1]
	d = g$model[,1][,2]
	x = x; p = y/(y+d)
	points(x,p,col=color,pch=mypch[[sym]],bg="white",cex=4*myscale)
}

# 
xvar.merge <- function(mat,span=2){
	mat = data.frame(mat)
	n = max(mat$x)
	x = seq(1,n,by=span)
	y = rep(NA,length(x))
	d = rep(NA,length(x))
	names(y) = x
	names(d) = x
	for(i in x){
		j = ((span/2)>abs(mat$x-i))*1
		y[paste(i)] = sum(mat$y*j*(span/2-abs(mat$x-i)))
		d[paste(i)] = sum(mat$d*j*(span/2-abs(mat$x-i)))
	}
	p = y/(y+d)
	return(cbind(x,p))
}
