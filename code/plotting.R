####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# plotting.R - plots features on the plot window for survival curves
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

# This function draws a box of the form we're used to for plotting logit curves.
logit_box <- function(days,ann=FALSE,myscale=1,...){
  par(ann=ann)
  
  par(lwd=myscale*10)
  par(cex=myscale*1)
  if(ann){
    plot(0,0,type="n",xlim=c(0,days),ylim=c(0,1.1),xlab="days",ylab="proportion alive",yaxt="n",xaxt="n")
  }else{
    plot(0,0,type="n",xlim=c(0,days),ylim=c(0,1.1),xlab="",ylab="",yaxt="n",xaxt="n")
  }
  par(lwd=myscale*2)
  blank_axis(days=days,myscale=myscale,ann=ann)
  par(cex=1)
  par(lwd=1)
}

# 
blank_axis <- function(days,myscale=1,ann=FALSE,...){
  axis(
    side=2,
    at=c(0,.2,.4,.6,.8,1),
    tck=-.03,
    labels=ann,
    lwd.ticks=myscale*6)
  axis(
    side=1,
    at=seq(0,days,by=5),
    tck=-.03,
    labels=ann,
    lwd.ticks=myscale*6)
}

# 
lines.wormlogit = function(g,points=g$points,myscale=1,...){
	if(is.null(g)){return()}
	x = seq(0,maxday(g)+5,by=.1)
	if(!is.null(g$fun) & !is.null(g$points)){
		y = g$fun(x)
		lines(x,y,type="l",col=g$color,...)
		g$points(g$glm,g$color,g$symbol,myscale=myscale)#,days=days)
	}else{
		cat("Model g, named",g$ID,"does not have plotting function!!!\n")
	}
}

#
lines.km_line <- function(x,myscale=1,...){
	ldot=list(...)
	col=x$col
	lwd=x$lwd*myscale
	pch=x$pch
	lx=list(x$curve[,1],x$curve[,2],type="s",ylim=c(0,1.1),col=col,lwd=lwd,pch=pch,cex=x$cex)
	for(i in names(ldot)){
		lx[[i]] = ldot[[i]]
	}
	do.call(lines,lx)
}

#-----------------------------------------------------------
# TODO: this function is unfinished
plot.lifespanList <-function(g,days=maxdayList(g),...){
	logit_box()
	for(i in g){
		lines(i,days,...)
	}
	points(g$glm,g$color,g$sym,days=days)
}







