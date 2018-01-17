####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# plotting2.R - additional functions for drawing plots/features
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

# make multiple plots, for each of the plots passed in through the list
multiplot = function(
			graph.list,
			days=NULL,
			ann=FALSE,
			xlab="",
			ylab="",
			myscale=1,
			...){
	if (is.null(days)) {days = maxday(graph.list)}
	logit_box(days=days,myscale=myscale)
	par(lwd=myscale*10)	
	for(i in graph.list){
		lines(
			i,
			myscale=myscale,...)
	}
	par(lwd=1)
}

# 
glm_legend <- function(x,ld50=FALSE,myscale=1,legendScale=1,...){
  leg=character(0)
  fill=character(0)
  if(!(length(x))){
    return()
  }
  nx=length(x)
  if(!nx){return()}
  bg=rep('white',length(x))
  sym=character(0)
  for(k in x){
    leg = c(leg, k$name)
    fill = c(fill, k$color)
    sym = c(sym,k$pch)
  }
  lopts=list(
    x='topright'
    ,bty='n'
    ,cex=2*myscale*legendScale
  )
  lopts$legend=leg
  lopts$fill=fill
  do.call(legend, lopts)
}

