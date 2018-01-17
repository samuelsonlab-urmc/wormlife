####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# maxday.R - Determine an appropriate upper bound for x axis of plot based on
#             longest survival value
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

maxday <- function(x,...){
	UseMethod("maxday")
}

maxday.km_line <- function(x){
	kms=x
	days=0
	i=kms
	g=i$curve
	if(is.null(g)){next}
	suppressWarnings({
		days = max(g[,'x'],days,na.rm=TRUE)
	})
	days = 5*ceiling(days/5)
	return (days)
}

maxday.list <- function(graph.list){
	days=0
	for(i in names(graph.list)){
		x=graph.list[[i]]
		days = max(maxday(x),days,na.rm=TRUE)
	}
	days = 5*ceiling(days/5)
	return (days)
}

maxday.wormlogit <- function(x){
	days=0
	propAlive = .02
	yTerm = 1/propAlive-1
	g=x$glm
	
	if(is.null(g)){return(0)}
	b = g$coefficients
	suppressWarnings({
		days = max(g$model[,2],days,na.rm=TRUE)
		if(-b[1]/b[2] < 100){
			days = max((log(yTerm)+b[1])/(-b[2]),days,na.rm=TRUE)
		}
	})
	return (days)
}
