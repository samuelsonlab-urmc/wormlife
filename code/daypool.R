####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# daypool.R - Functions for pooling results across trials for the same observation timepoints
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

daypool <- function(x,...){
	UseMethod("daypool")
}

daypool.default <-function(x,...){
  	xn = deparse(substitute(x))
	ux = sort(unique(x))
	ll=self.list(...)
	lo=list()
	lo[[xn]]=ux
	for(j in names(ll)){
		y = ll[[j]]
		uy = numeric(0)
		for(i in ux){
			yx = y[x==i]
			uy[[as.character(i)]] = sum(yx,na.rm=TRUE)
		}
		lo[[j]] = uy
	}
	return(lo)
}

# Takes as input a matrix with the order days, dead, live, censored
# Pools rows in a table that have the same value in the first column
daypool.matrix <-function(x,...){
	tab =x
	x= tab[,1]
	ncols =dim(tab)[2]
	ux = sort(unique(x))
	uxn = length(ux)
	out = matrix(0,nrow=uxn,ncol=ncols,dimnames =list(NULL,colnames(tab)))
	out[,1]=ux
	for(j in 2:ncols){
		y = tab[,j]
		uy = numeric(0)
		for(i in 1:uxn){
			out[i,j] = sum(y[x==ux[i]],na.rm=TRUE)
		}
	}
	return(out)
}

daypool.data.frame <-function(x,...){
	out = daypool.matrix(x)
	out = data.frame(x)
	return(out)
}
