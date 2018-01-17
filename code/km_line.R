####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# km_line.R - Plot curves for Kaplan-Meier style survival data
#    "traditional" non-replica set data
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

KM_data_columns_default = c(time='time',dead='dead',censor='censor',live='live')

# 
tradCurve <- function(x,day0=FALSE){
	dead = x[,'dead']
	
	total = total.km_line(x)
	
	rtot = numeric(0)
	last=total
	
	#safe to assume time is sorted unique, from call to daypool
	for(i in dead){
		last = last-i
		rtot = c(rtot,last)
		
	}
	prop=rtot/total
	time=x[,'time']
	curve=cbind(x=c(time[1],time),y=c(1,prop),censor=c(0,x[,'censor']))
	if(day0){
		curve=rbind(c(0,1,0),curve)
	}
	return(curve)
}

# 
km_line <- function(
		tab,
		ID,
		name=ID,
		color="gray",
		lwd=10,
		pch=5,
		cex=lwd/5,
		tot='live',
		day0=FALSE,
		columns = KM_data_columns_default,
		annotation=NULL,
		...){
	x=getDataTable(tab,columns)
	
	x = daypool.matrix(x)
	y = tradCurve(x,day0=day0)
	mycensor=y[,'censor']
	out=list(
		name=name,
		pch=pch,
		lwd=lwd,
		cex=cex,
		col=color, 
		color=color,
		table=tab,
		num_worms=total.km_line(x),
		ID=ID,
		trials=NULL,
		curve=y,
		censor=mycensor,
		annotation=parseAnnotation(tab,annotation=annotation,ID=ID),
		...
		)
	class(out) = 'km_line'
	return(out)
}

# 
total.km_line <- function(x){
	suppressWarnings({
		dead = as.numeric(x[,'dead'])
		time=as.numeric(x[,'time'])
		l=!!(x[,'live'])
	})
	l[is.na(l)]=FALSE
	t=time
	t[!l]=Inf
	if(!sum(l)){
		return(sum(dead))
	}
	time_live0_ind=which.min(t)
	time_live0=time[time_live0_ind]
	
	pre_l= (time<=time_live0)

	live0=x[time_live0_ind,'live']
	total = sum(dead[pre_l],live0,na.rm=TRUE)
	return(total)
}

# 
getKM <-function(tab,ID,col='red',sym='oc',sirname='',...){
		kms = km_line(tab=tab,
			ID=ID,
			name=paste(sirname,ID,sep=''),
			color=col,
			symbol=sym,
			tot='dead',
			day0=TRUE,
			...)
	return(kms)
}

# This function assumes four columns, the first has time, the second alive, the third dead, the fourth censoring
# NOTE: INCOMPLETE?
# TODO: check this
kmmodel<-function(tab,alive0=FALSE){
	x = as.numeric(tab[,1])
	a = as.numeric(tab[,2])
	d = as.numeric(tab[,3])
	
	g=NULL
	b =  c(b=NA,c=NA)
	if(sum(!is.na(d))>=2){
		suppressWarnings({
		g = glm(cbind(a, d) ~ x, family=binomial(logit))
		})
		b = g$coefficients
		names(b) = c("b","c")
		f = function(x){1/(exp(-(b[1]+b[2]*x))+1)}
	}else{
		g = NULL
		b =  c(b=NA,c=NA)
		f = function(x){
			rep(NA,length(x))
			}
	}
	return(g)
}

# TODO: plot points at KM line for observations
points.km_line <- function(x,...){

}
