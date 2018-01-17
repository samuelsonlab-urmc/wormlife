####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# intro.R - Plots the landing screen that displays on load
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

graph1= c(
"N2_L4440",
"eat-2(ad465)_L4440",
"daf-2(e1370)_L4440"
)
start_col=list(
"black",
"blue",
"red")
myintro=c("Welcome","to","Wormplot")

start_list <-function(){

	test_tab=start_tab() 
	xnames=names(test_tab)
	mig=list()
	
	for(i in 1:3){
		mig[[xnames[i]]]=getlogit(
			test_tab[[i]],
			xnames[i],
			col=start_col[[i]],
			pch='cc')
	}
	return(mig)
}

start_tab <-function(){
	test_tab=getTablist(
		getRawData(
			paste(res,'test_data.tsv',sep='')),
		IDcols=c("strain","RNAi")
		)
	y=test_tab[graph1]
	names(y)=myintro
	return(y)
}
