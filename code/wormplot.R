####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# wormplot.R - loads or attempts to install dependencies; calls to open the main plot window
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

local({r <- getOption("repos")
	r["CRAN"] <- "http://cran.r-project.org" 
	options(repos=r)
})
inpacks = installed.packages()[,1]
wlRequires = c('tcltk','tkrplot','survival')
for(i in wlRequires){
	if (!(i %in% inpacks)){
		install.packages(i)
	}
}
for(i in wlRequires){
	#print(i)
	require(package=i,character.only=TRUE ) || stop(paste(i,"library not available"))
}

require(tcltk) || stop("tcl/tk library not available")
require(tkrplot) || stop("tkrplot library not available")
source('plotGUI.R')
openMain()
