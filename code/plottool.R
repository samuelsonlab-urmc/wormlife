####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# plottool.R - loads other WormLife files, and includes functions for retrieving and
#               manipulating data
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

res = ""
# utility function for loading other WormLife files
mypack <-function(x){
  for(i in x){
    source(paste(res,i,sep=""))
  }
}

# WormLife code files
srcfiles=c(
'generic_tools.R',
'modelData.R',
'daypool.R',
'wld.R',

'logit.R',
'km_line.R',
'ld50.R',
'maxday.R',
'tabline.R',

'plotting.R',
'plotting2.R',
'points.R',
'trialmodel.R',

'intro.R',
'wormTK.R',
'dataImport.R',
'graphCommands.R',
'outputStuff.R',
'buildMenu.R'
)

mypack(srcfiles)
control = c(pos="DAF-16",neg="L4440")
cat("Sourcing plottools\n")
ML_addr=TRUE

# utility function for numerical formatting
myformat <- function(x,round_len=2){
	x = round(x,digits=round_len)
	x = format(x,nsmall=round_len)
}

# 
get.line = function(...){
	UseMethod("get.line")
}

# 
get.coef <-function(g){
	bb = (g$b[["b"]])
	cc = (g$b[["c"]])
	LD_50 = - (bb/cc)
	out = list(b=bb,c=cc,LD_50=LD_50)
	return(out)
}

# 
get.line.wormlogit <- function(g,neg){
	ID = g$ID
	out = get.coef(g)
	nl = get.coef(neg)

	out = c(out,rat_LD50=out[['LD_50']]/nl[['LD_50']])
	for(i in names(out)){
		out[[i]] = myformat(out[[i]])
	}
	out = c(ID=ID,out)
	for(i in names(g$annotation)){
		out[[i]] = g$annotation[[i]]
	}
	if(ML_addr){
		pl=sub("^([^_]*)_.*","\\1",ID,perl=TRUE)
		wl=sub("^[^_]*_([^_]*).*","\\1",ID,perl=TRUE)
		od=""
		out = c(order=od,LibPlate=pl,LibWell=wl,out)
	}
	return(out)
}

# 
getControls <- function(clonenames,controlnames){
	out =list()
	for(i in controlnames){
		out[[i]] = grep(i,clonenames,perl=TRUE,value=TRUE,ignore.case = TRUE)[1]
	}
	return(out)
}

# 
get.series = function(x,id="name",...){
	d=x
	graph.list = list()
	xnames = sort(unlist(unique(d[[id]])))
	for(i in xnames){
		cat("Fitting",i,"\n")
		graph.list[[i]] = wormlogit(d[d[[id]]==i,],ID=i,...)
	}
	return(graph.list)
}

# 
get.wormseries <- function(
		d,
		id="ID",
		cn,cp,
		columns=c(time="time",dead="dead",total="total"),
		annotation=NULL,
		wlnames=sort(unlist(unique(d[[id]]))),
		col='red',
		subselect=NULL,...
		){
	xnames = wlnames
	if(!is.null(subselect)){
		xnames=c(cn,cp,subselect)
	}
	tab = character(0)
	neg <- wormlogit(d[grepl(cn,d[[id]]),],
			columns=columns,
			trial=trial,
			color="black",
			sym="oc",
			ID=cn,
			annotation=annotation)
	pos <- wormlogit(d[grepl(cp,d[[id]]),],
			columns=columns,
			trial=trial,
			color="black",
			sym="cc",
			ID=cp,
			annotation=annotation)
	graph.list = list()
	for(i in xnames){
		cat("Fitting",i,"\n")
		graph.list[[i]] = wormlogit(d[d[[id]]==i,],
				columns=columns,
				ID=i,
				annotation=annotation,color=col)

	}
	graph.list$pos=pos
	graph.list$neg=neg
	return(graph.list)
}

# 
do.wormlogit = function(
	d,
	id="ID",
	dir,
	postfix="",
	columns=data_columns_default,
	trial=NULL,
	annotation=NULL,
	col='red',
	points=my.points,
	control = c(pos="DAF-16",neg="L4440"),
	subselect=NULL,
	...){
	xnames = sort(unlist(unique(d[[id]])))
	if(!is.null(subselect)){
		xnames=c(control,subselect)
	}
	if(!is.null(trial)){
		do.trialview(d,id=id,dir=dir,columns=columns,trial=trial,annotation=annotation,subselect=subselect)
	}
	
	tab = character(0)
	cn = control[[2]]
	cp = control[[1]]
	dirplot=mypath(dir,"graphs")
	dir.create(dir,recursive=TRUE,showWarnings=FALSE)
	dir.create(dirplot,recursive=TRUE,showWarnings=FALSE)
	graph.list = get.wormseries(
		d,
		id=id,
		cp=cp,
		cn=cn,
		columns=columns,
		annotation=annotation,
		wlnames=xnames,
		col=col,
		subselect=subselect)
	neg <- graph.list[['neg']]
	pos <- graph.list[['pos']]

	daymax= maxday(graph.list)
	cat("Days scale from 0 to",daymax,"\n")
	wormplot(pos,neg,file=mypath(dirplot,"controls"),verbose=TRUE,days=daymax,points=points)
	for(i in xnames){
		wormplot(pos,neg,graph.list[[i]],file=mypath(dirplot,i),verbose=TRUE,days=daymax,points=points)
		tab <- rbind(tab,as.character(get.line(graph.list[[i]],neg)))
	}
	if(!(cp %in% names(graph.list))){
		tab <- rbind(tab,as.character(get.line(pos,neg)))
	}
	if(!(cn %in% names(graph.list))){
		tab <- rbind(tab,as.character(get.line(neg,neg)))
	}
	tab = data.frame(tab,stringsAsFactors=FALSE)
	myColumns=names(get.line(neg,neg))
	colnames(tab) = myColumns
	rownames(tab) = tab$ID
	write.table(tab,
		file=mypath(dir,"fit-table.tsv"),
		row.names=FALSE,
		quote=FALSE,
		sep="\t")
	return(tab)
}

# 
split.table = function(x,split=NULL,sid=NA){
	out=list()
	if(is.null(split)){
		return(list("all"=x))
	}
	if(is.na(sid)){sid = paste(dnames(split),collapse="_")}
	cat(sid,"\n")
	x = mergeColumns(x,columns=split,outname=sid)
	xnames = sort(unlist(unique(x[[sid]])))
	for(i in xnames){
		out[[i]] = x[(x[[sid]]==i),]
	}
	return(out)
}

#
process = function(
		input,
		rundir,
		days=NA,
		outputdir="results",
		id=c("LibPlate", "LibWell"),
		split=NULL,
		columns=data_columns_default,
		trial=NULL,
		annotation=NULL,
		col='red',
		subselect=NULL,
		...){
	
	if(typeof (input)=="character"){
		d = get.table(input=input,id=id)
	}else{
		d = input
	}

	cat ("Run label:",rundir,"\n")
	tablist <- getTablist(d,IDcols=split)
	outTab=list()
	for(i in names(tablist)){
		dir = mypath(outputdir,rundir,i)
		if(i == 'all'){dir = mypath(outputdir,rundir)}
		outTab[[i]] = do.wormlogit(
			tablist[[i]],
			dir=mypath(outputdir,rundir,i),
			columns=columns,
			trial=trial,
			annotation=annotation,
			col=col,
			subselect=subselect,
			...)
	}
	return(outTab)
}

# 
get_control <- function(x,stext="L4440"){grep(stext,x,value=TRUE)}
get_neg <-	function(x){get_control(x,control['neg'])}
get_pos <-	function(x){get_control(x,control['pos'])}

