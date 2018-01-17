####################################################################################
# WormLife- Analysis Tools for High-Throughput C Elegans Survival Data
#
# trialview.R - functions for implementing the TrialView functionality
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

MODEL_TYPES=c('Logit','Kaplan-Meier')
MODEL_COLUMNS=list()
MODEL_COLUMNS[[1]]=list(time=c('time','day'),dead='dead',total='total')
MODEL_COLUMNS[[2]]=list(time=c('time','day'),dead='dead',live='live',censor='censor')
names(MODEL_COLUMNS)=MODEL_TYPES
MODEL_FUNCTION=list(logit=getlogit,km=getKM)

# 
getControls <- function(clonenames,controlnames){
	out =list()
	for(i in controlnames){
		out[[i]] = grep(i,clonenames,perl=TRUE,value=TRUE,ignore.case=TRUE)[1]
	}
	return (out)
}

# 
trial.graphs <- function(tab,trialCols,modelfun=getlogit,col='red',sym='oc',gname=''){
	tablist=getTablist(tab,trialCols)
	tnames=names(tablist)
	out=list()
	for(i in tnames){
		out[[i]]=modelfun(
			tablist[[i]],
			i=i,
			col=col,
			sym=sym,
			sirname=paste(gname,"_",i,sep='')
			)
	}
	return(out)
}

# 
process_trialview <- function(tab,
		outputdir='trialviews',
		idcols,
		trialcols='trial',
		controls=control,
		modelfun=getlogit,
		modelcol=MODEL_COLUMNS[[1]]
		){
	dir.create(outputdir,recursive=TRUE,showWarnings=FALSE)
	tablist=getTablist(tab,idcols)
	xnames=names(tablist)
	gcon=list()
	for(i in controls){
		gcon[[i]]=trial.graphs(tablist[[i]],trialcols,modelfun,col='black',gname=i)
	}
	glist=list()
	for(i in xnames){
		mylog=trial.graphs(tablist[[i]],trialcols,modelfun,col='red',gname=i)
		ts=names(mylog)
		ntrials=length(ts)
		jpeg(paste(outputdir,'/',i,'.jpg',sep=''),height=960,width=720*ntrials)
		par(mfrow=c(1,ntrials))
		for(t in ts){
			mygraphs=list()
			for(g in names(gcon)){
				gt=gcon[[g]][[t]]
				if(!is.null(gt)){
					mygraphs[[paste(g,t,sep='_')]]=gt
				}
			}
			mygraphs[[paste(i,t,sep='_')]]=mylog[[t]]
			multiplot(mygraphs,main=paste("Trial",t))
			
		}
		dev.off()
		glist[[i]]=mylog
	}
	return(glist)
}

# 
stdev_trialview <- function(tab,
		idcols,
		trialcols,
		neg=control['neg'],
		modelfun=getlogit,
		modelcol=MODEL_COLUMNS[[1]]
		){
	tablist=getTablist(tab,idcols)
	xnames=names(tablist)
	gcon=trial.graphs(tablist[[neg]],trialcols,modelfun,col='black',gname=i)
	ld_con=ld50(gcon)
	print(ld_con)
	rat_list=list()
	for(i in xnames){
		mylog=trial.graphs(tablist[[i]],trialcols,modelfun,col='red',gname=i)
		ld_g=ld50(mylog)
		ng=names(ld_g)
		rat_g=ld_g[ng]/ld_con[ng]
		rat_list[[i]]=rat_g
	}
	return(rat_list)
}

# 
do.trialview <- function(
		tab,
		id="ID",
		trial=NULL,
		subselect=NULL,
		...){
	xnames = sort(unlist(unique(d[[id]])))
	dirplot=mypath(dir,"trialview")
	dir.create(dirplot,recursive=TRUE,showWarnings=FALSE)
	daymax=0
	cn = control[[2]]
	cp = control[[1]]
	glist=list()
	
	glist[['pooled']]=get.wormseries(
		d,
		id=id,
		cp=cp,
		cn=cn,
		columns=columns,
		annotation=annotation,
		wlnames=xnames,
		subselect=subselect)
	daymax = max(daymax,maxday(glist[['pooled']]))
	
	if (!is.null(trial)){
		d[[trial]]=as.character(d[[trial]])
		for( i in sort(unique(d[[trial]]))){
			glist[[as.character(i)]]=get.wormseries(
				d[d[[trial]]==i,],
					id=id,
					cp=cp,
					cn=cn,
					columns=columns,
					annotation=annotation,
					wlnames=xnames,
					subselect=subselect)
			daymax = max(daymax,maxday(glist[[i]]))
		}
	}
	ntrials=length(glist)
	cat("Days scale from 0 to",daymax,"\n")
	jpeg(file=paste(mypath(dirplot,"controls"),".jpg",sep=""),height=960,width=720*ntrials)
	par(mfrow=c(1,ntrials))
	for(i in names(glist)){
		multiplot(graph.list=list(glist[[i]]$pos,glist[[i]]$neg),days=daymax,ann=TRUE,main=paste("Trial",i),cex.main=5,points=raw.points)
		text(x=0,y=1.1,labels=paste("Trial",i),pos=4,cex=4)
	}
	dev.off()
	for(j in xnames){
		jpeg(file=paste(mypath(dirplot,j),".jpg",sep=""),height=960,width=720*ntrials, quality=20)
		par(mfrow=c(1,ntrials))
		for(i in names(glist)){
			multiplot(graph.list=list(glist[[i]]$pos,glist[[i]]$neg,glist[[i]][[j]]),days=daymax,ann=TRUE,main=paste("Trial",i),cex.main=5,points=raw.points)
			text(x=0,y=1.1,labels=paste("Trial",i),pos=4,cex=4)
			text(x=daymax,y=1.1,labels=paste("pop_size=",glist[[i]][[j]]$num_worms,sep=''),pos=2,cex=4)
		}
		dev.off()
	}
}

# 
process = function(input,
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
	tablist <- split.table(d,split=split)
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


control = c(pos="DAF-16",neg="L4440")
# 
get_control <- function(x,stext="L4440"){
	grep(stext,x,value=TRUE)
}
get_neg <-	function(x){get_control(x,control['neg'])}
get_pos <-	function(x){get_control(x,control['pos'])}



