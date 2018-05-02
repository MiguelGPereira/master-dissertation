#TODO predict.caren help file
predict.caren<- function(prm,exs,class=NULL,crit='conf', strat='bestrule',preds.format='class',
		all.caren=FALSE,bag=FALSE,nbags=10,boost=FALSE,boost.filter=FALSE,boost.noens=FALSE,
		cm=FALSE,Bas=FALSE,Top=3,	dec=FALSE, vote=FALSE,file=TRUE)
{
	if ( file.exists("caren_temp.bas") ){
		file.copy('caren_temp.bas','caren_temp_train.bas')
	}
	#file.remove('caren_temp.bas.pre',showWarnings=FALSE)
#	if ( file.exists("caren_temp.bas.pre") ){
#		file.remove("caren_temp.bas.pre")
#	}
	File<-'caren_temp.bas'		
	if(is.null(class)) class.option<-"" else class.option <- paste('-class',class,sep='')	
	#if(is.null(class)) class.option<-"" else class.option <- paste('-class',class,sep='')	
	if(strat=='bestrule') strat <- 'dec'
	if(all.caren) option.all <- " -all" 
	else option.all <- ""
	if(bag) option.bag <- paste(" -bag1",nbags,"0.1",sep='') 
	else option.bag <- ""
	# 	if(boost) option.boost <- paste(" -boostcaren_temp_train.bas","10",sep='') 
	#	else option.boost <- ""
	if(boost) option.boost <- paste(" -boostcaren_temp_train.bas,2",sep='') else option.boost <- ""
	if(boost.filter) option.boost.filter <- paste(" -filter",sep='') 
	else option.boost.filter <- ""
	if(boost.noens) option.boost.noens <- paste(" -noens",sep='') 
	else option.boost.noens <- ""
	if(cm) option.cm<-" -cm -file " 
	else option.cm <- ""
	if(dec) option.dec<-" -dec" 
	else option.dec <- ""
	if(vote) option.vote<-" -vote" 
	else option.vote <- ""
	if(file) option.file<-" -file" 
	else option.file <- ""
	#classpath<-paste('-classpath ',.libPaths(),'/','carenR/exec ',sep='')
	#classpath<-paste('-classpath "',paste(c(.libPaths(),'.'),'/','carenR/exec ',sep='',collapse=";"),'"')
	#classpath<-paste('-classpath "', paste(c(.libPaths(),'.'),'/','carenR/exec ',sep='',collapse='":"'),'"',collapse="")
	if(R.version$os=='mingw32') cp.sep<-'";"' else cp.sep<-'":"'
	classpath<-paste("-classpath ",paste(c(.libPaths(),'.'),'/','carenR/exec',sep='',collapse=cp.sep),' ',sep='"')
	if(Bas)
	{
		command<-paste('java ', classpath, 'predict',prm,File,
				option.all,option.bag,option.boost,option.boost.filter,option.boost.noens,
				option.cm,option.dec,option.vote,option.file,
				'-Bas -s, -null? ',
				paste('-T',Top,sep=''),paste('-',crit,sep=''),paste('-',strat,sep=''))
	}
	else
	{
		#		command <- paste('java',classpath,'predict',prm,File, option.all,option.bag,option.boost,option.boost.filter, option.boost.noens,option.cm,'-Att -classCLASS')
		command<-paste('java',classpath,'predict',prm,File,'-Att -s, -null?',class.option,
				option.all,option.bag,option.boost,option.boost.filter,option.boost.noens,
				option.cm,option.dec,option.vote,option.file)
	}
	cat("\n", command," \n")
	write(paste(date(),'>',command),file='class_exp.aux',append=TRUE)
	save.data(exs,'caren_temp.bas',col.names=!Bas)
	my.shell(command)
	
	if(!all.caren)
	{
		if(preds.format=='class')
			# gets column 2 of preds
			#sub(paste(class,'=',sep=''),'',as.character(read.csv('caren_temp.bas.pre',header=TRUE,sep=';')[,2]))
			read.csv('caren_temp.bas.pre',header=TRUE,sep=';')
		else 
		{
			probs <- read.csv('caren_temp.bas.pre',header=TRUE,sep=';')[,-1]
			preds <- as.factor(apply(probs,1,which.max))
			levels(preds) <- sub(paste(class,'.',sep=''),'',colnames(probs))
			as.character(preds)
		}
	}
}


# caren(Train:basket.data|string, min.sup:percent, min.conf:percent, Options:string, file:boolean) -> rule.set
caren <-function(Train, min.sup=0.02, min.conf=0.1, null='?',imp=0.0, Options="",  file=FALSE, prm=FALSE, 
		all.caren=FALSE, chi=FALSE,X2=FALSE,
		class=NULL,H=NULL,A=NULL,lift=NULL,conv=NULL,Srik=NULL,K=NULL,Ms=NULL,bonf=FALSE,
		Dist=FALSE,G=tail(colnames(Train,1)),Alpha=0.05,disc='none',POI=NA,ks=TRUE,cvm=FALSE,Ss=FALSE,
		Bas=FALSE,
		Xmx="1000M") 
{
	# practically disables scientific notation because of parameter passing to caren
	options(scipen=100)
	# file or data.frame ?
	if(file==TRUE) File<-Train
	else 
	{
		#file.remove('caren_temp.bas',showWarnings=FALSE)
		if ( file.exists("caren_temp.bas") ){
			file.remove("caren_temp.bas")
		}
		save.data(Train,'caren_temp.bas',col.names=!Bas)
		File<-'caren_temp.bas'
		
	}
	# Basics
	if(imp==0) imp.option <- "" 
	else imp.option <- paste('-imp',imp,sep='')
	if(Bas) bas.option <- "-Bas" else bas.option<- "-Att"
	if(is.null(H)) H.option <- "" 
	else H.option <- paste('-H',paste(H,collapse=","),sep='')
	if(is.null(A)) A.option <- "" 
	else A.option <- paste('+A',paste(A,collapse=","),sep='')
	if(is.null(lift)) lift.option <- "" 
	else lift.option <- paste('-lift',lift,sep='')
	if(is.null(conv)) conv.option <- "" 
	else conv.option <- paste('-conv',conv,sep='')
	if(is.null(Srik)) Srik.option <- "" 
	else 
	{
		Srik.option <- paste('-Srik',Srik,sep='')
		#          version.option <- 'carenclass'
	}
	if(is.null(K)) K.option <- "" 
	else K.option <- paste('-K',K,sep='')
	if(is.null(Ms)) Ms.option <- "" 
	else Ms.option <- paste('-Ms',Ms,sep='')
	if(all.caren) output.option <- "-pRulesTemp -ocsRulesTemp"
	else if(prm) output.option <- "-pRulesTemp" else if(Dist) output.option <- "-ocsRulesTemp2" else output.option <- "-ocsRulesTemp"
	if(chi) chi.option <- " -chi" 
	else chi.option <- ""
	if(X2) X2.option <- " -X2" 
	else X2.option <- ""
	if(bonf) bonf.option <- " -bonf" 
	else bonf.option <- ""
	null.option<-paste(" -null",null,sep='')
	version.option='caren'
	
	# classification specifics
	if(!is.null(class)) class.options <- paste(' -class',class,' -H',class,' -pRulesTemp',sep='') 
	else class.options <- ""
	# settings for distribution rules
	if(Dist) 
	{
		dist.option <- " -Dist "
		G.option <- paste(" -G",POI,sep='')
		alpha.option <- paste(" -Alpha",Alpha,sep='')
		if(cvm) fittest.option <-" -cvm" 
		else if(ks) fittest.option <-" -ks" 
		else fittest.option <-""
		if(Ss) Ss.option <- " -Ss" else Ss.option<- ""
		dist.rules.options <- paste(dist.option,G.option,alpha.option,
				fittest.option,Ss.option)
	}
	else dist.rules.options <- ""
	
	# discretization options (needs work)
	if(disc=='none') disc.option <- ""
	else
	{
		num.attrs <- ""
		# n\uffffo discretiza o \uffffltimo: deveria ser mais geral
		for(i in colnames(Train)[1:(ncol(Train)-1)])
			if(class(Train[,i])=='integer' | class(Train[,i])=='numeric') num.attrs <- paste(num.attrs,i,sep=',')
		if(num.attrs=="") disc.option <- ""
		else
		if(disc=='bin') disc.option <- paste(' -bin', num.attrs, sep='')
		else if(disc=='Srik') disc.option <- paste(paste(' -Srik', num.attrs, sep=''),' -K3 -Ms0.5')
		else if(disc=='Srik.K1.5') disc.option <- paste(paste(' -Srik', num.attrs, sep=''),' -K1.5 -Ms0.5')
		else if(disc=='Srik.K3.Md') disc.option <- paste(paste(' -Srik', num.attrs, sep=''),' -K3 ')
		else if(disc=='Srik.K6.Md') disc.option <- paste(paste(' -Srik', num.attrs, sep=''),' -K6 ')
		else if(disc=='cin') disc.option <- paste(' -cin', num.attrs, sep='')
	}
	
	#java options
	java.option<-paste(" -Xmx",Xmx,sep='')
	# build caren command (now with hack to detour bug in carendf (does not run with -Att option)
	# classpath is now GLOBAL VAR
	# classpath<-paste('-classpath ',.libPaths(),'/','carenR/exec ',sep='')
	# classpath<-paste('-classpath "',paste(.libPaths(),'/','carenR/exec ',sep='',collapse=";"),'"')
	# classpath<-paste('-classpath "',paste(c(.libPaths(),'.'),'/','carenR/exec ',sep='',collapse=";"),'"')
	# classpath<-paste('-classpath "', paste(c(.libPaths(),'.'),'/','carenR/exec ',sep='',collapse='";"'),'"')
	# classpath<-paste('-classpath "',paste(c(.libPaths(),'.'),'/','carenR/exec ',sep='',collapse=";"),'"')

	#TODO the Xmx option is hardwired. Should be controlable by user or dynamic.
	if(R.version$os=='mingw32') cp.sep<-'";"' else cp.sep<-'":"'
	classpath<-paste("-classpath ",paste(c(.libPaths(),'.'),'/','carenR/exec',sep='',collapse=cp.sep),' ',sep='"')
	caren.command <- paste('/usr/lib/jvm/java-7-openjdk-amd64/jre/bin/java ',java.option,classpath,version.option, File,min.sup, min.conf,imp.option,
			H.option, A.option, lift.option, conv.option, Srik.option, K.option, Ms.option,
			null.option,bonf.option,
			dist.rules.options,
			class.options,
			output.option, chi.option, X2.option, Options, bas.option, '-s, ')
	cat('\n',caren.command,'\n')                       
	write.to.aux(caren.command)
	#	system('rm RulesTemp.csv')
	#	system('rm RulesTemp.prm')
	#	my.shell("sync")
	k<-my.shell(caren.command)
	#cat(k)
	if(all.caren) "All"
	else if(prm) "RulesTemp"
	else if(!Dist) load.rules('RulesTemp.csv')
	else load.rules.dr('RulesTemp2.csv')
	
	
}

		 
# chamada para produzir dist rules
# carenclass auto-mpg.csv.data 0.05 1 -Att -s; -Gmpg -Dist -d -cindisp,hp,weight,acc -null? -Alpha0.2 -ocsautorules


# caren.att(Train:attrvalue.data|string, min.sup:percent, min.conf:percent, Options:string, file:string) -> rule.set
caren.att <- function(Train, min.sup=0.02, min.conf=0.1, imp=0, Options="", file=TRUE,prm=FALSE, disc='none',...) 
		{
			caren(Train, min.sup, min.conf, imp, Options=paste(" -Att -null?",Options), file=file, prm=prm, disc=disc,...)
		}


##################################################################
# Pretty print of an AR set
# output rules to console.
# ar.pp(R:rule.set) -> (output to console)
##################################################################
#TODO ar.pp help file
#TODO make ar.pp more configurable so that one can choose the measures to go within brackets
ar.pp <- function(R) 
	{
		if(class(R)=='data.frame') 
			{
				nr<-nrow(R)
				for(i in 1:nr) 
					{
						cat('(',R$Sup[i],',',R$Conf[i],')',as.vector(R$Cons[i]),'<- ')
						cat(unlist(R$Ant[i]),sep=' & ')
						cat('\n')
					}
			}
		else cat('No rules to print! \n')
	}


##################################################################
# ar.pp.dev(R:rule.set) -> data.frame
##################################################################

ar.pp.dev <- function(R) 
	{
		rules<-NULL
		nr<-nrow(R)
		for(i in 1:nr) rules<-c(rules, paste(paste(unlist(R$Ant[i]),collapse=' & '),' -> ',R$Cons[i]))
		#    paste(R$cons[i],'<- ',paste(unlist(R$ant[i]),collapse=' & '))
      			
		data.frame(rules=rules,sup=R$Sup,conf=R$Conf,lift=R$Lift,conv=R$Conv,chi2=R$Chi2)
	}



###################################################
#
# Distribution rules
# January 2006
#
###################################################

# load.rules.dr(File:string) -> dist.rule.set
# File contains a set of distribution rules in csv format as produced by caren

# hacked in August07 in S. Carlos to accommodate possible bug in caren

load.rules.dr <- function(File,long=TRUE) 
	{
		Rules<-read.csv(File,sep=";",header=TRUE,strip.white=TRUE)
		Rules$Subgroup<-strsplit(as.character(Rules$Subgroup),"  &  ")
		Rules
	}

# plot.drs(drs: dist.rule.set, apr:default dist.rule, n: height of plot matrix)
#
# drs: dataframe with dist rules
# st: the index of the first rule to plot. Example: if drs has 20 rules we can plot rules 10 to 19 by setting st=10 and n=3
# apr: the default rule (as a dataframe line)
# n: number of plots per line/column of the graphics window
# xlim:  2 element vector with lower and upper limits of the xx axis. Must be defined.
# cex: character expansion coefficient
# iterate: FALSE means you start with st. TRUE means you continue from after the last shown plot.
# ... : other parameters for plot
#
# Example:
#    ds<-load.rules.dr('busrules2.csv')
#    plot.drs(ds,st=1,xlim=c(2000,8000),n=3,n.int=10)
#    plot.drs(ds,st=10,xlim=c(2000,8000),n=3,n.int=10)
#


plot.drs<-function(drs,st=1,apr=drs[drs$Ant_sup==1,],n=3,m=n,xlim=NA,ylim=NA,
		cex=NA,iterate=FALSE,n.int=10,sg.cex=1,st.cex=1,dist.rep='density',...) 
	{
  		# size of characters is a function of n
  		if(is.na(cex)) cex=1-0.2*(max(n,m)-1)
  		# calculate xlim and step from reference distribution if one is given
		# or from rule 1, otherwise
  		if(nrow(apr)!=0) dist<-as.vector.dist(apr$Dist)
		else dist<-as.vector.dist(drs[1,]$Dist)
		poi <- unlist(strsplit(as.character(drs$Dist[1]),"="))[1]
  		if(length(xlim)==1)
			{
    				xmin <- min(dist)
    				xmax <- max(dist)
    				xlim <- c(xmin,xmax)
  			}
  		else
			{
    				xmin <- xlim[1]
    				xmax <- xlim[2]
  			}
  		step <- (xmax-xmin)/n.int
  		# calculate ylim
  		if(length(ylim)==1) ylim <- c(0,1/step)
  		# prepare for iterative sessions
  		if(iterate)
			{
    				st <- last.shown+1
    				last.shown <<- last.shown+n*m
  			}
  		else last.shown <<- st+n*m-1
		# check if there are more rules to show
		if(st>nrow(drs)) cat("[CarenR]:No more rules to show.\n")
		else
			{
  			# start drawing
  			close.screen(all.screens=TRUE)
  			split.screen(c(n,m))
  			for(i in (1:min(n*m,nrow(drs)-st+1)))
				{
    					# start plotting by drs[st,]
    					j <- i+st-1
    					screen(i)
						if(m==1) {
							par(mar=c(1,1,3,1)+0.5)
							par(mgp=c(3,0.5,0))
						} 
						else {
							par(mar=c(1,1,1,1)+0.5)
							par(mgp=c(3,0.5,0))
						}
						#plot.dr(apr,col='grey',cex.axis=1,xlim=xlim,ylim=ylim,n.int=n.int,
						#		subplot=TRUE,...)
    					#par(new=TRUE)
    					plot.dr(drs[j,],apr=apr,cex.axis=1,xlim=xlim,ylim=ylim,n.int=n.int,
								subplot=TRUE,dist.rep=dist.rep,...)
    					rule.text <- paste(unlist(drs$Subgroup[j]),collapse=', ')
    					#cexex <- min(1, 1.05*(xlim[2]-xlim[1])/strwidth(rule.text,cex=cex))
    					#attempt to present long antecedent descriptions
    					ant.vec <- unlist(drs$Subgroup[j])
    					if(length(ant.vec)<=2) 
						{
      							if(length(ant.vec)==0) ant.text <- "whole population"
      							else ant.text <- paste(ant.vec,collapse=', ')
      							mtext(paste(ant.text,collapse=', '),line=0,cex=cex*sg.cex,adj=0,col="darkred")
    						}
    					else 
						{
      							mtext(paste(ant.vec[1:2],collapse=', '),line=1*cex*sg.cex,cex=cex*sg.cex,adj=0,col="darkred")
      							mtext(paste(ant.vec[3:length(ant.vec)],collapse=', '),line=0,cex=cex*sg.cex,adj=0,col="darkred")
    						}
    
   	 				#ant.text <- paste(unlist(drs$Subgroup[j]),collapse=', ')
    					#if(ant.text=="") ant.text <- "whole population"
    					#mtext(ant.text,line=0.25,cex=cex,adj=0,col="darkred")
    					if(length(xlim)!=0)
						{
      							text(xlim[1]-step/2,0.95/step,paste("KS.int=1-",sprintf("%0.2g",drs$pvalue[j]),"Sup=",sprintf("%0.4f",drs$Ant_sup[j])),cex=cex*st.cex,pos=4,col="darkblue")
      							text(xlim[1]-step/2,0.88/step,paste("Mean=",sprintf("%0.3f",drs$Mean[j]),"St.Dev=",sprintf("%0.3f",drs$Stdev[j])),cex=cex*st.cex,pos=4,col="darkblue")
      							text(xlim[1]-step/2,0.7/step,paste("P.O.I:",poi),cex=cex*st.cex,pos=4)
    						}
  				}
		}
	}





# plot.dr(dr:dist.rule, n.int: number of intervals for drawing an hist like plot, plot parameters)
# this function draws a single hist like plot and is used by plot.drs
# it is now data set independent (I guess)
#
# dr: one distribution rule
# n.int: number of intervals for plotting
# col: color of line plotted
# xlim: 2 element vector with lower and upper limits of the xx axis
# ylim: likewise for the yy axis
# ... : other parameters for plot

#TODO ylim is limited to 0-1
plot.dr <- function(dr,apr=NULL,n.int=10,col='black',xlim=NA,ylim=c(0,1),subplot=FALSE,
		dist.rep="density",...){
	if(nrow(dr)==0) return()
	ant<-paste(unlist(dr$Subgroup),collapse=' & ')
	dist<-as.vector.dist(dr$Dist)
	if(!is.null(apr)) apr.dist <- as.vector.dist(apr$Dist)
	if(all(is.na(xlim))){
		xmin <- min(dist)
		xmax <- max(dist)
		xlim <- c(xmin,xmax)
	}
	else{
		xmin <- xlim[1]
		xmax <- xlim[2]
	}
	step <- (xmax-xmin)/n.int
	breaks <- seq(xmin,xmax,step)
	labels <- round(seq(xmin+step,xmax,step),2)
	totfreq<-length(dist)
	if(!is.null(apr)) totfreq.apr<-length(apr.dist)
	if(subplot==FALSE) close.screen(all.screens=TRUE)
	if(dist.rep=="boxplot")	{
		if(!is.null(apr))
			boxplot(apr.dist,dist,border=c("grey","black"),
					col=c("white","yellow"),
					horizontal=TRUE,
					main="",type='l',ylab="",xlim=ylim,ylim=xlim,
					boxwex=0.05,
					at=c(ylim[1]+(ylim[2]-ylim[1])*(2/12),
						ylim[1]+(ylim[2]-ylim[1])*(6/12)),
					...)
		else boxplot(dist,border=c("black"),
					col=c("yellow"),
					horizontal=TRUE,
					main="",type='l',ylab="",
					...)
	}
	else {
		if(dist.rep=="polygon") {
			#main=as.character(ant),xlab=strsplit(as.character(dr$Dist[1]),"={")[[1]][1],
			to.plot <- table(cut(dist,breaks=breaks,labels=labels))/(totfreq*step)
			if(!is.null(apr)) to.plot.apr <- table(cut(apr.dist,breaks=breaks,labels=labels))/(totfreq.apr*step)
		}
		else if(dist.rep=="density") {	
			to.plot <- density(dist)
			if(!is.null(apr)) to.plot.apr <- density(apr.dist)
		}
		if(!is.null(apr)) {
			plot(to.plot.apr,main="",type="l",col="grey",xlim=xlim,ylim=ylim,...)
			par(new=TRUE)
		}
#TODO plot.dr(drs[1,], drs[drs$Ant_sup==1,], dist.rep="density") -- messed up xlab 
#  plot(table(cut(dist,main=as.character(ant),xlab=strsplit(as.character(dr$Dist[1]),"={")[[1]][1],breaks=breaks,labels=labels))/(totfreq*step),
#  plot(table(cut(dist,main=as.character(ant),xlab=strsplit(as.character(dr$Dist[1]),"={")[[1]][1],breaks=breaks,labels=labels))/totfreq,
		plot(to.plot,main="",type='l',col=col,ylab="",xlim=xlim,ylim=ylim,...)
#       ylim=ylim,...
	}
}




# as.vector.dist(x: caren.dist) -> original sample
# converts a distribution string into a vector with the original sample
# motivation: using R's ks
as.vector.dist <- function(x)
{ 
	untable(as.matrix.dist(x))
}


# untable(dist matrix: matrix representing a distribution) -> vector with sample
# inverse function of table()
untable <- function(m) 
	{
  		v<-NULL
  		for(i in 1:nrow(m))
    		v <- c(v,rep(m[i,1],m[i,2]))
  		v
	}

# as.matrix.dist(dist:caren.dist:string from caren of the form ATTR = {x1/f1, ..., xn/fn}) -> matrix with two columns x and f
# converts a string with a distribution (caren output format) into a matrix with the same information
as.matrix.dist <- function(dist2)
	{
  		x<-strsplit(strsplit(as.character(dist2),"{ ",fixed=TRUE)[[1]][2]," }",fixed=TRUE)[[1]][1]
  		y<-strsplit(x,",")[[1]]
  		a<-as.numeric(unlist(strsplit(y,"/")))
  		matrix(a,length(a)/2,2,byrow=TRUE)
	}

# pp.dr(R:dist.rule.set,lim:display width in characters) -> (output to console)
# pretty print distribtution rules 
#TODO fix pp.dr and ar.pp names to become print
	pp.dr <- function(R,lim=80) {
		if(class(R)=='data.frame') {
			nr<-nrow(R)
			for(i in 1:nr) {
				cat(sprintf("Sup=%.3f KS.int=1-%.2g Mean=%.3f St.Dev=%.3f\n",R$Ant_sup[i],R$pvalue[i],R$Mean[i],R$Stdev[i]))
#                  cat('(',R$Ant_sup[i],',',R$ks[i],') ')
				cat(unlist(R$Subgroup[i]),sep=' & ')
				cutcat(paste('\n  -> ',as.vector(R$Dist[i])),lim=lim,sep=',',indent=12)
				cat('\n')
			}
		}
		else cat('No rules to print! \n')
	}
	
	# pretty prints in csv format
	pp2csv.dr <- function(R) {
		if(class(R)=='data.frame') {
			nr<-nrow(R)
			for(i in 1:nr) {
				cat('(',R$Ant_sup[i],',',R$pvalue[i],')')
				cat(unlist(R$Subgroup[i]),sep=' & ')
				cat('->',as.vector(R$Dist[i]))
				cat('\n')
			}
		}
		else cat('No rules to print! \n')
	}

	# auxiliary to pp.dr. This function indents and breaks a long string into lines.
	cutcat <- function(s,lim=80,sep=',',indent=11) {
		to.indent<-FALSE
		while(nchar(s)>lim){
			line<-""
			while(nchar(line)<lim){
				line <- paste(line,unlist(strsplit(s,sep))[1],sep,sep="")
				s <- paste(unlist(strsplit(s,sep))[-1],collapse=sep)
			}
			if(to.indent) cat(paste(rep(' ',indent),collapse=''),line,'\n',sep='')
			else cat(line,'\n',sep='')
			if(!to.indent) lim <- lim-indent
			to.indent<-TRUE
		}
		if(to.indent) cat(paste(rep(' ',indent),collapse=''),s,'\n',sep='')
		else cat(s,'\n',sep='')
		
	}

	

##################################################
#
# Association Rules
#
##################################################

load.rules <- function(File,long=TRUE,Dist)
        {
                Rules<-read.csv(File,sep=";",header=TRUE,strip.white=TRUE)
                #if(long) names(Rules) <- c('sup','conf','lift','conv','chi2','lapl','leve','jacc','cosi','phi','MI','cons','ant')
                #else names(Rules) <- c('sup','conf','cons','ant')
                Rules$Ant<-strsplit(as.character(Rules$Ant),"  &  ")
                Rules
        }


########################
#
# Associated Utilities
#
########################

my.shell <- function(Comm,...)
	{
		 if(R.version$os=='mingw32') { 
  
			shell(Comm,translate=TRUE,invisible=TRUE,intern=FALSE,mustWork = TRUE) 
		}
		 else system(Comm)
	}
write.to.aux <- function(s) 
	{
        	write(paste(date(),'>',s),file='class_exp.aux',append=TRUE)
	}
###############################
#
#Save Data
#
###############################
save.data <- function(dataframe,file,col.names=TRUE) 
{
	write.table(dataframe,file,row.names=FALSE,col.names=col.names,quote=FALSE,sep=',')
}
