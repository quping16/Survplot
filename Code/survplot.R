
survplot=function(svar="OS", stime=data[,"surtim"], sind=data[,"surind"], 
	group=data[,"rhierclus2"], 
	timeunits="day", desiredunits="year", time.est=1, 
	desiredxint=2, xminorticmark=T,
  par.mgp=c(2, 0.5, 0), par.mar=c(2.5, 3, 1.5, 1),
	cexticmark=.5, cexaxis=.8, cexlab=0.8, cexmain=0.9, legcex=.7, pvaluecex=.7,
	linecol=c("green", "red"), linety=c(1,1), linewd=2,  
	mtitle="Overall Survival", xlabel=desiredunits, ylabel="Overall Survival", 
	leg=T, legpos="inside", showevents=T, showyearest=T, 
	legtext=c("Low Risk", "High Risk"), legloc=c(100, .26),  
	pvalue=T, pvalue.belowlegend=T, pvalueloc=c(500, .40))
{

  if (!is.factor(group)) 
  {
    stop("\nThe group variable needs to be entered as a factor!\n")
  }

  # Survival times may come in days, months or years, and we may want to
  # convert it to say months or years to show in the x-axis
  if (timeunits==desiredunits) tuni=1 else
  if (timeunits=="day" & desiredunits=="year") tuni=365.25 else 
    if (timeunits=="day" & desiredunits=="month") tuni=365.25/12 else 
      if (timeunits=="month" & desiredunits=="year") tuni=12
  
  # every what years/months do you want to mark the x-axis?
  # desiredxint=2 tells every 2 years we want to have a major mark on the x-axis
  
  ###################################
  ## basic calculations
  ###################################
  
  #number of samples and events in each group, note that some levels may 
  #have 0 observations in it, but will still be in the legend
  ngroups=nlevels(group); ngroups
  nsamples=table(group); nsamples
  nevents=table(group, sind)[,2]; nevents  

  # determine the maximum units in x-axis  
  maxx=max(stime%/%tuni, na.rm=T)+1; maxx
  if (maxx%%2==1) maxx=maxx+1 
  #make sure maxx is an even number

  # log-rank test p value
  ltest=survdiff(Surv(stime, sind)~group)
  print(ltest)
  pvalue2=1-pchisq(ltest$chisq, df=sum(ngroups)-1); pvalue2
  
  ## get n.risk, n.event, survival estimate at each time point
  fit = survfit(Surv(stime, sind)~group); fit
  summary(fit)
  
  ## Maybe you want to estimate 5-year survival but the data can only do 3-year
  ## get the maximum year/month estimate the data can do
  time.est2=min(tapply(stime, group, max), na.rm=T)%/%tuni; 
  time.est2  
	##### need to find the maximum stime in each group, then take the minimum of that
  if (time.est2<time.est) time.est=time.est2
  yest=summary(fit, times=time.est*tuni)
  yest

###############################
##  plotting the KM curves
###############################

  #par.mgp=c(2, 0.5, 0), par.mar=c(2.5, 3, 1.5, 1),
  
  if (legpos=="inside") par(xpd=T, mar=par.mar, mgp=par.mgp)
  else if (legpos=="below") par(xpd=T, mar=par.mar+c(6, 0, 0, 0), mgp=par.mgp)
  else if (legpos=="right") par(xpd=T, mar=par.mar+c(0, 0, 0, 14), mgp=par.mgp)
  #xpd=T makes legend go a little beyond the x-axis limit
    
## make sure to use the colors corresponding to categories that 
## have observations in them (those categories with 0 observations 
## won't be plotted)
linecol2=linecol[(nsamples!=0)]; linecol2
linety2=linety[(nsamples!=0)]; linety2

## only the curves, no axes yet
plot(fit,col=linecol2, 
  mark=3,           # tickmark type 
	cex=cexticmark,		# size of the tic marks
	lwd=linewd, 	
	lty=linety2,
	axes=F, 		## no axes are drawn 
	font=1, 		## font of main title 
	xaxs="i",		## this makes the axes contact each other
	yaxs="i", 	## this makes the axes contact each other
	xlim=c(0, maxx*tuni))

## add x-axis, with major tic marks put in desired places
axis(1,tck=-.02,
	cex.axis=cexaxis,  ## size of the x-axis
	at=seq(0, maxx*tuni, by=desiredxint*tuni), 	
	labels=paste(seq(0,maxx,by=desiredxint)), 	 
	font=1, 				## font of the axis labels: 1=regular, 2=boldface, 3=italic
	padj=-.7			## perpendicular distance from axis labels to tick marks,
                ## the more negative, the more towards the tick marks  
	)				 

## add x-axis minor tic marks if xminorticmark is true
if (xminorticmark) rug(x = seq(0, maxx*tuni, by=tuni), ticksize = -0.015, side = 1)

## add y-axis
axis(2,tck=-.02, 
	cex.axis=cexaxis,
	at=seq(0,1,by=.2), labels=c("", paste(seq(20,100,by=20), "%", sep="")), 
	las=1, 	## makes the axis labels perpendicular to the tick marks
	font=1, 	## font of axis labels
	#hadj=.7	## distance from axis labels to the tick marks
	hadj=0.8	## the bigger, the more away from the tick marks
	)
## add minor y-axis tic marks
rug(x = seq(0.1, 0.9, by=0.2), ticksize = -0.015, side = 2)


###########################
## add titles and labels
###########################

#par(family=fontfamily)
title(main=mtitle, line=0.8, cex.main=cexmain)

title(xlab=xlabel, line=par.mgp[1]-0.9, cex.lab=cexlab)
title(ylab=ylabel, cex.lab=cexlab,
	line=par.mgp[1],  		# distance of ylab from the axis line 
	#font.lab=1           # font of xlab or ylab
	)


#####################
## add legends
#####################

if (leg==T)
{

## add simple legends first
  
legend(legloc[1], legloc[2], legtext,
	lty=linety, 
	col=linecol, 
	lwd=linewd, 	## control the line width of the legend
	bty="n", 	## no box around legend is drawn
	cex=legcex)		## increase the text size by 1.2 times than default

## lbox has various coordinates related to the legend
lbox=legend(legloc[1], legloc[2], legtext,
	lty=linety, 
	col=linecol, 
	lwd=linewd, 	## control the line width of the legend
	bty="n", 	## no box around legend is drawn
	cex=legcex)		## increase the text size by 1.2 times than default

## add number of events or deaths

if (showevents==T)   
{
  pos.x=lbox$rect$left+lbox$rect$w+(lbox$text$x[1]-lbox$rect$left)
  
  if (svar=="OS") tmp="Deaths / N" else 
    if (svar=="PFS") tmp="Events / N"
  text(pos.x, lbox$rect$top, tmp, cex=legcex, font=2) 
  
  count=0
  for (i in 1:ngroups)
  {
    if (nsamples[i]!=0)
    {
    count=count+1
    text(pos.x, lbox$text$y[i], 
     paste(ltest$obs[count], "/", ltest$n[count], sep=""), cex=legcex, font=1)

    } else 
    {
      # When a category has 0 observations in it, we just
      # show 0/0, meaning 0 observations and 0 events
    text(pos.x, lbox$text$y[i], paste(0, "/", 0, sep=""), cex=legcex, font=1)
    }

  } 

} ## the end of showevents


## add, say, 5-year estimates

if (showyearest==T)   
{
  pos.x2=lbox$rect$left+lbox$rect$w+3*(lbox$text$x[1]-lbox$rect$left)
  
  # make the first letter in desiredunits upper case 
  #h=paste(toupper(substr(desiredunits, 1,1)), substr(desiredunits, 2, nchar(desiredunits))) 

  h=desiredunits
  text(pos.x2, lbox$rect$top+0.9*(lbox$rect$top-lbox$text$y[1]), 
     paste(time.est, h, sep="-"), cex=legcex, font=2)
  text(pos.x2, lbox$rect$top, paste("Estimate"), cex=legcex, font=2)

count=0
for (i in 1:ngroups)
{

    if (nsamples[i]!=0)
    {
    count=count+1
    if ( !is.na(yest$surv[count]))
    {
      tmp=paste(round(yest$surv[count]*100, 0), 
            "% (", round(yest$lower[count]*100,0), ", ", 
            round(yest$upper[count]*100,0),")", sep="")
    text(pos.x2, lbox$text$y[i], tmp, cex=legcex, font=1)
    } else {
    text(pos.x2, lbox$text$y[i], "NA", cex=legcex, font=1)
    }

    } else
    {
    text(pos.x2, lbox$text$y[i], "NA", cex=legcex, font=1)
    }

} 

} ### the end of showyearest

} ## the end of drawing legends


#####################
#   add p-value
#####################

if (pvalue)
{
## if you want to make p value bold faced, 
## use font=4 option in the text functions below

if (pvalue.belowlegend==T & showevents==T & showyearest==T)
{
  pos.x3=lbox$rect$left+lbox$rect$w+(lbox$text$x[1]-lbox$rect$left)
  pos.y3=lbox$text$y[i]+0.9*(lbox$text$y[i]-lbox$text$y[i-1])
  
  if (pvalue2<.0001) 
    text(pos.x3, pos.y3, paste("P-value < .0001"), cex=pvaluecex) else 
    text(pos.x3, pos.y3, paste("P-value =", round(pvalue2, 4)), cex=pvaluecex)
}
 
if (pvalue.belowlegend==F)
{
if (pvalue2<.0001) 
  text(pvalueloc[1], pvalueloc[2], paste("P-value < .0001"), 
       cex=pvaluecex, font=3) else 
  text(pvalueloc[1], pvalueloc[2], paste("P-value =", round(pvalue2, 3)), 
       cex=pvaluecex, font=3)
}

} #end of drawing p value

if (dev.cur()>2) dev.off()
#out=data.frame(surv.est=yest$surv, lower=yest$lower, upper=yest$upper)
#out=data.frame(group=yest$strata, n.risk=yest$n.risk, n.event=yest$n.event, round(out,2))

#return(list(sdiffres=res, out, pvalue=pvalue2))


}



