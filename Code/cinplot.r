cinplot=function(timeunits="year", 
                 stime=clin$time2mmprog, status=clin$mmprogstatus, 
                 group=clin$score2, year.est=5,
                 legtext=c("Low Risk", "High Risk"), 
                 linecol=c('gray70', 'gray30'), linety=c(1,1), linewd=2,  
                 mtitle="", xlabel="Years from Enrollment", ylabel="Probability", 
                 legloc=c(100, .26), 
                 axistouch=T, 
                 par.mgp=c(2, 0.5, 0), par.mar=c(2.5, 3, 1.5, 1),
                 tickmark=T, cexticmark=.5, cexaxis=.8, cexlab=0.8, cexmain=0.9,
                 leg=T, showevents=T, showyearest=T, 
                 legcex=.7, 
                 pvalue=T, pvaluecex=.7)
{
  #status takes values 0 (censored), 1 (cause of failure of interest), 2 (competing risk, that is, 
  #other cause of failure that will prevent from the cause of interest)
  
  cat("\nMake sure the status variable is coded as following: \n\n0 = censored\n1 = cause of failure of interest\n2 = other cause of failure\n")
  
  #number of samples and events in each group
  ngroups=length(unique(group)); ngroups
  nsamples=table(group); nsamples
  nevents=table(group, status)[,2]; nevents

  group=factor(group, levels=c(0, 1), labels=legtext); group
  xx=cuminc(ftime=stime, fstatus=clin$mmprogstatus, group=group); xx
  names(xx)

  #calculate estimate at a specific time point and ci by normal approximation
  #is there another package to calculate ci more accurately than normal approximation? 
  yest=timepoints(xx, times=year.est); yest
  ylower=yest$est-qnorm(0.975)*sqrt(yest$var)
  yupper=yest$est+qnorm(0.975)*sqrt(yest$var)
  ylower
  yupper
#   # delta method to calculate ci
#   ylower2=exp(log(yest$est)-qnorm(0.975)*sqrt(yest$var/yest$est^2))
#   yupper2=exp(log(yest$est)+qnorm(0.975)*sqrt(yest$var/yest$est^2))
#   ylower2
#   yupper2
  
  # delete the output for the cause of no interest
  x=xx
  x[[3]]=x[[4]]=NULL
  x[['Tests']]=xx[['Tests']][1,]
  names(x)[1:2]=substr(names(x[1:2]), 1, nchar(names(x[1:2]))-2)
  x
  
  ##### plotting

  par(xpd=T, mar=par.mar, mgp=par.mgp)

  maxx=ceiling(max(stime)); maxx
  
  plot(x=x[[1]]$time, y=x[[1]]$est, col=linecol[1], lty=linety[1], type="l", 
       ylim=c(0,1), xlim=c(0, maxx), xlab="", ylab="", bty="n", xaxs="i", yaxs="i", axes=F)
  points(x=x[[2]]$time, y=x[[2]]$est, col=linecol[2], lty=linety[2], type="l", ylim=c(0,1))

  axis(1,tck=-.02,
     cex.axis=cexaxis,                   ## controls the size of the x-axis
     at=seq(0, maxx, by=2), 	## controls where to label the axis
     labels=paste(seq(0,maxx,by=2)), 	## controls the look of axis labels 
     font=1, 				## controls the font of the axis labels: 1=regular, 2=boldface, 3=italic
     #padj=-.85				## controls the perpendicular distance from axis labels to tick marks
     padj=-.7)				## the more negative, the more towards the tick marks; the more positive, the more away from the tick marks
  rug(x = seq(1, maxx, by=2), ticksize = -0.015, side = 1)
  
  axis(2, tck=-.02, 
     cex.axis=cexaxis,
     at=seq(0,1,by=.2), labels=c("", paste(seq(20,100,by=20), "%", sep="")), 
     las=1, 	## makes the axis labels perpendicular to the tick marks
     font=1, 	## controls the font of axis labels
     #hadj=.7	## controls the distance from axis labels to the tick marks
     hadj=0.8	## the bigger, the more away from the tick marks
     )
  rug(x = seq(0.1, 0.9, by=0.2), ticksize = -0.015, side = 2)
  
  title(main=mtitle, line=0.8, cex.main=cexmain)
  title(xlab=xlabel, line=par.mgp[1]-0.9, cex.lab=cexlab)
  title(ylab=ylabel, cex.lab=cexlab,
        line=par.mgp[1],  		##  control the distance of ylab from the axis line 
        #font.lab=1  ## control the font of xlab or ylab
  )
  
  legend(legloc[1], legloc[2], legtext,
         lty=linety, 
         col=linecol, 
         lwd=linewd,   ## control the line width of the legend
         bty="n", 	## no box around legend is drawn
         cex=legcex)		## increase the text size by 1.2 times than default
  
  res2=legend(legloc[1], legloc[2], legtext,
              lty=linetype, 
              col=linecols, 
              lwd=linewd,   ## control the line width of the legend
              bty="n", 	## no box around legend is drawn
              cex=legcex)		## increase the text size by 1.2 times than default
  
  ## add number of samples and number of events
  pos.x=res2$rect$left+res2$rect$w+(res2$text$x[1]-res2$rect$left)

  text(pos.x, res2$rect$top, "Events / N", cex=legcex, font=1, lwd=linewd) 

  count=0
  for (i in 1:ngroups)
  {
    if (nsamples[i]!=0)
    {
      count=count+1
      text(pos.x, res2$text$y[i], 
           paste(nevents[i], "/", nsamples[i], sep=""), cex=legcex, font=1)
    } else 
    {
      text(pos.x, res2$text$y[i], 
           paste(0, "/", 0, sep=""), cex=legcex, font=1)
    }
  }  

  ## add est for a specific year
  pos.x2=res2$rect$left+res2$rect$w+2.8*(res2$text$x[1]-res2$rect$left)

  text(pos.x2, res2$rect$top, "Estimate", cex=legcex, font=1, lwd=linewd) 
  
  text(pos.x2, res2$rect$top+(res2$rect$top-res2$text$y[1])*0.9, 
       paste(year.est, timeunits, sep="-"), cex=legcex, font=1, lwd=linewd) 

  count=0
  for (i in 1:ngroups)
  {
    if (nsamples[i]!=0)
    {
      count=count+1
      text(pos.x2, res2$text$y[i], 
           paste(round(yest$est[i],2)*100, "% (", round(ylower[i],2)*100, 
                 ", ", round(yupper[i], 2)*100, ")", sep=""), cex=legcex, font=1)
    } else 
    {
      text(pos.x2, res2$text$y[i], 
           paste(0, "/", 0, sep=""), cex=legcex, font=1)
    }
  }  
      
  ## add p value
  
  if (pvalue)
  {
    pos.x3=res2$rect$left+res2$rect$w+(res2$text$x[1]-res2$rect$left)
    
    text(pos.x3, res2$text$y[i]+(res2$text$y[i]-res2$text$y[i-1]), 
         paste("P-value =", round(x[['Tests']][2],4)), cex=legcex, font=1, lwd=linewd) 
    
  }
  cat("\n")

}

