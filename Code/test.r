###############################################################
# Purpose: Making survival and cumulative incidence plots
# Author: Pingping Qu
# Date: 3-27-2017
###############################################################

rm(list=ls())

cdir="J:/g1/Dev/R/Survplot/Code"
ddir="J:/g1/Dev/R/Survplot/Data"
wdir="J:/g1/Dev/R/Survplot/Output"

setwd(cdir)
source('survplot.R')
source('cinplot.r')
library(survival)
library(cmprsk)

setwd(ddir)
train.clin=read.csv("survdata.csv", header=T, sep=",")
colnames(train.clin)

linecols=c('gray70', 'gray30')
linetype=c(1,1)
linecols2=c('gray70', 'gray30', 'gray70', 'gray30')
linetype2=c(3,3,1,1)
linewd=1

# timeunits="day"; desiredunits="year"; svar="PFS";
# stime=train.clin[,'efstim']
# sind=train.clin[,'efsind']
# group=as.factor(train.clin[,"score2.median"]); table(group)
# time.est=5
# desiredxint=2; xminorticmark=T
# cexticmark=.5; cexaxis=.8; cexlab=0.8; cexmain=0.9
# par.mgp=c(2, 0.5, 0); par.mar=c(2.5, 3, 1.5, 1)
# linecol=linecols; linety=linetype; linewd=linewd
# xlabel="Years from Therapy Starting Date"; ylabel="Progression-free Survival"
# 
# leg=T; showevents=T; showyearest=T;
# legtext=paste(c("Score <", "Score >="), round(train.clin[1, 'cutoff.median'],2))
# legloc=c(100, .24); legcex=.7
# pvalue=T; pvalue.belowlegend=T; pvalueloc=c(1.1*370, .40); pvaluecex=.7

setwd(wdir)
win.metafile("2-groups.wmf", width=4, height=3)
survplot(svar="PFS", stime=train.clin[,'efstim'], 
              sind=train.clin[,'efsind'], 
              group=as.factor(train.clin[,"score2.median"]), time.est=5,
              desiredxint = 2,
              linecol=linecols, linety=linetype, linewd=linewd,  
              mtitle="", xlabel="Years from Therapy Starting Date", 
              ylabel="Progression-free Survival", 
              legloc=c(100, .24), 
              legtext=paste(c("Score <", "Score >="), 
                            round(train.clin[1, 'cutoff.median'],2)), 
              pvalue=T, pvalue.belowlegend=T, pvalueloc=c(700, .40))
dev.off()


##combine with baseline GEP70 risk

combo=train.clin$combo.g70high.score2
table(combo, useNA="always")

# legtext=c("Low / Low", "Low / High", "High / Low", "High / High")
# legloc=c(10, .35)
# group=as.factor(combo)
# linecol=linecols2; linety=linetype2;

win.metafile("4-groups.wmf", width=4, height=3)
survplot(svar="PFS", stime=train.clin[,'efstim'], 
              sind=train.clin[,'efsind'], 
              group=as.factor(combo), time.est=5, 
              linecol=linecols2, linety=linetype2, linewd=linewd,  
              mtitle="", xlabel="Years from Therapy Starting Date", 
              ylabel="Progression-free Survival", 
              legtext=c("Low / Low", "Low / High", "High / Low", "High / High"), 
              legloc=c(10, .35),
              pvalue=T, pvalue.belowlegend=T)
dev.off()


### one group has 0 samples in it

combo=as.factor(train.clin$combo.g70high.score2)
table(combo)
levels(combo)

# legtext=c("Low / Low", "Low / High", "High / Low", "High / High")
# legloc=c(10, .35)
# linecol=linecols2; linety=linetype2;
# data=train.clin[combo!=3,]
# stime=train.clin[combo!=3,'efstim']
# sind=train.clin[combo!=3,'efsind'] 
# group=combo[combo!=3]
# table(group)
# levels(group)

win.metafile("one-group-has-0-samples.wmf", width=4, height=3)
survplot(svar="PFS", 
         stime=train.clin[combo!=3,'efstim'], 
         sind=train.clin[combo!=3,'efsind'], 
         group=combo[combo!=3], time.est=5, 
         linecol=linecols2, linety=linetype2, linewd=linewd,  
         mtitle="", xlabel="Years from Therapy Starting Date", 
         ylabel="Progression-free Survival", 
         legtext=c("Low / Low", "Low / High", "High / Low", "High / High"), 
         legloc=c(10, .31),
         pvalue=F, pvalue.belowlegend=F, pvalueloc=c(10*365.25, 0.85))
dev.off()

win.metafile("no-events-no-estimates-shown.wmf", width=4, height=3)
survplot(svar="PFS", 
         stime=train.clin[combo!=3,'efstim'], 
         sind=train.clin[combo!=3,'efsind'], 
         group=combo[combo!=3], time.est=5, 
         linecol=linecols2, linety=linetype2, linewd=linewd,  
         mtitle="", xlabel="Years from Therapy Starting Date", 
         ylabel="Progression-free Survival", 
         legtext=c("Low / Low", "Low / High", "High / Low", "High / High"), 
         legloc=c(10, .31), showevents=F, showyearest=F,
         pvalue=F, pvalue.belowlegend=F, pvalueloc=c(7*365.25, 0.05))
dev.off()


win.metafile("legend-below-figure.wmf", width=4, height=4.2)
survplot(svar="PFS", stime=train.clin[,'efstim'], 
         sind=train.clin[,'efsind'], 
         group=as.factor(combo), time.est=5, 
         linecol=linecols2, linety=linetype2, linewd=linewd,  
         mtitle="", xlabel="Years from Therapy Starting Date", 
         ylabel="Progression-free Survival", 
         legtext=c("Low / Low", "Low / High", "High / Low", "High / High"), 
         legloc=c(10, -0.35), legpos="below",
         pvalue=T, pvalue.belowlegend=T)
dev.off()

win.metafile("legend-to-the-right-of-figure.wmf", width=6.5, height=3)
survplot(svar="PFS", stime=train.clin[,'efstim'], 
         sind=train.clin[,'efsind'],
         group=as.factor(combo), time.est=5, 
         linecol=linecols2, linety=linetype2, linewd=linewd,  
         mtitle="", xlabel="Years from Therapy Starting Date", 
         ylabel="Progression-free Survival", 
         legtext=c("Low / Low", "Low / High", "High / Low", "High / High"), 
         legloc=c(12.5*365.25, 0.40), legpos="right",
         pvalue=T, pvalue.belowlegend=T)
dev.off()

setwd(ddir)
clin0=read.csv("cidata.csv", header=T, sep=",")
colnames(clin0)
table(clin0$basedz)
clin=clin0[clin0$basedz=="AMM", ]

clin$time2mmprog
clin$mmprogind
clin$cmpmmprog
cmpmmprog=clin$cmpmmprog
cmpmmprog[cmpmmprog==1]=2
clin$mmprogstatus=clin$mmprogind+cmpmmprog
cbind(clin$mmprogind, clin$cmpmmprog, clin$mmprogstatus)

# data=clin; timeunits="year" 
# stime=clin$time2mmprog/365.25; status=clin$mmprogstatus; group=clin$score2; year.est=5;
# legtext=c("Score < -2.47", "Score >= -2.47")
# axistouch=T 
# par.mgp=c(2, 0.5, 0); par.mar=c(2.5, 3, 1.5, 1)
# tickmark=T; cexticmark=.5; cexaxis=.8; cexlab=0.8; cexmain=0.9
# linecol=c('gray70', 'gray30'); linety=c(1,1);linewd=2  
# mtitle=""; xlabel="Years from Enrollment"; ylabel="Probability" 
# leg=T; showevents=T; showyearest=T
# legloc=c(1, .9); legcex=.7
# legdist=c(.96, 1.6); 
# pvalue=T; pvalue.belowlegend=T; pvalueloc=c(1.1*370, .40); pvaluecex=.7
# filesize=c(4, 3)

setwd(wdir)
win.metafile("cumulative-incidence.wmf", width=4, height=3)
cinplot(timeunits="year", 
        stime=clin$time2mmprog/365.25, status=clin$mmprogstatus, 
        group=clin$score2, year.est=5,
        legtext=c("Score < -2.47", "Score >= -2.47"),
        linecol=c('gray70', 'gray30'), linety=c(1,1), linewd=2,  
        xlabel="Years from Enrollment", 
        legloc=c(1, .9))
dev.off()




