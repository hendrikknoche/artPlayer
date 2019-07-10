library(ggplot2)
library(sqldf)
<<<<<<< HEAD
library(here)
||||||| merged common ancestors
=======
library(here)
library(Rmisc)
>>>>>>> dda7a181fbc30f66062652366da9c35ee28868d6
setwd(paste(here::here("Processed - Kopi","data")))
fdf<-read.csv("fdf.csv")
fdf$episode<-gsub("ArtPlayer", "intervention", fdf$episode)
fdf$episode<-gsub("Debrief", "debrief", fdf$episode)
fdf$episode<-gsub("syringe", "stressor", fdf$episode)
fdf$episode<-gsub("ThreatRemoval", "stressor removal", fdf$episode)
fdf$episode<-factor(fdf$episode,levels=c("baseline","stressor","intervention","stressor removal", "debrief"))

fdfp <- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode"))
fdfBL<-fdf[fdf$episode=="baseline",c("PID","Avg.HR")]
names(fdfBL)[2]="HRatBaseline"
#sqldf("select PID, Avg.HR as HRatBaseline from fdf where ")
fdf<-merge(fdf,fdfBL)
fdf$HRchangeInPercent<-1-fdf$Avg.HR/fdf$HRatBaseline
fdf$HRchangeInBPM<-fdf$Avg.HR-fdf$HRatBaseline
fdfpp<- summarySE(fdf, measurevar="HRchangeInPercent", groupvars=c("episode"))
fdfpc<- summarySE(fdf, measurevar="HRchangeInBPM", groupvars=c("episode"))


<<<<<<< HEAD

||||||| merged common ancestors
fdfpc
=======
>>>>>>> dda7a181fbc30f66062652366da9c35ee28868d6

ggplot(fdfpc[fdfpc$episode!="stressor removal",], aes(x=episode, y=-1*HRchangeInBPM,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInBPM-ci, ymax=-1*HRchangeInBPM+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate reduction from baseline in BPM')+theme_bw()
