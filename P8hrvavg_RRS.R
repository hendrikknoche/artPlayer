library(ggplot2)
library(sqldf)
library(here)
library(Rmisc)
library(readxl)
setwd(paste(here::here("P8dataVRTV")))
#fdf<-read.csv("fdf.csv")
fdf<-read_excel("fdf-splitMOD.xlsx")
fdf$condMedium<-substr(fdf$condition,1,2)
#fdf$episode<-gsub("ArtPlayer", "intervention", fdf$episode)
#fdf$episode<-gsub("Debrief", "debrief", fdf$episode)
#fdf$episode<-gsub("syringe", "stressor", fdf$episode)
#fdf$episode<-gsub("ThreatRemoval", "stressor removal", fdf$episode)
#fdf$episode<-factor(fdf$episode,levels=c("baseline","stressor","intervention","stressor removal", "debrief"))

level_order <- c("baseline","stressor","intervention")

fdfp <- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode","condition"))
fdfr <- summarySE(fdf, measurevar="RRS", groupvars = c("episode","condition"))
fdfBL<-fdf[fdf$episode=="baseline",c("PID","Avg.HR")]
names(fdfBL)[2]="HRatBaseline"
fdfst<-fdf[fdf$episode=="stressor",c("PID","Avg.HR")]
names(fdfst)[2]="HRatStressor"

#sqldf("select PID, Avg.HR as HRatBaseline from fdf where ")
fdf<-merge(fdf,fdfBL)
fdf<-merge(fdf,fdfst)

fdf$HRchangeInPercent<-1-fdf$Avg.HR/fdf$HRatBaseline
fdf$HRchangeInBPM<-fdf$Avg.HR-fdf$HRatBaseline
fdf$HRchangeInBPMfromStressor<-fdf$Avg.HR-fdf$HRatStressor
fdf$HRchangeInPercentfromStressor<-1-fdf$Avg.HR/fdf$HRatStressor

fdfpp<- summarySE(fdf, measurevar="HRchangeInPercent", groupvars=c("episode","condition"))
fdfpc<- summarySE(fdf, measurevar="HRchangeInBPM", groupvars=c("episode","condition"))
fdpst<-summarySE(fdf, measurevar="HRchangeInBPMfromStressor", groupvars=c("episode","condition"))
fdpstpct<-summarySE(fdf, measurevar="HRchangeInPercentfromStressor", groupvars=c("episode","condition"))

oneway.test(fdf[fdf$episode=="intervention",]$HRchangeInBPMfromStressor~ fdf[fdf$episode=="intervention",]$condition)
oneway.test(fdf[fdf$episode=="intervention",]$HRchangeInPercentfromStressor~ fdf[fdf$episode=="intervention",]$condition)
oneway.test(fdf[fdf$episode=="intervention",]$HRchangeInPercentfromStressor~ fdf[fdf$episode=="intervention",]$condMedium)

qqnorm(fdf[fdf$episode=="intervention",]$HRchangeInPercentfromStressor, pch = 1, frame = FALSE)
qqline(fdf[fdf$episode=="intervention",]$HRchangeInPercentfromStressor, col = "steelblue", lwd = 2)


ggplot(fdpstpct[fdpstpct$episode=="intervention",], aes(x=condition, y=-100*HRchangeInPercentfromStressor,group=1, colour = condition))+
  geom_errorbar(aes(ymin=-100*HRchangeInPercentfromStressor-100*ci, ymax=-100*HRchangeInPercentfromStressor+100*ci), width=.1) +
  geom_point()+ylab('heart rate reduction in % from stressor')+theme_bw() + ggtitle("All Conditions")

ggplot(fdpstpct[fdpstpct$episode=="intervention",], aes(x=condition, y=-100*HRchangeInPercentfromStressor,group=1, colour = condition))+
  geom_errorbar(aes(ymin=-100*HRchangeInPercentfromStressor-100*ci, ymax=-100*HRchangeInPercentfromStressor+100*ci), width=.1) +
  geom_point()+ylab('heart rate reduction in % from stressor')+theme_bw() + ggtitle("All Conditions")


ggplot(fdfpc[fdfpc$episode!="stressor removal",], aes(x=episode, y=-1*HRchangeInBPM,group=1, colour = condition))+
  geom_errorbar(aes(ymin=-1*HRchangeInBPM-ci, ymax=-1*HRchangeInBPM+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate reduction from baseline in BPM')+theme_bw() + ggtitle("All Conditions")

ggplot(fdfpc[fdfpc$condition=="tv+",], aes(x=episode, y=-1*HRchangeInBPM,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInBPM-ci, ymax=-1*HRchangeInBPM+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate reduction from baseline in BPM')+theme_bw() + ggtitle("TV Condition")

ggplot(fdfpc[fdfpc$condition=="vr+",], aes(x=episode, y=-1*HRchangeInBPM,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInBPM-ci, ymax=-1*HRchangeInBPM+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate reduction from baseline in BPM')+theme_bw() + ggtitle("VR with Distractors")

ggplot(fdfpc[fdfpc$condition=="vr-",], aes(x=episode, y=-1*HRchangeInBPM,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInBPM-ci, ymax=-1*HRchangeInBPM+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate reduction from baseline in BPM')+theme_bw() + ggtitle("VR without Distractors")

ggplot(fdfpp[fdfpp$episode!="stressor removal",], aes(x=factor(episode, level = level_order), y=-1*HRchangeInPercent,group=1, colour = condition))+
  geom_errorbar(aes(ymin=-1*HRchangeInPercent-ci, ymax=-1*HRchangeInPercent+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate change from baseline in percent')+theme_bw() + ggtitle("All Conditions")

ggplot(fdfpp[fdfpp$condition=="tv+",], aes(x=episode, y=-1*HRchangeInPercent,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInPercent-ci, ymax=-1*HRchangeInPercent+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate change from baseline in percent')+theme_bw() + ggtitle("TV Condition")

ggplot(fdfpp[fdfpp$condition=="vr+",], aes(x=episode, y=-1*HRchangeInPercent,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInPercent-ci, ymax=-1*HRchangeInPercent+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate change from baseline in percent')+theme_bw() + ggtitle("VR with Distractors")

ggplot(fdfpp[fdfpp$condition=="vr-",], aes(x=episode, y=-1*HRchangeInPercent,group=1))+
  geom_errorbar(aes(ymin=-1*HRchangeInPercent-ci, ymax=-1*HRchangeInPercent+ci), width=.1) +
  geom_line() +
  geom_point()+ylab('heart rate change from baseline in percent')+theme_bw() + ggtitle("VR without Distractors")

ggplot(fdfr, aes(x=factor(episode, level = level_order), y=RRS,group=1))+
  geom_errorbar(aes(ymin=RRS-ci, ymax=RRS+ci), width=.1) +
  geom_line() +
  geom_point()+xlab('Episode')+ylab('Average RRS Rating')+theme_bw() + 
  ggtitle("TV with Distractors") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

