
library(ggplot2)
library(sqldf)
library(here)
library(Rmisc)
library(readxl)
setwd(paste(here::here("P8dataVRTV")))
#fdf<-read.csv("fdf.csv")

fdf<-read_excel("fdf-split.xlsx")
#fdf$episode<-gsub("ArtPlayer", "intervention", fdf$episode)
#fdf$episode<-gsub("Debrief", "debrief", fdf$episode)
#fdf$episode<-gsub("syringe", "stressor", fdf$episode)
#fdf$episode<-gsub("ThreatRemoval", "stressor removal", fdf$episode)
#fdf$episode<-factor(fdf$episode,levels=c("baseline","stressor","intervention","stressor removal", "debrief"))

fdfp <- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode","condition"))
fdfBL<-fdf[fdf$episode=="baseline",c("PID","condition","Avg.HR")]
names(fdfBL)[3]="HRatBaseline"
#sqldf("select PID, Avg.HR as HRatBaseline from fdf where ")
fdf<-merge(fdf,fdfBL)
fdf$HRchangeInPercent<-1-fdf$Avg.HR/fdf$HRatBaseline
fdf$HRchangeInBPM<-fdf$Avg.HR-fdf$HRatBaseline
fdfpp<- summarySE(fdf, measurevar="HRchangeInPercent", groupvars=c("episode","condition"))
fdfpc<- summarySE(fdf, measurevar="HRchangeInBPM", groupvars=c("episode","condition"))



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
level_order <- c("baseline","stressor","intervention")
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

