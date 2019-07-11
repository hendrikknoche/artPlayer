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
fdf$episode<-factor(fdf$episode,levels=c("baseline","stressor","intervention"))
fdf$condition<-factor(fdf$condition,levels = c("tv+","vr+","vr-"))
level_order <- c("baseline","stressor","intervention")

fdfp <- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode","condition"))
fdfr <- summarySE(fdf, measurevar="RRS", groupvars = c("episode","condition"))
fdfBL<-fdf[fdf$episode=="baseline",c("PID","Avg.HR")]
names(fdfBL)[2]="HRatBaseline"
fdfst<-fdf[fdf$episode=="stressor",c("PID","Avg.HR")]
names(fdfst)[2]="HRatStressor"
fdfRRS<-fdf[fdf$episode=="stressor",c("PID","RRS")]
names(fdfRRS)[2]="RRSatStressor"


#sqldf("select PID, Avg.HR as HRatBaseline from fdf where ")
fdf<-merge(fdf,fdfBL)
fdf<-merge(fdf,fdfst)
fdf<-merge(fdf,fdfRRS)
fdf$HRchangeInPercent<-1-fdf$Avg.HR/fdf$HRatBaseline
fdf$HRchangeInBPM<-fdf$Avg.HR-fdf$HRatBaseline
fdf$HRchangeInBPMfromStressor<-fdf$Avg.HR-fdf$HRatStressor
fdf$HRchangeInPercentfromStressor<-1-fdf$Avg.HR/fdf$HRatStressor
fdf$RRSchangeInfromStressor<-fdf$RRS-fdf$RRSatStressor

dffHR<- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode","condition"))
fdfpp<- summarySE(fdf, measurevar="HRchangeInPercent", groupvars=c("episode","condition"))
fdfpc<- summarySE(fdf, measurevar="HRchangeInBPM", groupvars=c("episode","condition"))
fdpst<-summarySE(fdf, measurevar="HRchangeInBPMfromStressor", groupvars=c("episode","condition"))
fdpstpct<-summarySE(fdf, measurevar="HRchangeInPercentfromStressor", groupvars=c("episode","condition"))
fdRRS_eb<- summarySE(fdf[fdf$episode=="intervention",], measurevar="RRSchangeInfromStressor", groupvars=c("episode","condition"))
names(fdRRS_eb)[4]="RRSchangeAVGInfromStressor"
fdf<-merge(fdf,fdRRS_eb)

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

#========== RRS plot ========== 
ggplot(fdf, aes(x=factor(episode, level = level_order), y=RRS,color=condition,group=condition))+
  geom_errorbar(aes(ymin=RRS-ci, ymax=RRS+ci), width=.2,position = position_dodge2(width=0.2)) +
  geom_line(position = position_dodge2(width=0.2))+
  geom_point(position = position_dodge2(width=0.2))+ylab('Average RRS rating')+theme_bw() + xlab('')+ #+xlab('Episode')
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position="bottom",legend.title=element_text(size=14),legend.text=element_text(size=16))+ 
  scale_color_discrete(name = "participant group:",breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))


#========== RRS plot stressor to intervention change ========== 
ggplot(fdRRS_eb, aes(x=condition, y=RRSchangeAVGInfromStressor, colour=condition)) + theme_bw() + 
  geom_point(position=position_dodge(0.2), color="red", size=2.5)+
geom_errorbar(aes(ymin=RRSchangeAVGInfromStressor-ci, ymax=RRSchangeAVGInfromStressor+ci), width=.2, position = position_dodge(0.2), color="red") +
geom_jitter(data=fdf[fdf$episode=="intervention",], mapping=aes(x=condition, y=RRSchangeInfromStressor), width=0.2, height=0, color="black", alpha=0.33, size=0.8)+
  ylab('change in RRS from stressor to intervention')+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position="bottom",legend.title=element_text(size=14),legend.text=element_text(size=16))+scale_x_discrete(breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))  
  
oneway.test(fdf[fdf$episode=="intervention",]$RRSchangeInfromStressor~ fdf[fdf$episode=="intervention",]$condition)
t.test(fdf[fdf$episode=="intervention",]$RRSchangeInfromStressor, mu=0, alternative="greater", conf.level=0.95)

#================ HR avg plots from baseline ++
ggplot(fdfpc[fdfpc$episode=='intervention',], aes(x=condition, y=HRchangeInBPM,group=condition,color=condition))+
  # geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.1) +
  geom_line(position=position_dodge(0.2)) + ylab('heart rate change from baseline in BPM')+theme_bw() +
  geom_point(position=position_dodge(0.2), size=2.5)+xlab('')+
  geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.2, position = position_dodge(0.2))+
  scale_color_discrete(name = "participant group:",breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position = "none")+scale_x_discrete(breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))  

#================ HR avg plots from baseline ++
ggplot(fdfpc, aes(x=episode, y=HRchangeInBPM,group=condition,color=condition))+
  # geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.1) +
  geom_line(position=position_dodge(0.2)) + ylab('heart rate change from baseline in BPM')+theme_bw() +
  geom_point(position=position_dodge(0.2), size=2.5)+
  geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.2, position = position_dodge(0.2))+
  scale_color_discrete(name = "participant group:",breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position="bottom",legend.title=element_text(size=14),legend.text=element_text(size=16))+xlab('')
+scale_x_discrete(breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))  


#================ HR  plots ++
ggplot(fdfp, aes(x=episode, y=Avg.HR,group=condition,color=condition))+
  # geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.1) +
  geom_line(position=position_dodge(0.2)) + ylab('heart rate change in BPM')+theme_bw() +
  geom_point(position=position_dodge(0.2), size=2.5)+
  geom_errorbar(aes(ymin=Avg.HR-ci, ymax=Avg.HR+ci), width=.2, position = position_dodge(0.2))+
  scale_color_discrete(name = "participant group:",breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position="bottom",legend.title=element_text(size=14),legend.text=element_text(size=16))+xlab('')
+scale_x_discrete(breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))  


#========== HR AVG plot stressor to intervention change ========== 
ggplot(fdpst[fdpst$episode=="intervention",], aes(x=condition, y=HRchangeInBPMfromStressor, colour=condition)) + theme_bw() + 
  geom_point(position=position_dodge(0.2), color="red", size=2.5)+
  geom_errorbar(aes(ymin=HRchangeInBPMfromStressor-ci, ymax=HRchangeInBPMfromStressor+ci), width=.2, position = position_dodge(0.2), color="red") +
  ylab('change in HR in BPM from stressor')+ylim(-35,0)+
  geom_jitter(data=fdf[fdf$episode=="intervention",], mapping=aes(x=condition, y=HRchangeInBPMfromStressor), width=0.2, height=0, color="black", alpha=0.33, size=0.8)+xlab('')+theme(axis.text=element_text(size=14),axis.title=element_text(size=16),legend.position="bottom",legend.title=element_text(size=14),legend.text=element_text(size=16))+scale_x_discrete(breaks=c("tv+", "vr+", "vr-"),labels=c("TV+", "VR+", "VR-"))  
ggsave("Rplot-HRavgFromStressor.pdf", width = 4, height = 6)
dev.copy(device = png, filename = 'MyPlot.png', width = 1000, height = 500) dev.off()

oneway.test(fdf[fdf$episode=="intervention",]$HRchangeInBPMfromStressor~ fdf[fdf$episode=="intervention",]$condition)
t.test(fdf[fdf$episode=="intervention",]$HRchangeInBPMfromStressor, mu=0, alternative="less", conf.level=0.95)
t.test(fdf[fdf$episode=="intervention",]$HRchangeInBPM, mu=0, alternative="less", conf.level=0.95)

Model.aov.cond<-aov(HRchangeInBPMfromStressor ~ condition + Error(PID), data=fdf[fdf$episode=="intervention",])
summary(Model.aov.cond)    
Model.aov.condfromBL<-aov(HRchangeInBPM ~ condition + Error(PID), data=fdf[fdf$episode=="intervention",])
summary(Model.aov.condfromBL)    
Model.aov.condMed<-aov(HRchangeInBPMfromStressor ~ condMedium + Error(PID), data=fdf[fdf$episode=="intervention",])
summary(Model.aov.condMed)   

# difference from baseline
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="tv+" ,]$HRchangeInBPM, mu=0, alternative="less", conf.level=0.95)
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="vr-" ,]$HRchangeInBPM, mu=0, alternative="less", conf.level=0.95)
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="vr+" ,]$HRchangeInBPM, mu=0, alternative="less", conf.level=0.95)
# difference from stressor
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="tv+" ,]$HRchangeInBPMfromStressor, mu=0, alternative="less", conf.level=0.95)
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="vr-" ,]$HRchangeInBPMfromStressor, mu=0, alternative="less", conf.level=0.95)
t.test(fdf[fdf$episode=="intervention" & fdf$condition=="vr+" ,]$HRchangeInBPMfromStressor, mu=0, alternative="less", conf.level=0.95)

# 
# qqnorm(fdf[fdf$episode=="intervention",]$RRSchangeInfromStressor, pch = 1, frame = FALSE)
# qqline(fdf[fdf$episode=="intervention",]$RRSchangeInfromStressor, col = "steelblue", lwd = 2)

# =====================   other stuff =============

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


