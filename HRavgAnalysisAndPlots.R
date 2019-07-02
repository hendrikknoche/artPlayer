library(ggplot2)
library(sqldf)
setwd(paste(here::here("Processed - Kopi","data")))
fdf<-read.csv("fdf.csv")
fdfp <- summarySE(fdf, measurevar="Avg.HR", groupvars=c("episode"))
fdfBL<-fdf[fdf$episode=="baseline",c("PID","Avg.HR")]
names(fdfBL)[2]="HRatBaseline"
sqldf("select PID, Avg.HR as HRatBaseline from fdf where ")
fdf<-merge(fdf,fdfBL)
fdf$HRchangeInPercent<-1-fdf$Avg.HR/fdf$HRatBaseline
fdf$HRchangeInBPM<-fdf$Avg.HR-fdf$HRatBaseline
fdfpp<- summarySE(fdf, measurevar="HRchangeInPercent", groupvars=c("episode"))
fdfpc<- summarySE(fdf, measurevar="HRchangeInBPM", groupvars=c("episode"))

ggplot(fdfpc, aes(x=episode, y=HRchangeInBPM))+
  geom_errorbar(aes(ymin=HRchangeInBPM-ci, ymax=HRchangeInBPM+ci), width=.1) 

+
  geom_line(position=pd) +
  geom_point(position=pd)