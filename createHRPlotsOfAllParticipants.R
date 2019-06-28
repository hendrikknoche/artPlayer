library(here)
library(xlsx)
library(ggplot2)
library(dplyr)
library(plyr)
library(here)
setwd(paste(here::here("Results","Polar","Johan")))

episodes<-read.xlsx("AllEpisodes.xlsx",1)
IBI<-read.xlsx("allIBIsbyPerson.xlsx",1)
IBI$time<-IBI$IBI/1000
IBI$eachTime<-ave(IBI$IBI/1000, IBI$PID, FUN=cumsum)
IBI$eachTimes=round(IBI$eachTime/60,1)

#episodes<-episodes[episodes$episode!="StopArtPlayer",]
episodes$eachTime<-ave(episodes$durationInSecs, episodes$PID, FUN=cumsum)
episodes$yminT<-400
episodes$ymaxT<-1400
episodeBoxes<-episodes[episodes$episodeType=="episodes",] 
ggplot(IBI,aes(x=eachTime,y=IBI))+ geom_point(size=.05,alpha=.5)+
  xlab("Time during experiment in seconds ")+
  ylab("inter beat intervals in milliseconds")+ylim(400,1400)+theme_bw() + 
  scale_x_continuous(breaks=seq(0,max(IBI$eachTime),60))+
  geom_vline(data = episodes , aes(xintercept = fromTime, group=episodeType,colour=episodeType),size = .25)+
  geom_rect(data = episodeBoxes , aes(xmin=fromTime,xmax=fromTime+durationInSecs,ymin=yminT,ymax=ymaxT,x=notSureVar,y=notSureVar), fill="red",alpha=.05)+
  geom_text(data=episodeBoxes, aes(x=fromTime, y=ymaxT, label=episode),size=2,hjust=1,vjust=1,angle =90)+
  facet_grid(rows = vars(PID))

d+facet_grid(rows = vars(PID))
  
ggplot(IBI[IBI$PID %in% c("P2","P3","P5") & IBI$eachTime>1400,],aes(x=eachTime,y=IBI))+ geom_point(size = .05,alpha=.5)+xlab("Time during experiment in seconds ")+ylab("inter beat intervals in milliseconds")+ylim(500,1300)+theme_bw() + scale_x_continuous(breaks=seq(1400,max(IBI$eachTime),60))+geom_vline(data = episodes[episodes$PID %in% c("P2","P3","P5") & episodes$fromTime>1400,] , aes(xintercept = fromTime, group=type,colour=type),size = .25)+geom_vline(xintercept = 1400,colour="red",size = .25)+facet_grid(rows = vars(PID))
ggplot(IBI[IBI$PID %in% c("P5"),],aes(x=eachTime,y=IBI))+ geom_point(size = .05,alpha=.5)+xlab("Time during experiment in seconds ")+ylab("inter beat intervals in milliseconds")+ylim(500,800)+theme_bw() + scale_x_continuous(breaks=seq(0,max(IBI$eachTime),60))+geom_vline(data = episodes[episodes$PID %in% c("P5"),] , aes(xintercept = fromTime, group=type,colour=type),size = .25)+geom_vline(xintercept = 0,colour="red",size = .25)+facet_grid(rows = vars(PID))

geom_vline()+

library(sqldf)
sqldf("select x_obj_pos as cX from df")
