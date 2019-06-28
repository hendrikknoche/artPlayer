library(here)
setwd(paste(here::here("Processed - Kopi","FaceReaderProcessed")))

library(reshape2)

#face <- read.csv("Participant 1_Participant 1_Analysis 1_video_20190207_183558_detailed.txt", skip = 2, header = TRUE, sep = "\t")
#headers = headers = read.csv("Participant 1_Participant 1_Analysis 1_video_20190207_183558_detailed.txt", skip = 1, header = F, nrows = 1, as.is = T, sep = "\t")
#have to remove stuff till after framerate

file_list = list.files()
file_list = file_list[grepl(".txt", file_list)]
df <- data.frame("PID" = 1:2, "event" = "syringe", "avg neutral" = 1:2, "avg happy" = 1:2, "avg sad" = 1:2, "avg angry" = 1:2, "avg surprised" = 1:2, 
                 "avg scared" = 1:2, "avg disgusted" = 1:2)
df <- df[-c(1,2),]

#participant = 3
for (participant in 1:7){
face <- read.csv(file = file_list[participant], header = TRUE, sep = "\t")

event1 <- subset(face, Event.Marker == "Syringe")
event1 <- event1[,-(9:20)]
event1 <- event1[, -10]
event1 <- event1[,-1]
event1[, 1:7] <- sapply(event1[, 1:7], as.character)
event1[, 1:7] <- sapply(event1[, 1:7], as.numeric)
avgSyringeNeutral <- sum(event1$Neutral)/length(event1$Neutral)
avgSyringeHappy <- sum(event1$Happy)/length(event1$Happy)
avgSyringeSad <- sum(event1$Sad)/length(event1$Sad)
avgSyringeAngry <- sum(event1$Angry)/length(event1$Angry)
avgSyringeSurprised <- sum(event1$Surprised)/length(event1$Surprised)
avgSyringeScared <- sum(event1$Scared)/length(event1$Scared)
avgSyringeDisgusted <- sum(event1$Disgusted)/length(event1$Disgusted)
tempFrame <- data.frame("PID" = participant,
                        "event" = "syringe",
                        "avg neutral"= avgSyringeNeutral,
                        "avg happy" = avgSyringeHappy,
                        "avg sad" = avgSyringeSad,
                        "avg angry" = avgSyringeAngry,
                        "avg surprised" = avgSyringeSurprised,
                        "avg scared" = avgSyringeScared,
                        "avg disgusted" =avgSyringeDisgusted)
df <- rbind(df,tempFrame)

event2 <- subset(face, Event.Marker == "RRS")
event2 <- event2[,-(9:20)]
event2 <- event2[, -10]
event2 <- event2[,-1]
event2[, 1:7] <- sapply(event2[, 1:7], as.character)
event2[, 1:7] <- sapply(event2[, 1:7], as.numeric)
avgRRSNeutral <- sum(event2$Neutral)/length(event2$Neutral)
avgRRSHappy <- sum(event2$Happy)/length(event2$Happy)
avgRRSSad <- sum(event2$Sad)/length(event2$Sad)
avgRRSAngry <- sum(event2$Angry)/length(event2$Angry)
avgRRSSurprised <- sum(event2$Surprised)/length(event2$Surprised)
avgRRSScared <- sum(event2$Scared)/length(event2$Scared)
avgRRSDisgusted <- sum(event2$Disgusted)/length(event2$Disgusted)
tempFrame <- data.frame("PID" = participant,
                        "event" = "rrs",
                        "avg neutral"= avgRRSNeutral,
                        "avg happy" = avgRRSHappy,
                        "avg sad" = avgRRSSad,
                        "avg angry" = avgRRSAngry,
                        "avg surprised" = avgRRSSurprised,
                        "avg scared" = avgRRSScared,
                        "avg disgusted" =avgRRSDisgusted)
df <- rbind(df,tempFrame)


temp <- subset(face, Stimulus == "ArtPlayer")
temp <- temp[,-(9:20)]
temp <- temp[, -10]
temp <- temp[,-1]
temp[, 1:7] <- sapply(temp[, 1:7], as.character)
temp[, 1:7] <- sapply(temp[, 1:7], as.numeric)
ArtplayerNeutral <- sum(temp$Neutral)/length(temp$Neutral)
ArtplayerHappy <- sum(temp$Happy)/length(temp$Happy)
ArtplayerSad <- sum(temp$Sad)/length(temp$Sad)
ArtplayerAngry <- sum(temp$Angry)/length(temp$Angry)
ArtplayerSurprised <- sum(temp$Surprised)/length(temp$Surprised)
ArtplayerScared <-  sum(temp$Scared)/length(temp$Scared)
ArtplayerDisgusted <- sum(temp$Disgusted)/length(temp$Disgusted)
tempFrame <- data.frame("PID" = participant,
                        "event" = "ArtPlayer",
                        "avg neutral"= ArtplayerNeutral,
                        "avg happy" = ArtplayerHappy,
                        "avg sad" = ArtplayerSad,
                        "avg angry" = ArtplayerAngry,
                        "avg surprised" = ArtplayerSurprised,
                        "avg scared" = ArtplayerScared,
                        "avg disgusted" =ArtplayerDisgusted)
df <- rbind(df,tempFrame)
}

write.csv(df, file = "averageResults.csv", row.names = FALSE)

library(readxl)
library(sqldf)
library(lme4)
library(ggplot2)
library(reshape2)
library(plyr)
library(Hmisc)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column   
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

temp <- melt(temp, id.vars=c("Stimulus"))
#temp <- subset(temp,variable == "Neutral")
gg2 <- summarySE(temp, measurevar="value", groupvars=c("variable")) #Udreger sd, se, og ci
gg2 <- gg2[-c(5),] #Ikke helt sikker p? hvad den g?r
#pdf(file="ci2-plot.pdf",width=5,height=2.8)
ggplot(gg2, aes(variable, value, colour=variable)) + theme_bw() + #hele plottet, s?rg for at bruge de rigtige variabler
  geom_point(position=position_dodge(0.2), color="red", size=2.5) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin=(gg2$value-gg2$ci), ymax=(gg2$value+gg2$ci)), width=.2, position = position_dodge(0.2), color="red") +
  ylab("Percentage") + xlab("Emotion") + theme(legend.position="none") +
  geom_jitter(data=temp, mapping=aes(variable, value), width=0.2, color="black", alpha=0.33, size=0.8)
#dev.off()
#Husk at ?ndre ylim s? det passer
