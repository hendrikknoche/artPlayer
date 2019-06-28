library(readxl)
library(sqldf)
library(lme4)
library(ggplot2)
library(reshape2)
library(plyr)
library(Hmisc)
library(here)
setwd(paste(here::here("Processed - Kopi","data")))


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

Full <- read_excel("fdf-split2 - All Participants.xlsx") #Henvis til de rigtige excel sheet
#Excel sheetet skal have v?rdierne i kolonner
Full <- subset(Full, PID != "") #Fjerner de r?kker hvor der ingen v?rdier er
episodes <- melt(Full, id.vars=c("episode")) #Samler tingene i forhold til Condition

gg <- subset(episodes, episode == "baseline")
#gg <- gg[-(1:7), ]
gg <- subset(gg, variable =="LF")
#gg <- subset(gg, variable == "LF") #V?lg den specifikke variabel der skal kigges p?
gg[,3] <- sapply(gg[,3], as.numeric) #S?rger for det der skal arbjedes med er numre
gg$episode <- as.factor(gg$episode) #S?rger for conditions bliver st?ende som faktorer frem for karaktere

gg2 <- subset(episodes, episode == "syringe")
#gg2 <- gg2[-(1:7), ]
gg2 <- subset(gg2, variable =="LF")
#gg2 <- subset(gg2, value != "NA")
gg2[,3] <- sapply(gg2[,3], as.numeric) #S?rger for det der skal arbjedes med er numre
gg2$episode <- as.factor(gg2$episode) #S?rger for conditions bliver st?ende som faktorer frem for karaktere
gg2 <- subset(gg2, value != "")

gg3 <- subset(episodes, episode == "ArtPlayer")
#gg3 <- gg3[-(1:7), ]
gg3 <- subset(gg3, variable =="LF")
gg3[,3] <- sapply(gg3[,3], as.numeric) #S?rger for det der skal arbjedes med er numre
gg3$episode <- as.factor(gg3$episode) #S?rger for conditions bliver st?ende som faktorer frem for karaktere


ggSum <- summarySE(gg, measurevar="value", groupvars=c("episode")) #Udreger sd, se, og ci
ggSum <- ggSum[-c(5),] #Ikke helt sikker p? hvad den g?r
ggSum2 <- summarySE(gg2, measurevar="value", groupvars=c("episode")) #Udreger sd, se, og ci
ggSum2 <- ggSum2[-c(5),] #Ikke helt sikker p? hvad den g?r
ggSum3 <- summarySE(gg3, measurevar="value", groupvars=c("episode")) #Udreger sd, se, og ci
ggSum3 <- ggSum3[-c(5),] #Ikke helt sikker p? hvad den g?r

sumCollected <- rbind(ggSum, ggSum2, ggSum3)

pdf(file="ci2PlotLF.pdf",width=5,height=2.8)
ggplot(sumCollected, aes(episode, value, colour=episode)) + theme_bw() + #hele plottet, s?rg for at bruge de rigtige variabler
  geom_point(position=position_dodge(0.2), color="red", size=2.5) +
  scale_y_continuous(limits = c(-100,1700)) +
  geom_errorbar(aes(ymin=(sumCollected$value-sumCollected$ci), ymax=(sumCollected$value+sumCollected$ci)), width=.2, position = position_dodge(0.2), color="red") +
  ylab("LF power ") + xlab("Episode") + theme(legend.position="none") +
  geom_jitter(data=gg, mapping=aes(episode, value), width=0.2, color="black", alpha=0.33, size=0.8) + 
  geom_jitter(data=gg2, mapping=aes(episode, value), width=0.2, color="black", alpha=0.33, size=0.8) + 
  geom_jitter(data=gg3, mapping=aes(episode, value), width=0.2, color="black", alpha=0.33, size=0.8)

dev.off()
#Husk at ?ndre ylim s? det passer
