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

Full <- read_excel("C:/Users/jwk_2/Desktop/Processed - Kopi/data/fdf-split.xlsx") #Henvis til de rigtige excel sheet
#Excel sheetet skal have værdierne i kolonner
Full <- subset(Full, PID != "") #Fjerner de rækker hvor der ingen værdier er
gg <- melt(Full, id.vars=c("episode")) #Samler tingene i forhold til Condition
gg <- subset(gg, episode == "baseline")
gg <- subset(gg, variable == "HF") #Vælg den specifikke variabel der skal kigges på
gg[,3] <- sapply(gg[,3], as.numeric) #Sørger for det der skal arbjedes med er numre
gg$episode <- as.factor(gg$episode) #Sørger for conditions bliver stående som faktorer frem for karaktere
gg2 <- summarySE(gg, measurevar="value", groupvars=c("episode")) #Udreger sd, se, og ci
gg2 <- gg2[-c(5),] #Ikke helt sikker på hvad den gør
pdf(file="ci2-plot.pdf",width=5,height=2.8)
ggplot(gg2, aes(episode, value))
dev.off()
#Husk at ændre ylim så det passer
