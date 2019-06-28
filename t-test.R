library(readxl)
library(sqldf)
library(lme4)
library(ggplot2)
library(reshape2)
library(plyr)
library(ggplot2)
library(Hmisc)
setwd("C:/Users/jwk_2/Desktop/Processed - Kopi/data") #set this to the right one

Full <- read_excel("C:/Users/jwk_2/Desktop/Processed - Kopi/data/All Data.xlsx") #Henvis til de rigtige excel sheet
Full <- subset(Full, PID != "") #Fjerner de rækker hvor der ingen værdier er
gg <- melt(Full, id.vars=c("episode")) #Samler tingene i forhold til Condition

#gg <- subset(gg, variable == "Avg.HR") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "HF") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "LF") #Vælg den specifikke variabel der skal kigges på
gg <- subset(gg, variable == "LFHF") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "pNN50") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "rMSSD") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "SDNN") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, variable == "RRS") #Vælg den specifikke variabel der skal kigges på
#gg <- subset(gg, value != "NA")
gg[,3] <- sapply(gg[,3], as.numeric) #Sørger for det der skal arbjedes med er numre
gg$episode <- as.factor(gg$episode) #Sørger for conditions bliver stående som faktorer frem for karaktere

baseline <- subset(gg, episode == "baseline")
syringe <- subset(gg, episode == "syringe")
ArtPlayer <- subset(gg, episode == "ArtPlayer")

t1Two <- t.test(baseline$value,syringe$value, alternative = c("t"), paired = TRUE)
t2Two <- t.test(baseline$value,ArtPlayer$value, alternative = c("t"), paired = TRUE)
t3Two <- t.test(syringe$value,ArtPlayer$value, alternative = c("t"), paired = TRUE)

w1Two <- wilcox.test(baseline$value, syringe$value, alternative = c("t"), paired = TRUE)
w2Two <- wilcox.test(baseline$value, ArtPlayer$value, alternative = c("t"), paired = TRUE)
w3Two <-wilcox.test(syringe$value, ArtPlayer$value, alternative = c("t"), paired = TRUE)

t1Greater <- t.test(baseline$value,syringe$value, alternative = c("greater"), paired = TRUE)
t2Greater <- t.test(baseline$value,ArtPlayer$value, alternative = c("greater"), paired = TRUE)
t3Greater <- t.test(syringe$value,ArtPlayer$value, alternative = c("greater"), paired = TRUE)

w1Greater <- wilcox.test(baseline$value, syringe$value, alternative = c("greater"), paired = TRUE)
w2Greater <- wilcox.test(baseline$value, ArtPlayer$value, alternative = c("greater"), paired = TRUE)
w3Greater <-wilcox.test(syringe$value, ArtPlayer$value, alternative = c("greater"), paired = TRUE)

t1Less <- t.test(baseline$value,syringe$value, alternative = c("less"), paired = TRUE)
t2Less <- t.test(baseline$value,ArtPlayer$value, alternative = c("less"), paired = TRUE)
t3Less <- t.test(syringe$value,ArtPlayer$value, alternative = c("less"), paired = TRUE)

w1Less <- wilcox.test(baseline$value, syringe$value, alternative = c("less"), paired = TRUE)
w2Less <- wilcox.test(baseline$value, ArtPlayer$value, alternative = c("less"), paired = TRUE)
w3Less <-wilcox.test(syringe$value, ArtPlayer$value, alternative = c("less"), paired = TRUE)

twoSided <- c(t1Two$p.value, t2Two$p.value, t3Two$p.value, w1Two$p.value, w2Two$p.value, w3Two$p.value)
greater <- c(t1Greater$p.value, t2Greater$p.value, t3Greater$p.value, w1Greater$p.value, w2Greater$p.value, w3Greater$p.value)
less <- c(t1Less$p.value, t2Less$p.value, t3Less$p.value, w1Less$p.value, w2Less$p.value, w3Less$p.value)
