library('RHRV')
library(here)
setwd(here::here())
source("CreateTimeAnalysisByEpisodes.R")
setwd(paste(here::here("Processed - Kopi","data")))

#source("HRV_Data_handler.R")
#import IBI data in milliseconds (three digits e.g 569, no decimal) from file
#first set directory to where the data file is located - here it's to the HRV Data folder in Kejser's P8 Google Drive folder

#things to do:
#send the processed data file to Hendrik
#Check that the thing loops correctly
#output the data in a single file (opposite of melt) -> dcast
#library(reshape2)
#asdf<-data.frame(dcast(fdf,pid+episode~measure))
#write.csv(asdf,row.names="false")
#check that file works
#write a readme on how the file should be ordered, where they should be, etc.
#do the importscript to check the plots
#do a quick write-up on analysis

entire.data <- list()
max_part <- 7

#Write which participant's data you want to load
#participant = 1
fdf<-data.frame("PID" = 1:2, "episode" =  c("base","syr"), "measure" = c("meas1","meas2"),"value" = c(21,15))
fdf <- fdf[-c(1,2),]
xtt<-data.frame("PID" = 1:2, "episode" =  c("base","syr"), "measure" = c("meas1","meas2"),"value" = c(21,15))
xtt <- xtt[-c(2),]
episodes <- data.frame("episode" = c("baseline","syringe","ArtPlayer"))
hr <- data.frame("PID" = 1:2, "Average HR")
hr <- hr[-c(1,2),]
hrTemp <- data.frame("PID" = 1:2, "episode" = c("base","syr"), "measure" = c("meas1"), "value" = c(21,15))
hrTemp <- hrTemp[-c(2),]

#Check the difference in baseline
differenceInHR <- data.frame("PID"= 1:2, "difference" = c(21,12))
differenceInHR <- differenceInHR[-c(1,2),]
diffTemp <- data.frame("PID"= 1:2,  "difference" = c(21,12))
diffTemp <- diffTemp[-c(2),]

for (participant in 1:max_part) {

#source("CreateTimeAnalysisByEpisodes.R")
file_list = list.files()
file_list = file_list[grepl(".txt", file_list)]

part_number = (participant * 2) - 1
part_timestamp = part_number + 1

IBI<-read.csv(file = file_list[part_number]) #insert .txt file name here
tsIBI<-as.data.frame(cumsum(c(0, IBI[2:nrow(IBI),]/1000)))
names(tsIBI)<-c('beats')
#create data structure
hrv.data  = CreateHRVData()
baseline.data = CreateHRVData()
stressor.data = CreateHRVData()
intervention.data = CreateHRVData()
participant.data =CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE )

#need to write data back to file as I couldn't figure out how to simply inject it into the data structure, not sure whether there are dependencies
write.table(tsIBI$beats, file = "testTSibi.txt", sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)
#you need to go to the text file now and remove the first line with "x" and save again
#then you can run the next commands
hrv.data = LoadBeatAscii(hrv.data, "testTSibi.txt")
#hrv.data = AddEpisodes(hrv.data,
 #                      InitTimes = c(0,502,892),
  #                     Tags = c("Baseline", "Stressor","Intervention"),
   #                    Durations = c(300,300,600),
    #                   Values = c(0,0,0))

hrv.data = LoadEpisodesAscii(hrv.data,File = file_list[part_timestamp],header = FALSE)

hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)
PlotNIHR(hrv.data, main = "niHR",Tags = "all")


hrv.data = CreateTimeAnalysis(hrv.data, size = 300, interval = 7.8125)

baseline.data$TimeAnalysis = CreateTimeAnalysisByEpisodes(hrv.data, Tag = "Baseline")
xtt$PID <- participant #xtt is a temporary dataframe to save the data in.
xtt$episode <- episodes$episode[1] #do this for the rest as well

xtt$value <- baseline.data[["TimeAnalysis"]][["resultIn"]][["SDNN"]]
#xtt$episode <- "baseline
xtt$measure <- "SDNN"
fdf <- rbind(fdf,xtt)

xtt$value <- baseline.data[["TimeAnalysis"]][["resultIn"]][["pNN50"]]
#xtt$episode <- "baseline"
xtt$measure <- "pNN50"
fdf <- rbind(fdf,xtt)

xtt$value <- baseline.data[["TimeAnalysis"]][["resultIn"]][["rMSSD"]]
#xtt$episode <- "baseline"
xtt$measure <- "rMSSD"
fdf <- rbind(fdf,xtt)

stressor.data$TimeAnalysis = CreateTimeAnalysisByEpisodes(hrv.data, Tag = "Syringe")
xtt$episode <- episodes$episode[2] #do this for the rest as well
xtt$value <- stressor.data[["TimeAnalysis"]][["resultIn"]][["SDNN"]]
#xtt$episode <- "syringe"
xtt$measure <- "SDNN"
fdf <- rbind(fdf,xtt)

xtt$value <- stressor.data[["TimeAnalysis"]][["resultIn"]][["pNN50"]]
#xtt$episode <- "syringe"
xtt$measure <- "pNN50"
fdf <- rbind(fdf,xtt)

xtt$value <- stressor.data[["TimeAnalysis"]][["resultIn"]][["rMSSD"]]
#xtt$episode <- "syringe"
xtt$measure <- "rMSSD"
fdf <- rbind(fdf,xtt)

intervention.data$TimeAnalysis = CreateTimeAnalysisByEpisodes(hrv.data, Tag = "ArtPlayer")
xtt$episode <- episodes$episode[3] #do this for the rest as well
xtt$value <- intervention.data[["TimeAnalysis"]][["resultIn"]][["SDNN"]]
#xtt$episode <- "ArtPlayer"
xtt$measure <- "SDNN"
fdf <- rbind(fdf,xtt)

xtt$value <- intervention.data[["TimeAnalysis"]][["resultIn"]][["pNN50"]]
#xtt$episode <- "ArtPlayer"
xtt$measure <- "pNN50"
fdf <- rbind(fdf,xtt)

xtt$value <- intervention.data[["TimeAnalysis"]][["resultIn"]][["rMSSD"]]
#xtt$episode <- "ArtPlayer"
xtt$measure <- "rMSSD"
fdf <- rbind(fdf,xtt)


hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = SetVerbose(hrv.data,TRUE)
hrv.data =
  CalculatePowerBand(hrv.data , indexFreqAnalysis = 1,
                     size = 100, shift = 2, type = "fourier",
                     ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                     LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )

baseline.data$FreqAnalysis = SplitPowerBandByEpisodes(hrv.data, indexFreqAnalysis = 1, Tag = c("Baseline"))
freqTemp <- baseline.data$FreqAnalysis$InEpisodes$LF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "baseline"
xtt$measure <- "LF"
fdf <- rbind(fdf,xtt)

freqTemp <- baseline.data$FreqAnalysis$InEpisodes$HF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "baseline"
xtt$measure <- "HF"
fdf <- rbind(fdf,xtt)

stressor.data$FreqAnalysis = SplitPowerBandByEpisodes(hrv.data, indexFreqAnalysis = 1, Tag = c("Syringe"))
freqTemp <- stressor.data$FreqAnalysis$InEpisodes$LF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "syringe"
xtt$measure <- "LF"
fdf <- rbind(fdf,xtt)

freqTemp <- stressor.data$FreqAnalysis$InEpisodes$HF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "syringe"
xtt$measure <- "HF"
fdf <- rbind(fdf,xtt)

intervention.data$FreqAnalysis = SplitPowerBandByEpisodes(hrv.data, indexFreqAnalysis = 1, Tag = c("ArtPlayer"))
freqTemp <- intervention.data$FreqAnalysis$InEpisodes$LF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "ArtPlayer"
xtt$measure <- "LF"
fdf <- rbind(fdf,xtt)

freqTemp <- intervention.data$FreqAnalysis$InEpisodes$HF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "ArtPlayer"
xtt$measure <- "HF"
fdf <- rbind(fdf,xtt)

baseline.data$FreqAnalysis$InEpisodes$LFHF <- baseline.data$FreqAnalysis$InEpisodes$LF / baseline.data$FreqAnalysis$InEpisodes$HF
freqTemp <- baseline.data$FreqAnalysis$InEpisodes$LFHF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "baseline"
xtt$measure <- "LFHF"
fdf <- rbind(fdf,xtt)


stressor.data$FreqAnalysis$InEpisodes$LFHF <- stressor.data$FreqAnalysis$InEpisodes$LF / stressor.data$FreqAnalysis$InEpisodes$HF
freqTemp <- stressor.data$FreqAnalysis$InEpisodes$LFHF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "syringe"
xtt$measure <- "LFHF"
fdf <- rbind(fdf,xtt)



intervention.data$FreqAnalysis$InEpisodes$LFHF <- intervention.data$FreqAnalysis$InEpisodes$LF / intervention.data$FreqAnalysis$InEpisodes$HF
freqTemp <- intervention.data$FreqAnalysis$InEpisodes$LFHF
xtt$value <- sum(freqTemp)/length(freqTemp)
xtt$episode <- "ArtPlayer"
xtt$measure <- "LFHF"
fdf <- rbind(fdf,xtt)


baselineHR <- SplitHRbyEpisodes(hrv.data, Tag = "Baseline")
hrTemp$PID <- participant
hrTemp$episode <- "baseline"
hrTemp$measure <- "Avg HR"
hrTemp$value <- mean(baselineHR$InEpisodes)
hr <- rbind(hr,hrTemp)
fdf <- rbind(fdf,hrTemp)

syringeHR <- SplitHRbyEpisodes(hrv.data, Tag = "Syringe")
hrTemp$episode <- "syringe"
hrTemp$value <- mean(syringeHR$InEpisodes)
hr <- rbind(hr,hrTemp)
fdf <- rbind(fdf,hrTemp)
interventionHR <- SplitHRbyEpisodes(hrv.data, Tag = "ArtPlayer")
hrTemp$episode <- "ArtPlayer"
hrTemp$value <- mean(interventionHR$InEpisodes)
hr <- rbind(hr,hrTemp)
fdf <- rbind(fdf,hrTemp)

PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16, Tags = c("Baseline","ArtPlayer","Syringe"))

participant.data$baseline <- baseline.data
participant.data$stressor <- stressor.data
participant.data$intervention <- intervention.data
participant.data$hrv <- hrv.data
participant.data$ID <- participant
entire.data[[participant]] <- participant.data
entire.data[[participant]]$FreqAnalysis = hrv.data$FreqAnalysis
entire.data[[participant]]$TimeAnalysis = hrv.data$TimeAnalysis

firstHR <- baselineHR$InEpisodes[1:1120]
firstHR <- firstHR[!is.na(firstHR)]
secondHR <- baselineHR$InEpisodes[1121:2240]
secondHR <- secondHR[!is.na(secondHR)]
diffTemp$PID <- participant
diffTemp$difference <- sum(firstHR)/length(firstHR)-sum(secondHR)/length(secondHR)
differenceInHR <- rbind(differenceInHR,diffTemp)

}
#fdf <- subset(fdf, PID != 3)

library(reshape2)
asdf<-data.frame(dcast(fdf,PID+episode~measure))
write.csv(asdf, file = "fdf.csv", row.names = FALSE)
#write.csv(asdf, file = "fdfNo3.csv", row.names = FALSE)

# see here for more details on size of windows, shifts etc.
# https://cran.r-project.org/web/packages/RHRV/vignettes/RHRV-quickstart.html