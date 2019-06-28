
#IMPORTANT!!!!
#always make sure to have run hrv script - MIST.R before running this. To check if you have done this check if you have
#entire.data in the workspace and that it has values in it

#claining up the workspace
rm(baseline.data, hrv.data, IBI, intervention.data, stressor.data, tsIBI, participant.data, part_number, part_timestamp, participant, file_list)

outFile <- paste(getwd(),"AverageScores.txt", sep="/")
write.table(df, outFile, sep="\t", row.names = FALSE, col.names = TRUE, append = TRUE)

#change participant to the one we want to look at
for (participant in 1:max_part) {

average_LFHF <- data.frame()
# puts all data for the participant into these variables
#frequency analysis stuff
freq_baseline <- entire.data[[participant]]$baseline$FreqAnalysis$InEpisodes
freq_stressor <- entire.data[[participant]]$stressor$FreqAnalysis$InEpisodes
freq_intervention <- entire.data[[participant]]$intervention$FreqAnalysis$InEpisodes

#time domain stuff
tim_baseline <- entire.data[[participant]]$baseline$TimeAnalysis$InEpisodes
tim_stressor <- entire.data[[participant]]$stressor$TimeAnalysis$InEpisodes
tim_intervention <- entire.data[[participant]]$intervention$TimeAnalysis$InEpisodes

PlotPowerBand(entire.data[[participant]]$hrv, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16, Tags = c("Baseline","Intervention","Stressor"))

par(mfrow=c(3,1))
plot(freq_baseline$LFHF,ylim = c(0,5), main = "Baseline")
plot(freq_stressor$LFHF,ylim = c(0,5), main = "Stressor")
plot(freq_intervention$LFHF,ylim = c(0,5), main = "intervention")
baseline_avg <- sum(freq_baseline$LFHF)/length(freq_baseline$LFHF)
stressor_avg <- sum(freq_stressor$LFHF)/length(freq_stressor$LFHF)
intervention_avg <- sum(freq_intervention$LFHF)/length(freq_intervention$LFHF)

average_score <- c(baseline_avg,stressor_avg,intervention_avg)
#avg <- average_score$Value

df <- data.frame(baseline_avg,stressor_avg,intervention_avg)

df_time <- data.frame (tim_baseline, tim_stressor, tim_intervention)

write.table(df, outFile, sep="\t", row.names = FALSE, col.names = FALSE,append = TRUE)

write.table(df2, outFile, sep="\t2", row.names = FALSE, col.names = FALSE,append = TRUE)

#APlotPowerBand(entire.data[[participant]]$hrv,indexFreqAnalysis = 1)

#average_LFHF[[participant]] <- data.frame(base_avg,stressor_avg,intervention_avg)
#average_LFHF[[participant]] <- df
}
aks <- c(48.29,31.69,39.18,41.43,51.46,137.62,65.12)
qqnorm(aks)
