#Load necessary libraries
library(data.table)
library(dplyr)

# This function can be used to calculate your prediction scores before submitting to Kaggle
evaluate = function(finalPredictions){
  
  # Load the data
  tourneyCompact = fread("MNCAATourneyCompactResults.csv")
  
  # Filter data for seasons: 2013, 2014, 2015 & 2016
  seasons2Test = seq(2013, 2016)
  tourneyCompact = filter(tourneyCompact, Season %in% seasons2Test)
  
  # Remove the Play-In ("First Four") games
  tourneyCompact = filter(tourneyCompact, DayNum > 135)
  
  # Select only relavent columns
  tourneyCompact = tourneyCompact[, c(1, 3, 5)]
  tourneyCompact$Result = 1
  names(tourneyCompact)[c(2:3)] = c("Team1", "Team2")
  
  # Swap the teams and change the result if the winning team number is 
  # greater than the losing team number
  tourneyCompact$Result = as.numeric(!(tourneyCompact$Team1 > tourneyCompact$Team2))
  tourneyCompact$temp = 0
  tourneyCompact$temp[tourneyCompact$Result == 0] = tourneyCompact$Team1[tourneyCompact$Result == 0]
  tourneyCompact$Team1[tourneyCompact$Result == 0] = tourneyCompact$Team2[tourneyCompact$Result == 0]
  tourneyCompact$Team2[tourneyCompact$Result == 0] = tourneyCompact$temp[tourneyCompact$Result == 0]
  tourneyCompact = tourneyCompact[, -5]
  
  # Create a dataset with "id" and "Result" variables
  actualResults = tourneyCompact %>% mutate(id = paste(Season, Team1, Team2, sep = "_"))
  actualResults = select(actualResults, id, Result)
  
  # Merge our predictions with actual results to form a "final" dataset
  finalPredictions$id = as.character(finalPredictions$id)
  finalPredictions$pred = as.numeric(as.character(finalPredictions$pred))
  final = merge(actualResults, finalPredictions)
  print(actualResults)
  # Calculate the log loss for each match
  final = final %>% mutate(predProb = (Result*log(pred) + (1-Result)*log(1-pred)))
  
  # Calculate our final score
  score = -sum(final$predProb)/nrow(final)
  return (score)
}

#SampleSubmission
sampleSubmission = fread("../input/MSampleSubmissionStage1.csv")
evaluate(sampleSubmission)


