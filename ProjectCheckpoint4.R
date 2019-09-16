# Project_Checkpoint_4
#code for R Project Checkpoint 4

#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("dplyr")

library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(readr)

#import dataset on honeybee colonies January-March 2018
colonyData <- read_csv("honey_p03_t005.csv")

#import dataset on honeybee colony health stressors January-March 2018
stressorData <- read_csv("honey_p08_t002.csv")

#summary of imported data:
summary(colonyData)
summary(stressorData)

#cleaning up
colonyState <- colonyData[, c(1)]
colonyData <- mutate(colonyData, state = NULL)

stressorState <- stressorData[, c(1)]
stressorData <- mutate(stressorData, state = NULL)

colonyPercent <- colonyData[, c(4,7)]
colonyData <- mutate(colonyData, lost_percent = NULL, renovated_percent = NULL)

#correlation matrix:
corMatrix_colony <- rcorr(as.matrix(colonyData), type = c("pearson","spearman"))
corMatrix_colony

corMatrix_stressor <- rcorr(as.matrix(stressorData), type = c("pearson","spearman"))
corMatrix_stressor

#combining data
combinedData <- merge(colonyData, stressorData)

#correlation matrix again:
rcorMatrix_combined <- rcorr(as.matrix(combinedData), type = c("pearson","spearman"))
rcorMatrix_combined

#the most correlated variables are:
#diseases and other_pests
lmCombined <- lm(combinedData$lost ~ combinedData$diseases + combinedData$other_pests, combinedData)
summary(lmCombined)

#plotting those variables against one another
plot(combinedData$lost, combinedData$diseases)
plot(combinedData$lost, combinedData$other_pests)
plot(combinedData$diseases, combinedData$other_pests)
#the first two plots aren't very helpful
#the third at least has the makings of a pattern
