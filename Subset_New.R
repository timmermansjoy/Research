#This script extracts the several groups (created in the script "Grouping.R") from the full dataset
#and calculates the success within these groups

#Step 1: Run Grouping.R to retreive the subgroups
#Step 2: Add disease name column to "All_Groups.xlsx" this file is an output from "Grouping.R"

#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')
Subgroups <- read_xlsx('Data/All_Groups.xlsx')

#Create Empty Dataframes
Subgroup_Success <- data.frame(matrix(ncol = 2, nrow = 0))
Subgroup_Termination <- data.frame(matrix(ncol = 2, nrow = 0))
#Create malleable dataframes
Data_Subset <- Data
Subgroups_Subset <- Subgroups

for(i in 1:(nrow(Subgroups))){
  #If code has 5 characters
  if (nchar(Subgroups$group[[i]]) == 5){
    #subset code
    .a <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == Subgroups$group[[i]])
    #Count Success
    .b <- dplyr::count(.a, Trial_Success)
    #Create dataframes with percentage positive outcomes and terminations
    .z <- data.frame(Field = Subgroups$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups$disease[[i]], Termination = subset_termination(.b))
    #Add percentages to full overview dataframe
    Subgroup_Success <- rbind(Subgroup_Success, .z)
    Subgroup_Termination <- rbind(Subgroup_Termination, .y)
    #Delete Used data from primary dataframes
    Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != Subgroups$group[[i]])
    Subgroups_Subset <- subset(Subgroups_Subset, Subgroups_Subset$group != Subgroups$group[[i]])
  }
}

#Create malleable dataframes
Subgroups_Subset2 <- Subgroups_Subset

for(i in 1:(nrow(Subgroups_Subset))){
  #If code has 3 characters
  if (nchar(Subgroups_Subset$group[[i]]) == 3){
    #subset code
    .a <- subset(Data_Subset, grepl(paste('^', Subgroups_Subset$group[[i]], sep = ""), Data_Subset$`ICD-10 Code`))
    #Count Success
    .b <- dplyr::count(.a, Trial_Success)
    #Create dataframes with percentage positive outcomes and terminations
    .z <- data.frame(Field = Subgroups_Subset$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups_Subset$disease[[i]], Termination = subset_termination(.b))
    #Add percentages to full overview dataframe
    Subgroup_Success <- rbind(Subgroup_Success, .z)
    Subgroup_Termination <- rbind(Subgroup_Termination, .y)
    #Delete Used data from primary dataframes
    Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, Subgroups_Subset$group[[i]]))
    Subgroups_Subset2 <- subset(Subgroups_Subset2, Subgroups_Subset2$group != Subgroups_Subset$group[[i]])
  }
}

#Create Empty Dataframes
Chapter_Success <- data.frame(matrix(ncol = 2, nrow = 0))
Chapter_Termination <- data.frame(matrix(ncol = 2, nrow = 0))
#Create malleable dataframes
Subgroups_Subset3 <- Subgroups_Subset2

for(i in 1:(nrow(Subgroups_Subset2))){
  #If code has 1 character
  if (nchar(Subgroups_Subset2$group[[i]]) == 1){
    #subset code
    .a <- subset(Data, grepl(paste('^', Subgroups_Subset2$group[[i]], sep = ""), Data$`ICD-10 Code`))
    #Count Success
    .b <- dplyr::count(.a, Trial_Success)
    #Create dataframes with percentage positive outcomes and terminations
    .z <- data.frame(Field = Subgroups_Subset2$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups_Subset2$disease[[i]], Termination = subset_termination(.b))
    #Add percentages to full overview dataframe
    Chapter_Success <- rbind(Chapter_Success, .z)
    Chapter_Termination <- rbind(Chapter_Termination, .y)
    #Delete Used data from primary dataframes
    Data_Subset <- subset(Data_Subset, !str_detect(Data$`ICD-10 Code`, Subgroups_Subset2$group[[i]]))
    Subgroups_Subset3 <- subset(Subgroups_Subset3, Subgroups_Subset3$group != Subgroups_Subset2$group[[i]])
  }
}

#Calculate percentages of remaining non-grouped trials
#Count Success
.b <- dplyr::count(Data_Subset, Trial_Success)
#Create dataframes with percentage positive outcomes and terminations
.z <- data.frame(Field = 'Other', Success = Percentage_calc(.b))
.y <- data.frame(Field = 'Other', Termination = subset_termination(.b))
#Add percentages to full overview dataframe
Chapter_Success <- rbind(Chapter_Success, .z)
Chapter_Termination <- rbind(Chapter_Termination, .y)
