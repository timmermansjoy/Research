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

#
Subgroup_Success <- data.frame(matrix(ncol = 2, nrow = 0))
Subgroup_Termination <- data.frame(matrix(ncol = 2, nrow = 0))
Data_Subset <- Data
Subgroups_Subset <- Subgroups

for(i in 1:(nrow(Subgroups))){
  if (nchar(Subgroups$group[[i]]) == 5){
    .a <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == Subgroups$group[[i]])
    .b <- dplyr::count(.a, Trial_Success)
    .z <- data.frame(Field = Subgroups$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups$disease[[i]], Termination = subset_termination(.b))
    Subgroup_Success <- rbind(Subgroup_Success, .z)
    Subgroup_Termination <- rbind(Subgroup_Termination, .y)
    Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != Subgroups$group[[i]])
    Subgroups_Subset <- subset(Subgroups_Subset, Subgroups_Subset$group != Subgroups$group[[i]])
  }
}

Subgroups_Subset2 <- Subgroups_Subset

for(i in 1:(nrow(Subgroups_Subset))){
  if (nchar(Subgroups_Subset$group[[i]]) == 3){
    .a <- subset(Data_Subset, grepl(paste('^', Subgroups_Subset$group[[i]], sep = ""), Data_Subset$`ICD-10 Code`))
    .b <- dplyr::count(.a, Trial_Success)
    .z <- data.frame(Field = Subgroups_Subset$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups_Subset$disease[[i]], Termination = subset_termination(.b))
    Subgroup_Success <- rbind(Subgroup_Success, .z)
    Subgroup_Termination <- rbind(Subgroup_Termination, .y)
    Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, Subgroups_Subset$group[[i]]))
    Subgroups_Subset2 <- subset(Subgroups_Subset2, Subgroups_Subset2$group != Subgroups_Subset$group[[i]])
  }
}

Chapter_Success <- data.frame(matrix(ncol = 2, nrow = 0))
Chapter_Termination <- data.frame(matrix(ncol = 2, nrow = 0))
Subgroups_Subset3 <- Subgroups_Subset2

for(i in 1:(nrow(Subgroups_Subset2))){
  if (nchar(Subgroups_Subset2$group[[i]]) == 1){
    .a <- subset(Data_Subset, grepl(paste('^', Subgroups_Subset2$group[[i]], sep = ""), Data_Subset$`ICD-10 Code`))
    .b <- dplyr::count(.a, Trial_Success)
    .z <- data.frame(Field = Subgroups_Subset2$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = Subgroups_Subset2$disease[[i]], Termination = subset_termination(.b))
    Chapter_Success <- rbind(Chapter_Success, .z)
    Chapter_Termination <- rbind(Chapter_Termination, .y)
    Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, Subgroups_Subset2$group[[i]]))
    Subgroups_Subset3 <- subset(Subgroups_Subset3, Subgroups_Subset3$group != Subgroups_Subset2$group[[i]])
  }
}

#Calculate percentages of remaining non-grouped trials
.b <- dplyr::count(Data_Subset, Trial_Success)
.z <- data.frame(Field = 'Other', Success = Percentage_calc(.b))
.y <- data.frame(Field = 'Other', Termination = subset_termination(.b))
Chapter_Success <- rbind(Chapter_Success, .z)
Chapter_Termination <- rbind(Chapter_Termination, .y)

