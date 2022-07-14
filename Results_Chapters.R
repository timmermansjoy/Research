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
library(stringr)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')
Subgroups <- read_xlsx('Data/All_Chapters.xlsx')

#Create Empty Dataframes
Chapter_Success <- data.frame(matrix(ncol = 2, nrow = 0))
Chapter_Termination <- data.frame(matrix(ncol = 2, nrow = 0))
#Create malleable dataframes
.Data_Subset <- Data
.Subgroups_Subset <- Subgroups

for(i in 1:(nrow(Subgroups))){
    #subset code
    .a <- subset(Data, grepl(paste('^', .Subgroups_Subset$group[[i]], sep = ""), Data$`ICD-10 Code`))
    #Count Success and  Termination
    .b <- dplyr::count(.a, Trial_Success)
    .c <- dplyr::count(.a, Terminated)
    #Create dataframes with percentage positive outcomes and terminations
    .z <- data.frame(Field = .Subgroups_Subset$disease[[i]], Success = Percentage_calc(.b))
    .y <- data.frame(Field = .Subgroups_Subset$disease[[i]], Termination = Percentage_calc(.c))
    #Add percentages to full overview dataframe
    Chapter_Success <- rbind(Chapter_Success, .z)
    Chapter_Termination <- rbind(Chapter_Termination, .y)
    #Delete Used data from primary dataframes
    .Data_Subset <- subset(.Data_Subset, !str_detect(.Data_Subset$`ICD-10 Code`, .Subgroups_Subset$group[[i]]))
  }

#Write Results
Chapters_Results <- write_xlsx(Chapter_Success, "data//Chapter_Results.xlsx")