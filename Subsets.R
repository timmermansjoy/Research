#This script extracts the several groups (created in the script "Grouping.R") from the full dataset 
#and calculates the success within these groups


#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
library(reshape2)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')

#Extract C50.9: Breast Cancer
Breast_Cancer <- subset(Data, Data$`ICD-10 Code` == 'C50.9')

  #Count success
  .Suc_Breast_Cancer <- dplyr::count(Breast_Cancer, Trial_Success)

  #Calculate percentage success and termination and add to dataframe
  Subgroup_Success <- data.frame(Field = "Breast Cancer", Success = Percentage_calc(.Suc_Breast_Cancer))
  Subgroup_Termination <- data.frame(Field = "Breast Cancer", Termination = subset_termination(.Suc_Breast_Cancer))
  
  #Remove Breast Cancer from Data
  Data_Subset <- subset(Data, Data$`ICD-10 Code` != 'C50.9')

#The above steps will be repeated for each group
  
#C
C <- subset(Data_Subset, grepl("^C", Data_Subset$`ICD-10 Code`))
.suc_C <- dplyr::count(C, Trial_Success)
.z <- data.frame(Field = "Cancer", Success = Percentage_calc(.suc_C))
.y <- data.frame(Field = "Cancer", Termination = subset_termination(.suc_C))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
#remove C from dataset

#B18.2
Chronic_viral_hepatitis_C <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B18.2')
.suc_HepC <- dplyr::count(Chronic_viral_hepatitis_C, Trial_Success)
.z <- data.frame(Field = "Chronic Viral Hepatitis C", Success = Percentage_calc(.suc_HepC))
.y <- data.frame(Field = "Chronic Viral Hepatitis C", Termination = subset_termination(.suc_HepC))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B18.2')

#B24
HIV <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B24')
.suc_HIV <- dplyr::count(Chronic_viral_hepatitis_C, Trial_Success)
.z <- data.frame(Field = "HIV", Success = Percentage_calc(.suc_HIV))
.y <- data.frame(Field = "HIV", Termination = subset_termination(.suc_HIV))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B24')

#A and B
.A <- subset(Data_Subset, grepl("^A", Data_Subset$`ICD-10 Code`))
.B <- subset(Data_Subset, grepl("^B", Data_Subset$`ICD-10 Code`))
AB <- rbind(.A, .B)
.suc_AB <- dplyr::count(AB, Trial_Success)
.z <- data.frame(Field = "Infectious and Parasitic Disease", Success = Percentage_calc(.suc_AB))
.y <- data.frame(Field = "Infectious and Parasitic Disease", Termination = subset_termination(.suc_AB))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
#remove A and B from dataset

#G30.9
Alzheimer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'G30.9')
.suc_Alzheimer <- dplyr::count(Alzheimer, Trial_Success)
.z <- data.frame(Field = "Alzheimer", Success = Percentage_calc(.suc_Alzheimer))
.y <- data.frame(Field = "Alzheimer", Termination = subset_termination(.suc_Alzheimer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'G30.9')
  
#G
G <- subset(Data_Subset, grepl("^G", Data_Subset$`ICD-10 Code`))
.suc_G <- dplyr::count(G, Trial_Success)
.z <- data.frame(Field = "Nervous System", Success = Percentage_calc_large(.suc_G))
.y <- data.frame(Field = "Nervous System", Termination = subset_termination_large(.suc_G))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != grepl("^G", Data_Subset$`ICD-10 Code`))
#removing "G" from the dataset does not work as of yet


