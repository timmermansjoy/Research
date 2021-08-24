#This script extracts the several groups (created in the script "Grouping.R") from the full dataset 
#and calculates the success within these groups


#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')

#Extract C50.9: Breast Cancer
Breast_Cancer <- subset(Data, Data$`ICD-10 Code` == 'C50.9')

  #Percentage positive outcomes
  .Suc_Breast_Cancer <- dplyr::count(Breast_Cancer, Trial_Success)
  .Succes_Breast_Cancer <- Percentage_calc(.Suc_Breast_Cancer)
  
  #Percentage Terminations
  .term_Breast_Cancer <- subset_termination(.Suc_Breast_Cancer)
  
  #add results into dataframe
  Subgroup_Success <- data.frame("Breast Cancer" = .Succes_Breast_Cancer)
  Subgroup_Termination <- data.frame("Breast Cancer" = .term_Breast_Cancer)
  
  #Remove Breast Cancer from Data
  Data_Subset <- subset(Data, Data$`ICD-10 Code` != 'C50.9')

#The above steps will be repeated for each group
  
#C
C <- subset(Data_Subset, grepl("^C", Data_Subset$`ICD-10 Code`))
.suc_C <- dplyr::count(C, Trial_Success)
Subgroup_Success$Cancer <- Percentage_calc(.suc_C) 
Subgroup_Termination$Cancer <- subset_termination(.suc_C)
#remove C from dataset

#B18.2
Chronic_viral_hepatitis_C <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B18.2')
.suc_HepC <- dplyr::count(Chronic_viral_hepatitis_C, Trial_Success)
Subgroup_Success$"Chronic Viral Hepatitis C" <- Percentage_calc(.suc_HepC)
Subgroup_Termination$"Chronic Viral Hepatitis C" <- subset_termination(.suc_HepC)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B18.2')

#B24
HIV <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B24')
.suc_HIV <- dplyr::count(Chronic_viral_hepatitis_C, Trial_Success)
Subgroup_Success$HIV<- Percentage_calc(.suc_HIV)
Subgroup_Termination$HIV <- subset_termination(.suc_HIV)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B24')

#A and B
.A <- subset(Data_Subset, grepl("^A", Data_Subset$`ICD-10 Code`))
.B <- subset(Data_Subset, grepl("^B", Data_Subset$`ICD-10 Code`))
AB <- rbind(.A, .B)
.suc_AB <- dplyr::count(AB, Trial_Success)
Subgroup_Success$"Infectious and Parasitic Disease" <- Percentage_calc(.suc_AB) 
Subgroup_Termination$"Infectious and Parasitic Disease" <- subset_termination(.suc_AB)
#remove A and B from dataset

#G30.9
Alzheimer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'G30.9')
.suc_Alzheimer <- dplyr::count(Alzheimer, Trial_Success)
Subgroup_Success$Alzheimer <- Percentage_calc(.suc_Alzheimer)
Subgroup_Termination$Alzheimer <- subset_termination(.suc_Alzheimer)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'G30.9')
  
#G
G <- subset(Data_Subset, grepl("^G", Data_Subset$`ICD-10 Code`))
.suc_G <- dplyr::count(G, Trial_Success)
Subgroup_Success$"Nervous System" <- Percentage_calc_large(.suc_G)
Subgroup_Termination$"Nervous System" <- subset_termination_large(.suc_G)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != grepl("^G", Data_Subset$`ICD-10 Code`))
#removing "G" from the dataset does not work as of yet

