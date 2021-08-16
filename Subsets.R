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
  
  #add results into dataframe
  Subgroup_Success <- data.frame("Breast Cancer" = .Succes_Breast_Cancer)
  
  #Remove Breast Cancer from Data
  Data_Subset <- subset(Data, Data$`ICD-10 Code` != 'C50.9')

#The above steps will be repeated for each group
  
#C
C <- subset(Data_Subset, grepl("^C", Data_Subset$`ICD-10 Code`))
.suc_C <- dplyr::count(C, Trial_Success)
Subgroup_Success$Cancer <- Percentage_calc(.suc_C)  
#remove C from dataset

#G30.9
Alzheimer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'G30.9')
.suc_Alzheimer <- dplyr::count(Alzheimer, Trial_Success)
Subgroup_Success$Alzheimer <- Percentage_calc(.suc_Alzheimer)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'G30.9')
  
#G
G <- subset(Data_Subset, grepl("^G", Data_Subset$`ICD-10 Code`))
.suc_G <- dplyr::count(G, Trial_Success)
Subgroup_Success$"Nervous System" <- Percentage_calc_large(.suc_G)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != grepl("^G", Data_Subset$`ICD-10 Code`))
#removing "G" from the dataset does not work as of yet

