#This script extracts the several groups (created in the script "Grouping.R") from the full dataset 
#and calculates the success within these groups


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

#Extract C50.9: Breast Cancer
.Breast_Cancer <- subset(Data, Data$`ICD-10 Code` == 'C50.9')

  #Count success
  .Suc_Breast_Cancer <- dplyr::count(.Breast_Cancer, Trial_Success)

  #Calculate percentage success and termination and add to dataframe
  Subgroup_Success <- data.frame(Field = "Cancer, Breast", Success = Percentage_calc(.Suc_Breast_Cancer))
  Subgroup_Termination <- data.frame(Field = "Cancer, Breast", Termination = subset_termination(.Suc_Breast_Cancer))
  
  #Remove Breast Cancer from Data
  Data_Subset <- subset(Data, Data$`ICD-10 Code` != 'C50.9')

#The above steps will be repeated for each group

#B18.2
.Chronic_viral_hepatitis_C <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B18.2')
.suc_HepC <- dplyr::count(.Chronic_viral_hepatitis_C, Trial_Success)
#create temporary dataframes for percentages in the current group
#Z being for positive outcomes, Y for Termination
.z <- data.frame(Field = "Chronic Viral Hepatitis C", Success = Percentage_calc(.suc_HepC))
.y <- data.frame(Field = "Chronic Viral Hepatitis C", Termination = subset_termination(.suc_HepC))
#Add temporary dataframes to permanent outcome dataframes
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B18.2')

#B24
.HIV <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'B24')
.suc_HIV <- dplyr::count(.HIV, Trial_Success)
.z <- data.frame(Field = "HIV", Success = Percentage_calc(.suc_HIV))
.y <- data.frame(Field = "HIV", Termination = subset_termination(.suc_HIV))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'B24')

#A and B
.A <- subset(Data_Subset, grepl("^A", Data_Subset$`ICD-10 Code`))
.B <- subset(Data_Subset, grepl("^B", Data_Subset$`ICD-10 Code`))
.AB <- rbind(.A, .B)
.suc_AB <- dplyr::count(.AB, Trial_Success)
Success_Chapter <- data.frame(Field = "Infectious and Parasitic Disease, other", Success = Percentage_calc(.suc_AB))
Termination_Chapter <- data.frame(Field = "Infectious and Parasitic Disease, other", Termination = subset_termination(.suc_AB))
Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, "A"))
Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, "B"))
#remove A and B from dataset

#C16
.C16 <- subset(Data_Subset, Data_Subset$'ICD-10 Code' == 'C16.0' | Data_Subset$'ICD-10 Code' == 'C16.1' | 
                Data_Subset$'ICD-10 Code' == 'C16.2' | Data_Subset$'ICD-10 Code' == 'C16.3' | Data_Subset$'ICD-10 Code' == 'C16.4' | 
                Data_Subset$'ICD-10 Code' == 'C16.5' | Data_Subset$'ICD-10 Code' == 'C16.6' | Data_Subset$'ICD-10 Code' == 'C16.7' | 
                Data_Subset$'ICD-10 Code' == 'C16.8'| Data_Subset$'ICD-10 Code' == 'C16.9')
.suc_Stomach_Cancer <- dplyr::count(.C16, Trial_Success)
.z <- data.frame(Field = "Cancer, Stomach", Success = Percentage_calc(.suc_Stomach_Cancer))
.y <- data.frame(Field = "Cancer, Stomach" , Termination = subset_termination(.suc_Stomach_Cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$'ICD-10 Code' != 'C16.0' & Data_Subset$'ICD-10 Code' != 'C16.1' & 
                        Data_Subset$'ICD-10 Code' != 'C16.2' & Data_Subset$'ICD-10 Code' != 'C16.3' & Data_Subset$'ICD-10 Code' != 'C16.4' & 
                        Data_Subset$'ICD-10 Code' != 'C16.5' & Data_Subset$'ICD-10 Code' != 'C16.6' & Data_Subset$'ICD-10 Code' != 'C16.7' & 
                        Data_Subset$'ICD-10 Code' != 'C16.8' & Data_Subset$'ICD-10 Code' != 'C16.9')

#C18.9
.colon_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C18.9')
.suc_colon_cancer <- dplyr::count(.colon_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Colon", Success = Percentage_calc(.suc_colon_cancer))
.y <- data.frame(Field = "Cancer, Colon", Termination = subset_termination(.suc_colon_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C18.9')

#C22
.C22 <- subset(Data_Subset, Data_Subset$'ICD-10 Code' == 'C22.0' | Data_Subset$'ICD-10 Code' == 'C22.1' | 
                 Data_Subset$'ICD-10 Code' == 'C22.2' | Data_Subset$'ICD-10 Code' == 'C22.3' | Data_Subset$'ICD-10 Code' == 'C22.4' | 
                 Data_Subset$'ICD-10 Code' == 'C22.5' | Data_Subset$'ICD-10 Code' == 'C22.6' | Data_Subset$'ICD-10 Code' == 'C22.7' | 
                 Data_Subset$'ICD-10 Code' == 'C22.8'| Data_Subset$'ICD-10 Code' == 'C22.9')
.suc_liver_Cancer <- dplyr::count(.C22, Trial_Success)
.z <- data.frame(Field = "Cancer, Liver", Success = Percentage_calc(.suc_liver_Cancer))
.y <- data.frame(Field = "Cancer, Liver" , Termination = subset_termination(.suc_liver_Cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$'ICD-10 Code' != 'C22.0' & Data_Subset$'ICD-10 Code' != 'C22.1' & 
                        Data_Subset$'ICD-10 Code' != 'C22.2' & Data_Subset$'ICD-10 Code' != 'C22.3' & Data_Subset$'ICD-10 Code' != 'C22.4' & 
                        Data_Subset$'ICD-10 Code' != 'C22.5' & Data_Subset$'ICD-10 Code' != 'C22.6' & Data_Subset$'ICD-10 Code' != 'C22.7' & 
                        Data_Subset$'ICD-10 Code' != 'C22.8' & Data_Subset$'ICD-10 Code' != 'C22.9')
#C25.9
.pancreas_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C25.9')
.suc_pancreas_cancer <- dplyr::count(.pancreas_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Pancreas", Success = Percentage_calc(.suc_pancreas_cancer))
.y <- data.frame(Field = "Cancer, Pancreas", Termination = subset_termination(.suc_pancreas_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C25.9')

#C34.9
.lung_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C34.9')
.suc_lung_cancer <- dplyr::count(.lung_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Lung", Success = Percentage_calc(.suc_lung_cancer))
.y <- data.frame(Field = "Cancer, Lung", Termination = subset_termination(.suc_lung_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C34.9')

#C56
.Ovary_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C56')
.suc_Ovary_cancer <- dplyr::count(.Ovary_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Ovary", Success = Percentage_calc(.suc_Ovary_cancer))
.y <- data.frame(Field = "Cancer, Ovary", Termination = subset_termination(.suc_Ovary_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C56')

#C61
.Prostate_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C61')
.suc_Prostate_cancer <- dplyr::count(.Prostate_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Prostate", Success = Percentage_calc(.suc_Prostate_cancer))
.y <- data.frame(Field = "Cancer, Prostate", Termination = subset_termination(.suc_Prostate_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C61')

#C71.9
.Brain_cancer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C71.9')
.suc_Brain_cancer <- dplyr::count(.Brain_cancer, Trial_Success)
.z <- data.frame(Field = "Cancer, Brain", Success = Percentage_calc(.suc_Brain_cancer))
.y <- data.frame(Field = "Cancer, Brain", Termination = subset_termination(.suc_Brain_cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C71.9')

#C80.9
.cancer_NOS <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C80.9')
.suc_cancer_NOS <- dplyr::count(.cancer_NOS, Trial_Success)
.z <- data.frame(Field = "Cancer, Unspecified", Success = Percentage_calc(.suc_cancer_NOS))
.y <- data.frame(Field = "Cancer, Unspecified", Termination = subset_termination(.suc_cancer_NOS))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C80.9')

#C83
.C83 <- subset(Data_Subset, Data_Subset$'ICD-10 Code' == 'C83.0' | Data_Subset$'ICD-10 Code' == 'C83.1' | 
                 Data_Subset$'ICD-10 Code' == 'C83.2' | Data_Subset$'ICD-10 Code' == 'C83.3' | Data_Subset$'ICD-10 Code' == 'C83.4' | 
                 Data_Subset$'ICD-10 Code' == 'C83.5' | Data_Subset$'ICD-10 Code' == 'C83.6' | Data_Subset$'ICD-10 Code' == 'C83.7' | 
                 Data_Subset$'ICD-10 Code' == 'C83.8'| Data_Subset$'ICD-10 Code' == 'C83.9')
.suc_lymphoma_Cancer <- dplyr::count(.C83, Trial_Success)
.z <- data.frame(Field = "Cancer, Non-follicular lymphoma", Success = Percentage_calc(.suc_lymphoma_Cancer))
.y <- data.frame(Field = "Cancer, Non-follicular lymphoma" , Termination = subset_termination(.suc_lymphoma_Cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$'ICD-10 Code' != 'C83.0' & Data_Subset$'ICD-10 Code' != 'C83.1' & 
                        Data_Subset$'ICD-10 Code' != 'C83.2' & Data_Subset$'ICD-10 Code' != 'C83.3' & Data_Subset$'ICD-10 Code' != 'C83.4' & 
                        Data_Subset$'ICD-10 Code' != 'C83.5' & Data_Subset$'ICD-10 Code' != 'C83.6' & Data_Subset$'ICD-10 Code' != 'C83.7' & 
                        Data_Subset$'ICD-10 Code' != 'C83.8' & Data_Subset$'ICD-10 Code' != 'C83.9')

#C85.9
.cancer_NHlymphoma <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C85.9')
.suc_NHlymphoma_cancer <- dplyr::count(.cancer_NHlymphoma , Trial_Success)
.z <- data.frame(Field = "Cancer, Lymphoma Unspecified", Success = Percentage_calc(.suc_NHlymphoma_cancer ))
.y <- data.frame(Field = "Cancer, Lymphoma Unspecified", Termination = subset_termination(.suc_NHlymphoma_cancer ))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C85.9')

#C90.0
.cancer_MultipleMyeloma<- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C90.0')
.suc_MultipleMyeloma_cancer <- dplyr::count(.cancer_MultipleMyeloma , Trial_Success)
.z <- data.frame(Field = "Cancer, Multiple Myeloma", Success = Percentage_calc(.suc_MultipleMyeloma_cancer ))
.y <- data.frame(Field = "Cancer, Multiple Myeloma", Termination = subset_termination(.suc_MultipleMyeloma_cancer ))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C90.0')

#C91
.C91 <- subset(Data_Subset, Data_Subset$'ICD-10 Code' == 'C91.0' | Data_Subset$'ICD-10 Code' == 'C91.1' | 
                 Data_Subset$'ICD-10 Code' == 'C91.2' | Data_Subset$'ICD-10 Code' == 'C91.3' | Data_Subset$'ICD-10 Code' == 'C91.4' | 
                 Data_Subset$'ICD-10 Code' == 'C91.5' | Data_Subset$'ICD-10 Code' == 'C91.6' | Data_Subset$'ICD-10 Code' == 'C91.7' | 
                 Data_Subset$'ICD-10 Code' == 'C91.8'| Data_Subset$'ICD-10 Code' == 'C91.9')
.suc_leukaemia_Cancer <- dplyr::count(.C91, Trial_Success)
.z <- data.frame(Field = "Cancer, Lymphoid leukaemia", Success = Percentage_calc(.suc_leukaemia_Cancer))
.y <- data.frame(Field = "Cancer, Lymphoid leukaemia" , Termination = subset_termination(.suc_leukaemia_Cancer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$'ICD-10 Code' != 'C91.0' & Data_Subset$'ICD-10 Code' != 'C91.1' & 
                        Data_Subset$'ICD-10 Code' != 'C91.2' & Data_Subset$'ICD-10 Code' != 'C91.3' & Data_Subset$'ICD-10 Code' != 'C91.4' & 
                        Data_Subset$'ICD-10 Code' != 'C91.5' & Data_Subset$'ICD-10 Code' != 'C91.6' & Data_Subset$'ICD-10 Code' != 'C91.7' & 
                        Data_Subset$'ICD-10 Code' != 'C91.8' & Data_Subset$'ICD-10 Code' != 'C91.9')

#C94.2
.cancer_AML<- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C94.2')
.suc_AML_cancer <- dplyr::count(.cancer_AML , Trial_Success)
.z <- data.frame(Field = "Cancer, Acute Myeloid Leukaemia", Success = Percentage_calc(.suc_AML_cancer ))
.y <- data.frame(Field = "Cancer, Acute Myeloid Leukaemia", Termination = subset_termination(.suc_AML_cancer ))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C94.2')

#C95.9
.cancer_Leukaemia_NOS<- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'C95.9')
.suc_Leukaemia_NOS_cancer <- dplyr::count(.cancer_Leukaemia_NOS , Trial_Success)
.z <- data.frame(Field = "Cancer, Leukaemia Unspecified", Success = Percentage_calc(.suc_Leukaemia_NOS_cancer ))
.y <- data.frame(Field = "Cancer, Leukaemia Unspecified", Termination = subset_termination(.suc_Leukaemia_NOS_cancer ))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'C95.9')

#C
.C <- subset(Data_Subset, grepl("^C", Data_Subset$`ICD-10 Code`))
.suc_C <- dplyr::count(.C, Trial_Success)
.z <- data.frame(Field = "Cancer, other", Success = Percentage_calc(.suc_C))
.y <- data.frame(Field = "Cancer, other", Termination = subset_termination(.suc_C))
Success_Chapter <- rbind(Success_Chapter, .z)
Termination_Chapter <- rbind(Termination_Chapter, .y)
Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, "C"))
#remove C from dataset

#D
#E11.9
#E14
#E
#F33.9
#F

#G30.9
.Alzheimer <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'G30.9')
.suc_Alzheimer <- dplyr::count(.Alzheimer, Trial_Success)
.z <- data.frame(Field = "Alzheimer", Success = Percentage_calc(.suc_Alzheimer))
.y <- data.frame(Field = "Alzheimer", Termination = subset_termination(.suc_Alzheimer))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'G30.9')
  
#G
.G <- subset(Data_Subset, grepl("^G", Data_Subset$`ICD-10 Code`))
.suc_G <- dplyr::count(.G, Trial_Success)
.z <- data.frame(Field = "Nervous System, other", Success = Percentage_calc_large(.suc_G))
.y <- data.frame(Field = "Nervous System, other", Termination = subset_termination_large(.suc_G))
Success_Chapter <- rbind(Success_Chapter, .z)
Termination_Chapter <- rbind(Termination_Chapter, .y)
Data_Subset <- subset(Data_Subset, !str_detect(Data_Subset$`ICD-10 Code`, "G"))

#H35
#H
#I
#J11.1
.Influenza <- subset(Data_Subset, Data_Subset$`ICD-10 Code` == 'J11.1')
.suc_Influenza <- dplyr::count(.Influenza, Trial_Success)
.z <- data.frame(Field = "Influenza", Success = Percentage_calc(.suc_Influenza))
.y <- data.frame(Field = "Influenza", Termination = subset_termination(.suc_Influenza))
Subgroup_Success <- rbind(Subgroup_Success, .z)
Subgroup_Termination <- rbind(Subgroup_Termination, .y)
Data_Subset <- subset(Data_Subset, Data_Subset$`ICD-10 Code` != 'J11.1')

#J44.9
#J45.9
#J
#K
#L40
#L
#M06.9
#M
#N
#R52.9
#T
#NA
#Others