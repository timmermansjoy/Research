#This script calculates the overall results for all safety trials


#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')

#Subset Safety Trials
Safety <- subset(Data, Safety == TRUE)

#Overall Results Percentage
.Termination_safety <- dplyr::count(Safety, Terminated)
.tot_perc_term <- Percentage_calc(.Termination_safety)
Design_Results_Safety <- data.frame(.tot_perc_term)
.succes_overall_safety <-dplyr::count(.ICD_Codes_Safety, success)
Design_Results_Safety$Tot_perc_suc <- Percentage_calc(.succes_overall_safety)
.randomised_overall_safety <- dplyr::count(Safety, Randomized)
Design_Results_Safety$Tot_perc_rand <- Percentage_calc(.randomised_overall_safety)
.single_overall_safety <- dplyr::count(Safety, Single_Blind)
Design_Results_Safety$Tot_perc_single <- Percentage_calc(.single_overall_safety)
.double_overall_safety <- dplyr::count(Safety, Double_Blind)
Design_Results_Safety$Tot_perc_double <- Percentage_calc(.double_overall_safety)
.triple_overall_safety <- dplyr::count(Safety, Triple_blind)
Design_Results_Safety$Tot_perc_triple <- Percentage_calc(.triple_overall_safety)
Design_Results_Safety$Tot_perc_blind <- Design_Results_Safety$Tot_perc_triple + Design_Results_Safety$Tot_perc_double + Design_Results_Safety$Tot_perc_single
.control_overall_safety <- dplyr::count(Safety, Controlled)
Design_Results_Safety$Tot_perc_control <- Percentage_calc(.control_overall_safety)
.RCT_safety <- subset(Safety, Controlled == TRUE & Randomized == TRUE)
Design_Results_Safety$tot_perc_RCT <- (nrow(.RCT_safety)/nrow(Safety))*100

#Randomised vs non-randomised
.Randomised_safety <- subset(Safety, Randomized == TRUE)
.Non_Randomised_safety <- subset (Safety, Randomized == FALSE)
.success_randomised_safety <- dplyr::count(.Randomised_safety, Trial_Success)
.success_non_randomised_safety <- dplyr::count(.Non_Randomised_safety, Trial_Success)
.Randomised_Success_safety <- Percentage_calc(.success_randomised_safety)
.Non_Randomised_Success_safety <- Percentage_calc(.success_non_randomised_safety)

#Controlled vs Non-Controlled
.Controlled_safety <- subset(Safety, Controlled == TRUE)
.Non_Controlled_safety <- subset (Safety, Controlled == FALSE)
.success_Controlled_safety <- dplyr::count(.Controlled_safety, Trial_Success)
.success_non_Controlled_safety <- dplyr::count(.Non_Controlled_safety, Trial_Success)
.Controlled_Success_safety <- Percentage_calc(.success_Controlled_safety)
.Non_Controlled_Success_safety <- Percentage_calc(.success_non_Controlled_safety)

#RCT vs non-RCT
.non_RCT_safety <- subset(Safety, Controlled == FALSE & Randomized == FALSE)
.success_non_RCT_safety <- dplyr::count(.non_RCT_safety, .non_RCT_safety$Trial_Success)
.Non_RCT_Success_safety <- Percentage_calc(.success_non_RCT_safety)
.success_RCT_safety <- dplyr::count(.RCT_safety, .RCT_safety$Trial_Success)
.RCT_Success_safety <- Percentage_calc(.success_RCT_safety)

#Add Safety Results to a dataframe
Safety_Overall_Results <- data.frame('Controlled' = .Controlled_Success_safety, 'Not Controlled' = .Non_Controlled_Success_safety, 
                                       'Randomised' = .Randomised_Success_safety, 'Not Randomised' = .Non_Randomised_Success_safety,
                                       'RCT' = .RCT_Success_safety, 'Not RCT' = .Non_RCT_Success_safety)

