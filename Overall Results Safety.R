#This script calculates the overall results for all safety trials


#Packages
library(readxl)
library(dplyr)
library(sjmisc)
library(ggplot2)
source("Utils.R")

#Import Data Set
getwd()
#if wd is (...)/Research, use Data/Preliminary_Data.xlsx . Otherwise, use Preliminary_Data.xlsx.
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')


#Subset Safety Trials
Safety <- subset(Data, Safety == TRUE)

#Overall Results Percentage
Overall_Results_Safety <- data.frame(1)
.Termination_safety <- dplyr::count(Safety, Terminated)
Overall_Results_Safety$Tot_perc_term <- Percentage_calc(.Termination_safety)
Overall_Results_Safety$X1 <- NULL
.succes_overall_safety <-dplyr::count(.ICD_Codes_Safety, success)
Overall_Results_Safety$Tot_perc_suc <- Percentage_calc(.succes_overall_safety)
.randomised_overall_safety <- dplyr::count(Safety, Randomized)
Overall_Results_Safety$Tot_perc_rand <- Percentage_calc(.randomised_overall_safety)
.single_overall_safety <- dplyr::count(Safety, Single_Blind)
Overall_Results_Safety$Tot_perc_single <- Percentage_calc(.single_overall_safety)
.double_overall_safety <- dplyr::count(Safety, Double_Blind)
Overall_Results_Safety$Tot_perc_double <- Percentage_calc(.double_overall_safety)
.triple_overall_safety <- dplyr::count(Safety, Triple_blind)
Overall_Results_Safety$Tot_perc_triple <- Percentage_calc(.triple_overall_safety)
Overall_Results_Safety$Tot_perc_blind <- Overall_Results_Safety$Tot_perc_triple + Overall_Results_Safety$Tot_perc_double + Overall_Results_Safety$Tot_perc_single
.control_overall_safety <- dplyr::count(Safety, Controlled)
Overall_Results_Safety$Tot_perc_control <- Percentage_calc(.control_overall_safety)
.RCT_safety <- subset(Safety, Controlled == TRUE & Randomized == TRUE)
Overall_Results_Safety$tot_perc_RCT <- (nrow(.RCT_safety)/nrow(Safety))*100

#Randomised vs non-randomised
.Randomised_safety <- subset(Safety, Randomized == TRUE)
.Non_Randomised_safety <- subset (Safety, Randomized == FALSE)
.success_randomised_safety <- dplyr::count(.Randomised_safety, Trial_Success)
.success_non_randomised_safety <- dplyr::count(.Non_Randomised_safety, Trial_Success)
Randomised_Success_safety <- Percentage_calc(.success_randomised_safety)
Non_Randomised_Success_safety <- Percentage_calc(.success_non_randomised_safety)

#Controlled vs Non-Controlled
.Controlled_safety <- subset(Safety, Controlled == TRUE)
.Non_Controlled_safety <- subset (Safety, Controlled == FALSE)
.success_Controlled_safety <- dplyr::count(.Controlled_safety, Trial_Success)
.success_non_Controlled_safety <- dplyr::count(.Non_Controlled_safety, Trial_Success)
Controlled_Success_safety <- Percentage_calc(.success_Controlled_safety)
Non_Controlled_Success_safety <- Percentage_calc(.success_non_Controlled_safety)

#RCT vs non-RCT
.RCT_safety <- subset(Safety, Controlled == TRUE & Randomized == TRUE)
.non_RCT_safety <- subset(Safety, Controlled == FALSE & Randomized == FALSE)
.success_non_RCT_safety <- dplyr::count(.non_RCT_safety, .non_RCT_safety$Trial_Success)
Non_RCT_Success_safety <- Percentage_calc(.success_non_RCT_safety)
.success_RCT_safety <- dplyr::count(.RCT_safety, .RCT_safety$Trial_Success)
RCT_Success_safety <- Percentage_calc(.success_RCT_safety)

