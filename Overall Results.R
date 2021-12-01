#This script investigates the success and trial design across all included clinical trials from the WHO clinical trial registry

#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
source("Utils.R")

#Import Data Set
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')

#ICD Data Frame
.ICD_Codes <- data.frame(doc_id = Data$Id, code = Data$`ICD-10 Code`, success = Data$`Trial_Success`, Randomised = Data$Randomized)

#Overall Results Percentage
.Termination <- dplyr::count(Data, Terminated)
.tot_perc_term <- Percentage_calc(.Termination)
Overall_Results <- data.frame(.tot_perc_term)
.succes_overall <-dplyr::count(.ICD_Codes, success)
Overall_Results$Tot_perc_suc <- Percentage_calc(.succes_overall)
.randomised_overall <- dplyr::count(Data, Randomized)
Overall_Results$Tot_perc_rand <- Percentage_calc(.randomised_overall)
.single_overall <- dplyr::count(Data, Single_Blind)
Overall_Results$Tot_perc_single <- Percentage_calc(.single_overall)
.double_overall <- dplyr::count(Data, Double_Blind)
Overall_Results$Tot_perc_double <- Percentage_calc(.double_overall)
.triple_overall <- dplyr::count(Data, Triple_blind)
Overall_Results$Tot_perc_triple <- Percentage_calc(.triple_overall)
Overall_Results$Tot_perc_blind <- Overall_Results$Tot_perc_triple + Overall_Results$Tot_perc_double + Overall_Results$Tot_perc_single
.control_overall <- dplyr::count(Data, Controlled)
Overall_Results$Tot_perc_control <- Percentage_calc(.control_overall)
.RCT <- subset(Data, Controlled == TRUE & Randomized == TRUE)
Overall_Results$tot_perc_RCT <- (nrow(.RCT)/nrow(Data))*100

#Termination reasons
Termination_Reason <- dplyr::count(Data, Notes)

#Efficacy Trials
Efficacy <- subset(Data, Safety == FALSE)

#Randomised vs non-randomised, Efficacy
.Randomised <- subset(Efficacy, Randomized == TRUE)
.Non_Randomised <- subset (Efficacy, Randomized == FALSE)
.success_randomised <- dplyr::count(.Randomised, Trial_Success)
.success_non_randomised <- dplyr::count(.Non_Randomised, Trial_Success)
.Randomised_Success <- Percentage_calc(.success_randomised)
.Non_Randomised_Success <- Percentage_calc(.success_non_randomised)

#Controlled vs Non-Controlled, Efficacy
.Controlled <- subset(Efficacy, Controlled == TRUE)
.Non_Controlled <- subset (Efficacy, Controlled == FALSE)
.success_Controlled <- dplyr::count(.Controlled, Trial_Success)
.success_non_Controlled <- dplyr::count(.Non_Controlled, Trial_Success)
.Controlled_Success <- Percentage_calc(.success_Controlled)
.Non_Controlled_Success <- Percentage_calc(.success_non_Controlled)

#RCT vs non-RCT, Efficacy
.non_RCT <- subset(Efficacy, Controlled == FALSE & Randomized == FALSE)
.RCT_efficacy <- subset(Efficacy, Controlled == TRUE & Randomized == TRUE)
.success_non_RCT <- dplyr::count(.non_RCT, .non_RCT$Trial_Success)
.Non_RCT_Success <- Percentage_calc(.success_non_RCT)
.success_RCT <- dplyr::count(.RCT_efficacy, Trial_Success)
.RCT_Success <- Percentage_calc(.success_RCT)

#Add Efficacy Results to a dataframe
Efficacy_Overall_Results <- data.frame('Controlled' = .Controlled_Success, 'Not Controlled' = .Non_Controlled_Success, 
                                       'Randomised' = .Randomised_Success, 'Not Randomised' = .Non_Randomised_Success,
                                       'RCT' = .RCT_Success, 'Not RCT' = .Non_RCT_Success)
