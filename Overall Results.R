#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
library(ggplot2)

#Import Data Set
getwd()
#if wd is (...)/Research, use Data/Preliminary_Data.xlsx . Otherwise, use Preliminary_Data.xlsx.
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')

#ICD Data Frame
.ICD_Codes <- data.frame(doc_id = Data$Id, code = Data$`ICD-10 Code`, success = Data$`Trial_Success`, Randomised = Data$Randomized)

#Overall Results Percentage
Overall_Results <- data.frame(1)
.Termination <- dplyr::count(Data, Terminated)
Overall_Results$Tot_perc_term <- (.Termination$n[[2]]/(.Termination$n[[1]]+.Termination$n[[2]]))*100
Overall_Results$X1 <- NULL
.succes_overall <-dplyr::count(.ICD_Codes, success)
Overall_Results$Tot_perc_suc <- ((.succes_overall$n[[3]]/(.succes_overall$n[[2]]+.succes_overall$n[[3]]))*100)
.randomised_overall <- dplyr::count(Data, Randomized)
Overall_Results$Tot_perc_rand <- ((.randomised_overall$n[[2]]/(.randomised_overall$n[[1]]+.randomised_overall$n[[2]]))*100)
.single_overall <- dplyr::count(Data, Single_Blind)
Overall_Results$Tot_perc_single <- ((.single_overall$n[[2]]/(.single_overall$n[[1]]+.single_overall$n[[2]]))*100)
.double_overall <- dplyr::count(Data, Double_Blind)
Overall_Results$Tot_perc_double <- ((.double_overall$n[[2]]/(.double_overall$n[[1]]+.double_overall$n[[2]]))*100)
.triple_overall <- dplyr::count(Data, Triple_blind)
Overall_Results$Tot_perc_triple <- ((.triple_overall$n[[2]]/(.triple_overall$n[[1]]+.triple_overall$n[[2]]))*100)
Overall_Results$Tot_perc_blind <- Overall_Results$Tot_perc_triple + Overall_Results$Tot_perc_double + Overall_Results$Tot_perc_single
.control_overall <- dplyr::count(Data, Controlled)
Overall_Results$Tot_perc_control <- ((.control_overall$n[[2]]/(.control_overall$n[[1]]+.control_overall$n[[2]]))*100)

#Randomised vs non-randomised
Randomised <- .ICD_Codes[which(.ICD_Codes$Randomised == "TRUE")]
