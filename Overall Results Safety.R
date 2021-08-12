#This script calculates the overall results for all safety trials

#Subset Safety Trials
Safety <- subset(Data, Safety == TRUE)

#Overall Results Percentage
Overall_Results_Safety <- data.frame(1)
.Termination_safety <- dplyr::count(Safety, Terminated)
Overall_Results_Safety$Tot_perc_term <- (.Termination_safety$n[[2]]/(.Termination_safety$n[[1]]+.Termination_safety$n[[2]]))*100
Overall_Results_Safety$X1 <- NULL
.succes_overall_safety <-dplyr::count(.ICD_Codes_Safety, success)
Overall_Results_Safety$Tot_perc_suc <- ((.succes_overall_safety$n[[2]]/(.succes_overall_safety$n[[2]]+.succes_overall_safety$n[[1]]))*100)
.randomised_overall_safety <- dplyr::count(Safety, Randomized)
Overall_Results_Safety$Tot_perc_rand <- ((.randomised_overall_safety$n[[2]]/(.randomised_overall_safety$n[[1]]+.randomised_overall_safety$n[[2]]))*100)
.single_overall_safety <- dplyr::count(Safety, Single_Blind)
Overall_Results_Safety$Tot_perc_single <- ((.single_overall_safety$n[[2]]/(.single_overall_safety$n[[1]]+.single_overall_safety$n[[2]]))*100)
.double_overall_safety <- dplyr::count(Safety, Double_Blind)
Overall_Results_Safety$Tot_perc_double <- ((.double_overall_safety$n[[2]]/(.double_overall_safety$n[[1]]+.double_overall_safety$n[[2]]))*100)
.triple_overall_safety <- dplyr::count(Safety, Triple_blind)
Overall_Results_Safety$Tot_perc_triple <- ((.triple_overall_safety$n[[2]]/(.triple_overall_safety$n[[1]]+.triple_overall_safety$n[[2]]))*100)
Overall_Results_Safety$Tot_perc_blind <- Overall_Results_Safety$Tot_perc_triple + Overall_Results_Safety$Tot_perc_double + Overall_Results_Safety$Tot_perc_single
.control_overall_safety <- dplyr::count(Safety, Controlled)
Overall_Results_Safety$Tot_perc_control <- ((.control_overall_safety$n[[2]]/(.control_overall_safety$n[[1]]+.control_overall_safety$n[[2]]))*100)
.RCT_safety <- subset(Safety, Controlled == TRUE & Randomized == TRUE)
Overall_Results_Safety$tot_perc_RCT <- (nrow(.RCT_safety)/nrow(Safety))*100

#Randomised vs non-randomised
.Randomised_safety <- subset(Safety, Randomized == TRUE)
.Non_Randomised_safety <- subset (Safety, Randomized == FALSE)
.success_randomised_safety <- dplyr::count(.Randomised_safety, Trial_Success)
.success_non_randomised_safety <- dplyr::count(.Non_Randomised_safety, Trial_Success)
Randomised_Success_safety <- (.success_randomised_safety$n[[2]]/(.success_randomised_safety$n[[1]]+.success_randomised_safety$n[[2]]))*100
Non_Randomised_Success_safety <- (.success_non_randomised_safety$n[[2]]/(.success_non_randomised_safety$n[[2]]+.success_non_randomised_safety$n[[1]]))*100

#Controlled vs Non-Controlled
.Controlled_safety <- subset(Safety, Controlled == TRUE)
.Non_Controlled_safety <- subset (Safety, Controlled == FALSE)
.success_Controlled_safety <- dplyr::count(.Controlled_safety, Trial_Success)
.success_non_Controlled_safety <- dplyr::count(.Non_Controlled_safety, Trial_Success)
Controlled_Success_safety <- (.success_Controlled_safety$n[[2]]/(.success_Controlled_safety$n[[2]]+.success_Controlled_safety$n[[1]]))*100
Non_Controlled_Success_safety <- (.success_non_Controlled_safety$n[[2]]/(.success_non_Controlled_safety$n[[2]]+.success_non_Controlled_safety$n[[1]]))*100

#RCT vs non-RCT
.RCT_safety <- subset(Safety, Controlled == TRUE & Randomized == TRUE)
.non_RCT_safety <- subset(Safety, Controlled == FALSE & Randomized == FALSE)
.success_non_RCT_safety <- dplyr::count(.non_RCT_safety, .non_RCT_safety$Trial_Success)
Non_RCT_Success_safety <- (.success_non_RCT_safety$n[[2]]/(.success_non_RCT_safety$n[[2]]+.success_non_RCT_safety$n[[1]])*100)
.success_RCT_safety <- dplyr::count(.RCT_safety, .RCT_safety$Trial_Success)
RCT_Success_safety <- (.success_RCT_safety$n[[2]]/(.success_RCT_safety$n[[2]]+.success_RCT_safety$n[[1]])*100)

