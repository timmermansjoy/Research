#This script selects only the safety related trials and groups these based on their ICD-10 codes. Groups will have a minimum size of the "Threshold
#Threshold = 50

#Subset Safety Trials
Safety <- subset(Data, Safety == TRUE)

#ICD Data Frame
.ICD_Codes_Safety <- data.frame(doc_id = Safety$Id, code = Safety$`ICD-10 Code`, success = Safety$Trial_Success)

#Count Codes
Code_Counts_Safety <- dplyr::count(.ICD_Codes_Safety, code, sort=FALSE)

#Get codes with sufficient trials
Sufficient_Base_Safety <- subset(Code_Counts_Safety, n >= 50)