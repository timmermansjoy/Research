#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)

#Import Data Set
getwd()
#if wd is (...)/Research, use Data/Preliminary_Data.xlsx . Otherwise, use Preliminary_Data.xlsx.
Data<-read_xlsx('Data/Prelimenary_Data.xlsx')


#ICD Data Frame
ICD_Codes <- data.frame(doc_id = Data$Id, code = Data$`ICD-10 Code`, success = Data$`Trial Success`)

#Count Codes
Code_Counts <- dplyr::count(ICD_Codes, code, sort=FALSE)

#Overall Results
Overall_Results <- data.frame(1)

.Termination <- dplyr::count(Data, Terminated)
Overall_Results$Tot_perc_term <- (.Termination$n[[2]]/(.Termination$n[[1]]+.Termination$n[[2]]))*100

Overall_Results$X1 <- NULL

.succes_overall <-dplyr::count(ICD_Codes, success)
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

#Get codes with sufficient trials
Sufficient_Base <- subset(Code_Counts, n >= 50)

#Group insufficient cases on first three characters
.old <- Code_Counts$code[1]
.nums <- 0
.total <- 0
group_names <- rep(NA, nrow(Code_Counts))
.treshhold <- 50

Grouped_Codes <- data.frame(matrix(ncol = 2, nrow = 0))
.x <- c("group", "total")
colnames(Grouped_Codes) <- .x

  #go trough each element
for(i in 2:(nrow(Code_Counts))){
  if(Code_Counts$n[i] < .treshhold && Code_Counts$n[i-1] < .treshhold){
    #check if code is similar as the one above it
    if( str_contains(Code_Counts$code[i], substr(.old, 0, 3)) ){
      # if it is the first element that is similar also take the one above
      if (.nums == 0){
        .total = .total + Code_Counts$n[i-1]
        group_names[i-1] = substr(.old, 0, 3)
      }
      
      group_names[i] = substr(.old, 0, 3)
      .total = .total + Code_Counts$n[i]
      .nums = .nums + 1
    }
    else{
      if (.nums != 0){
        y <- data.frame(group=substr(.old, 0, 3), .total = .total)
        Grouped_Codes <- rbind(Grouped_Codes, y)
      }
      #Reset Variables
      .nums = 0
      .total = 0
      
      #Add standard name to the group
      group_names[i] = Code_Counts$code[i]
      #Current code in old variable for use in the next loop
      .old = Code_Counts$code[i]
      
    }
  }
  if(is.na(group_names[i])){
    group_names[i] = Code_Counts$code[i]
  }
}
#If, after original grouping, the group size is still smaller than treshhold: replace group with just the first letter
for(i in 1:(nrow(Grouped_Codes))){
  if(Grouped_Codes$.total[i]<.treshhold){
    Grouped_Codes$group[i] = substr(Grouped_Codes$group[i], 0, 1)
  }
}
#group Grouped_Codes to sum together all entries by group
Grouped_Codes = aggregate(Grouped_Codes$.total, by=list(Category = Grouped_Codes$group), FUN=sum)
colnames(Grouped_Codes) <- .x

#group everything that's still too small
for(i in 1:(nrow(Grouped_Codes))){
  if(Grouped_Codes$total[i]<.treshhold){
    Grouped_Codes$group[i] = "Others"
  }
}
Grouped_Codes = aggregate(Grouped_Codes$total, by=list(Category = Grouped_Codes$group), FUN=sum)
colnames(Grouped_Codes) <- .x

#put groupname
Code_Counts$groups=group_names

#Get grouped codes with sufficient trials
Sufficient_Grouped <- subset(Grouped_Codes, total >= .treshhold)
