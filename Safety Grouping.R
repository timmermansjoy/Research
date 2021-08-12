#This script selects only the safety related trials and groups these based on their ICD-10 codes. Groups will have a minimum size of the "Threshold
#Threshold = 50

#ICD Data Frame
.ICD_Codes_Safety <- data.frame(doc_id = Safety$Id, code = Safety$`ICD-10 Code`, success = Safety$Trial_Success)

#Count Codes
Code_Counts_Safety <- dplyr::count(.ICD_Codes_Safety, code, sort=FALSE)

#Get codes with sufficient trials
Sufficient_Base_Safety <- subset(Code_Counts_Safety, n >= 50)

#Group insufficient cases on first three characters
.old_safety <- Code_Counts_Safety$code[1]
.nums_safety <- 0
.total_safety <- 0
group_names_safety <- rep(NA, nrow(Code_Counts_Safety))
.treshhold <- 50

Grouped_Codes_Safety <- data.frame(matrix(ncol = 2, nrow = 0))
.x <- c("group", "total")
colnames(Grouped_Codes_Safety) <- .x

#go trough each element
for(i in 2:(nrow(Code_Counts_Safety))){
  if(Code_Counts_Safety$n[i] < .treshhold && Code_Counts_Safety$n[i-1] < .treshhold){
    #check if code is similar as the one above it
    if( str_contains(Code_Counts_Safety$code[i], substr(.old_safety, 0, 3)) ){
      # if it is the first element that is similar also take the one above
      if (.nums_safety == 0){
        .total_safety = .total_safety + Code_Counts_Safety$n[i-1]
        group_names_safety[i-1] = substr(.old_safety, 0, 3)
      }
      
      group_names_safety[i] = substr(.old_safety, 0, 3)
      .total_safety = .total_safety + Code_Counts_Safety$n[i]
      .nums_safety = .nums_safety + 1
    }
    else{
      if (.nums_safety != 0){
        y <- data.frame(group=substr(.old_safety, 0, 3), .total_safety = .total_safety)
        Grouped_Codes_Safety <- rbind(Grouped_Codes_Safety, y)
      }
      #Reset Variables
      .nums_safety = 0
      .total_safety = 0
      
      #Add standard name to the group
      group_names_safety[i] = Code_Counts_Safety$code[i]
      #Current code in old variable for use in the next loop
      .old_safety = Code_Counts_Safety$code[i]
      
    }
  }
  if(is.na(group_names_safety[i])){
    group_names_safety[i] = Code_Counts_Safety$code[i]
  }
}
#If, after original grouping, the group size is still smaller than treshhold: replace group with just the first letter
for(i in 1:(nrow(Grouped_Codes_Safety))){
  if(Grouped_Codes_Safety$.total_safety[i]<.treshhold){
    Grouped_Codes_Safety$group[i] = substr(Grouped_Codes_Safety$group[i], 0, 1)
  }
}
#group Grouped_Codes_Safety to sum together all entries by group
Grouped_Codes_Safety = aggregate(Grouped_Codes_Safety$.total_safety, by=list(Category = Grouped_Codes_Safety$group), FUN=sum)
colnames(Grouped_Codes_Safety) <- .x

#group everything that's still too small
for(i in 1:(nrow(Grouped_Codes_Safety))){
  if(Grouped_Codes_Safety$total[i]<.treshhold){
    Grouped_Codes_Safety$group[i] = "Others"
  }
}
Grouped_Codes_Safety = aggregate(Grouped_Codes_Safety$total, by=list(Category = Grouped_Codes_Safety$group), FUN=sum)
colnames(Grouped_Codes_Safety) <- .x

#put groupname
Code_Counts_Safety$groups=group_names_safety

#combine Sufficient_Base_Safety with Grouped_Codes_Safety
colnames(Sufficient_Base_Safety)<-colnames(Grouped_Codes_Safety)
All_Groups_Safety <- rbind(Grouped_Codes_Safety, Sufficient_Base_Safety)

