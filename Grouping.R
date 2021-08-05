#Count Codes
Code_Counts <- dplyr::count(.ICD_Codes, code, sort=FALSE)

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

#combine Sufficient_base with Grouped_codes
colnames(Sufficient_Base)<-colnames(Grouped_Codes)
All_Groups <- rbind(Grouped_Codes, Sufficient_Base)
