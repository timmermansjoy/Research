#Functions

#Calculate percentages
Percentage_calc <- function(DataFrame) {
  Result <- ((DataFrame$n[[2]]/(DataFrame$n[[2]]+DataFrame$n[[1]]))*100)
  return(Result)
}

Percentage_calc_large <- function(DataFrame) {
  Result <- ((DataFrame$n[[3]]/(DataFrame$n[[2]]+DataFrame$n[[3]]))*100)
  return(Result)
}

subset_termination <- function(DataFrame) {
  Results <- ((DataFrame$n[[3]]/sum(DataFrame$n))*100)
  return(Results)
}

subset_termination_large <- function(DataFrame) {
  Results <- ((DataFrame$n[[4]]/sum(DataFrame$n))*100)
  return(Results)
}
