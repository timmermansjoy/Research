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
