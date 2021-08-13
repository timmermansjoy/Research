#Functions

#Calculate percentages
Percentage_calc <- function(DataFrame) {
  Result <- ((DataFrame$n[[2]]/(DataFrame$n[[2]]+DataFrame$n[[1]]))*100)
  return(Result)
}