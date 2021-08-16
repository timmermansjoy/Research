#This script creates graphs from the data for visual analysis
#Run the matching script before creating the graph

#Packages
library(ggplot2)

#plot code occurrence, Run Script: Grouping.R
ggplot(data=All_Groups, aes(x=group, y=total, fill=substr(group, 0, 1))) + 
  geom_bar(stat='identity', width = 0.8) + 
  theme_minimal() + 
  xlab ("ICD-10 Codes") + 
  ylab ("Frequency") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank())
theme(legend.position = "none")

#TO-DO: better colour pallete for the above graph