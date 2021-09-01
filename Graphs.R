#This script creates graphs from the data for visual analysis
#Run the matching script before creating the graph

#plot code occurrence, 
#Associated Script: Grouping.R
library(ggplot2)
ggplot(data = All_Groups, aes(x=group, y=total, fill=substr(group, 0, 1))) + 
  geom_bar(stat='identity', width = 0.8) + 
  theme_minimal() + 
  xlab ("ICD-10 Codes") + 
  ylab ("Frequency") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank())
theme(legend.position = "none")

#TO-DO: better colour pallete for the above graph

#plot subgroup success
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Subgroup_Success, aes(x = Field, y = Success)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Positive Outcomes") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Plot subgroup termination
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Subgroup_Termination, aes(x = Field, y = Termination)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Terminated Trials") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Plot Chapter Success
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Success_Chapter, aes(x = Field, y = Success)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Positive Outcomes") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Plot Chapter Termination
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Termination_Chapter, aes(x = Field, y = Termination)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Positive Outcomes") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))