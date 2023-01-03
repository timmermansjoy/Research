#This script creates graphs from the data for visual analysis
#Run the matching script before creating the graph

#plot code occurrence, 
#Associated Script: Grouping.R
library(ggplot2)
ggplot(data = All_Groups, aes(x=group, y=total)) + 
  geom_bar(stat='identity', width = 0.8) + 
  theme_minimal() + 
  xlab ("ICD-10 Codes") + 
  ylab ("Frequency") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank())
theme(legend.position = "none")



#plot subgroup success
#Associated script: Subsets.R
Subgroup_Success <- read_xlsx('Data/Subgroup_Success.xlsx')
library(ggplot2)
ggplot(data = Subgroup_Success, aes(x = Field, y = Success, fill = Colour_Match)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Positive Outcomes") +
  guides(fill = guide_legend(title = "Success Rate")) +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#Plot subgroup termination
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Subgroup_Termination, aes(x = Field, y = Termination)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Terminated Trials") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#Plot Chapter Success
#Associated script: Subsets.R
library(ggplot2)
Chapter_Success <- read_xlsx('Data/Chapter_Success.xlsx')
ggplot(data = Chapter_Success, aes(x = Field, y = Success, fill = Colour_Match)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Positive Outcomes") +
  guides(fill = guide_legend(title = "Success Rate")) +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

#Plot Chapter Termination
#Associated script: Subsets.R
library(ggplot2)
ggplot(data = Chapter_Termination, aes(x = Field, y = Termination)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_minimal() +
  xlab ("Field of Research") +
  ylab ("Percentage Terminated Trials") +
  scale_y_continuous(limits = c(0 , 100)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))