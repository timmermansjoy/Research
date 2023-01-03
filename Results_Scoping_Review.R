#This scripts analyses the results from the scoping review

#Clear workspace
rm(list = ls(all.names = TRUE))

#Packages
library(readxl)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(RColorBrewer)
source("Utils.R")

#This part of the script analyzes the animal models referenced.
#Import Data Set
Animal_Species<-read_xlsx('Data/Animal_Species_Scoping_Review.xlsx')

#Count species
count_species<-dplyr::count(Animal_Species, Species)

#Create Pie Chart
ggplot(data = count_species, aes(x="", y = n, fill = Species)) +
  geom_bar(stat="identity", width=1, color='white') +
  coord_polar("y", start=0) +
  theme_void() +
  theme(text = element_text(size = 15)) +
  geom_text(aes(x=1.2, label = paste0(n)), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual (values=c("#006BA4","#FF800E","#ABABAB","#595959","#5F9ED1","#C85200","#898989","#A2C8EC","#FFBC79","#CFCFCF")) +
  guides(fill = guide_legend(title = "Animal Species"))

#This part of the script analyzes the number of references per paper
#Import Data set
References_SuccesRates <- read_xlsx('Data/References.xlsx')

#Create histogram Translational succes vs references
ggplot(data = References_SuccesRates, aes(x=Reported_Translational_Succes_rate, y = ..count.. , fill = Reference_Bins)) + 
  geom_histogram(binwidth = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15))\

#barplot
References_VS_Success <- read_xlsx('Data/Translation_vs_References.xlsx')

ggplot(data = References_VS_Success, aes(x=Translational_Success_Rate, y = N, fill = Reference_Bin))+
  geom_bar(stat="identity", width=0.5)+
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  xlab("Reported Translational Success Rate")+
  ylab("Number of Papers")+
  guides(fill = guide_legend(title = "Number of Included References"))

#create boxplot
ggplot(data = References_SuccesRates, aes(x=reorder(Reference_Bins,-Reported_Translational_Succes_rate), y=Reported_Translational_Succes_rate)) +
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.015), aes(colour=Reported_Translational_Succes_rate))+
  #scale_color_manual(values = c("0" = "red", "0.5" = "orange" , "0.75" = "#b6d7a8" , "1" = "green"))
  xlab ("Total Number of References") +
  ylab ("Reported Translational Success Rate")+
  theme_minimal() +
  theme(text = element_text(size = 15))+
  ylim(NA,1)

#create scatter plot animal vs human references
ggplot(data = References_SuccesRates, aes(x=Number_of_Animal_References, y =Number_of_Clinical_References)) + 
  geom_point(position = position_jitter(width=0.5, height=0.5))+
  geom_abline(alpha = 0.5, colour = "grey60", slope = 1) +
  labs(x="Number of Pre-Clinical References",y="Number of Clinical References")+
  theme_minimal()+
  theme(text = element_text(size = 15)) +
  xlim(NA,120)+
  ylim(NA,120)

#Success Rates
#count success rates
Overall_Success <- dplyr::count(References_SuccesRates, Reported_Translational_Succes_rate)
write_xlsx(Overall_Success, "data//Overall_Success.xlsx")

#Create Bar Plot
ggplot(data = Overall_Success, aes(x = Reported_Translational_Succes_rate, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")

#Count success Pharma
Pharma_Success <- dplyr::count(References_SuccesRates, Success_Pharmacology)

#Create Bar Plot
ggplot(data = Pharma_Success, aes(x = Success_Pharmacology, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")+
  ylim(NA,30)+
  xlim(NA,1)

#Count success Neuro
Neuro_Success <- dplyr::count(References_SuccesRates, Succes_Neuro)

#Create Bar Plot
ggplot(data = Neuro_Success, aes(x = Succes_Neuro, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")+
  ylim(NA,30)

#Count success Cancer
Cancer_Success <- dplyr::count(References_SuccesRates, Success_Cancer)

#Create Bar Plot
ggplot(data = Cancer_Success, aes(x = Success_Cancer, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")+
  ylim(NA,30)

#Success in SRs and MAs
#load data
All_Data <- read_xlsx('Data/Scoping_Review_Extraction.xlsx')
#subset
SRs_MAs <- subset(All_Data, Publication_Type == "Systematic Review" | Publication_Type == "Meta-Analysis")
#Count success
SR_MA_Success <- dplyr::count(SRs_MAs, Translational_Succes_rate)

#SR Success
SRs <-subset(All_Data, Publication_Type == "Systematic Review")
SR_Succes <- dplyr::count(SRs, Translational_Succes_rate)


#Graph success split by publication type
#load data
split_success <- read_xlsx('Data/Success_Split.xlsx')
#Create Graph
ggplot(data = split_success, aes(x = Succes_Rate, y = N, fill = Publication_Type)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  scale_fill_brewer(palette="Blues")+
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of Papers") +
  guides(fill = guide_legend(title = "Publication Type"))

#Compare Success Rate
Compare_Success <- read_xlsx('Data/Figuur6.xlsx')

ggplot(data = Compare_Success, aes(x = Field, y = Translational_Success))+
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), colour = "Blue")+
  theme_minimal() +
  xlab("Field of Research")+
  ylab("Reported Translational Success Rate")