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
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x=1.2, label = paste0(n)), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual (values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")) +
  guides(fill = guide_legend(title = "Animal Species"))

#This part of the script analyzes the number of references per paper
#Import Data set
References_SuccesRates <- read_xlsx('Data/References.xlsx')

#Create Scatter Plot Translational succes vs references
ggplot(data = References_SuccesRates, aes(x=Total_Number_of_References, y = Reported_Translational_Succes_rate)) + 
  geom_point()+
  labs(x="Total Number of References",y="Reported Translational Success Rate")+
  theme_minimal()

#create scatter plot animal vs human references
ggplot(data = References_SuccesRates, aes(x=Number_of_Animal_References, y =Number_of_Clinical_References)) + 
  geom_point(position = position_jitter(width=0.5, height=0.5))+
  labs(x="Number of Pre-Clinical References",y="Number of Clinical References")+
  theme_minimal()+
  xlim(NA,120)+
  ylim(NA,120)

#Success Rates
#count success rates
Overall_Success <- dplyr::count(References_SuccesRates, Reported_Translational_Succes_rate)

#Create Bar Plot
ggplot(data = Overall_Success, aes(x = Reported_Translational_Succes_rate, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")

#Count success Pharma
Pharma_Success <- dplyr::count(References_SuccesRates, Success_Pharmacology)

#Create Bar Plot
ggplot(data = Pharma_Success, aes(x = Success_Pharmacology, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
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
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")+
  ylim(NA,30)

#Count success Cancer
Cancer_Success <- dplyr::count(References_SuccesRates, Success_Cancer)

#Create Bar Plot
ggplot(data = Cancer_Success, aes(x = Success_Cancer, y = n)) +
  geom_bar(stat='identity', width = 0.1) +
  theme_minimal() +
  xlab ("Reported Translational Success Rates") +
  ylab ("Number of References")+
  ylim(NA,30)
