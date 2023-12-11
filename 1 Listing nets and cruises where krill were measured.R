 
#Natalie Nickells
#11th December 2023
#Aim: Identifying cruises & nets for which I have krill length data

#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency

#This script is an updated version of "ExtractingEventNAmes.R", at filepath D:\R code\ExtractingEventNAmes.R

#SET UP=========================================================================
#Loading required packages
library(tidyverse)

library(openxlsx)

#Setting working directory 
setwd("D:\\Cruise_data\\")

#DATA IMPORT====================================================================
#My data for each cruise has a different name and filepath. 
#I have a spreadsheet where these different filepaths are kept, so will first read this in. 

library(readxl)
filepathinfo<- read_excel("D:\\KLF Cruises & Nets - Data Tracking Sheet.xlsx")

#need columns 1 to 20, and columns Cruise_ID, KL_Data_Format and KL_Data_Filepath
filepathinfo<- select(filepathinfo, Cruise_ID, KL_Data_Filepath, KL_Data_Format) 
filepathinfo<- slice(filepathinfo, 1:20)


print(filepathinfo$KL_Data_Filepath)




