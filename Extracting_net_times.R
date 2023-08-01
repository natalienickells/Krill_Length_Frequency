#Natalie Nickells 

#1st August 2023
#Aim: To reassemble my pipeline that made my net times spreadsheet
# "ref.A", because the times have at some point been changes to 12hr
#from 24hr and this has caused an issue!!


#STEP 1: Checking the 'raw material'. 

#Setting work diectory

setwd("C:/Users/nanick73/OneDrive - NERC/Documents/Chapter2_KLF/Data")


#Below code is from "Combining CSV files.R"--------------


#Joining CSV files for datasets
#setwd("D:/Krill length frequency data")

install.packages("tidyverse")
install.packages("data.table")
library(tidyverse)
library(data.table)


#Reading all the csv files from KRILLBASE (they will all be same format)
JR082 <- read.csv('JR082_KRILLBASE_KLF.csv')
JR096 <- read.csv("JR096_KRILLBASE_KLF.csv")
JR100 <- read.csv("JR100_KRILLBASE_KLF.csv")
JR116 <- read.csv("JR116_KRILLBASE_KLF.csv")
JR15002 <- read.csv("JR15002_KRILLBASE_KLF.csv")
JR245<- read.csv("JR245_KRILLBASE_KLF.csv")
JR177<- read.csv("JR177_KRILLBASE_KLF.csv")
JR228<- read.csv("JR228_KRILLBASE_KLF.csv")
JR230<- read.csv("JR230_KRILLBASE_KLF.csv")
JR245<- read.csv("JR245_NETDATAcopied.xlsx" )
JR260<- read.csv("JR260_KRILLBASE_KLF.csv")
JR280<- read.csv("JR280_KRILLBASE_KLF.csv" )
JR291<- read.csv("JR291_KLF_KRILLBASE.csv")

df_list = list(JR082, JR096, JR100, JR116, JR15002, 
               JR177, JR228, JR230, JR245, JR260, 
               JR280, JR291)
big.df <- reduce(df_list, full_join )


view(big.df)
#AT THIS STAGE, 24 HR CLOCK STILL 

#Checking- contains all cruises, seems a reasonable length
list(unique(big.df$Cruise))
length(big.df$Id)

#Write to csv
write.csv(big.df, "KRILLBASE_KLF_data.csv")
use_github()
