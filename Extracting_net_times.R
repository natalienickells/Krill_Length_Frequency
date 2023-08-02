#Natalie Nickells 

#1st August 2023
#Aim: To reassemble my pipeline that made my net times spreadsheet
# "ref.A", because the times have at some point been changes to 12hr
#from 24hr and this has caused an issue!!


#STEP 1: Checking the 'raw material'. 

#Setting work directory

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



#2===========================================================================
#the below is from the script "NDandKLF"

#Aim: This script just adds net start date and start time (seperated), to the net data csv. Only for JR15002. 
#Likely will delete this portion once I find the updated way I do this. 
#Natalie Nickells 8th May 
install.packages('tidyverse')
install.packages('chron')
library(chron)#this will clash with lubridate
library(tidyverse)
setwd('C:\\Users\\nanick73\\OneDrive - NERC\\Documents\\Chapter2_KLF\\Data')

#Reading in KLF data #so i am just testing for JR15002 here
KLF <- read.csv("JR15002_KRILLBASE_KLF.csv")
ND <- read.csv("JR15002_NETDATA.csv")


colnames(KLF) #GMTtime is the time column
colnames(ND) #Start.of.event and End.of.event are the time columns

#Need to change time format in KLF and ND by adding extra columns
view(ND)
#Splitting ND start time into date and time
starts <- t(as.data.frame(strsplit(ND$Start.of.Event,' ')))
colnames(starts) <- c('start.date', 'start.time')
rownames(starts)<- NULL
starts <- as.data.frame(starts)

view(starts)

#ENDING VERSION NOT WORKING, COME BACK TO. ROWS 123 to 125 have missing data which can't be converted. 
ends = t(as.data.frame(strsplit(ND$End.of.event,' ')))
test <- as.data.frame (ends [1:123])


view(ND$End.of.event)


colnames(ends) = c('end.date', 'end.time')
rownames(ends)=NULL
ends <- as.data.frame(ends)

ND <- mutate(ND, 
             start.date= starts$start.date, 
             start.time = starts$start.time)




