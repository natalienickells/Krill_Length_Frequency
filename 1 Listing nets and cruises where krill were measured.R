 
#Natalie Nickells
#12th December 2023
#Aim: Identifying cruises & nets for which I have krill length data

#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency


#this is a test change to show tom 


#This script is an updated version of "ExtractingEventNAmes.R", at filepath D:\R code\ExtractingEventNAmes.R

#SET UP=========================================================================
#Loading required packages
library(tidyverse)
library(readxl)
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



#Reading in netnames

output<- data.frame()
for(i in 1: nrow(filepathinfo)) {
  
  cruise<- filepathinfo$Cruise_ID[i]
  filepath<-filepathinfo$KL_Data_Filepath[i]
  
  if(grepl(".csv", filepath)){ #at the mo this is only true for JR228 
    
    #Reading in CSV file
    df <-read.csv((paste0(filepath)))
    #Creating 'common code' which includes Cruise, event number and net number
    df <- unite(df, common.code, c(Cruise,Event, Netno))
    
    #Assigning net names
    netnames<- unique(df$common.code)
    
    #Assigning teams 
    assign(print(paste0(cruise, "KLFrawdata")), df)
    
    #then will want to add those common codes, along with the cruise, to a blank dataframe that I will want to populate
    rm(df)
    }
    
  
  if(grepl(".xlsx", filepath)){
    
    netnames<- (paste0(cruise,"_", getSheetNames(filepath)))
    #assign(print(paste0(cruise, "KLFrawdata")), read_excel((paste0(filepath))))
  }
  
  
  if(grepl(".xls", filepath)){
    #Getting the net names
    netnames<-(paste0(cruise,"_", excel_sheets(filepath)))
    
    #Reading the data
    #assign(print(paste0(cruise, "KLFrawdata")), read_excel((paste0(filepath))))
    
    #testing out reading this data in
    #test<- lapply(excel_sheets(filepath), read_excel, path = filepath)
  }
  
  #now make dataframe 
  #cruise in one column
  #netnames in another column
  
  #Making dataframe of net names
  temp<- data.frame(netnames, 
                       cruise)
  
  #Binding to existing dataframe
  output<- rbind(output, temp)
  
  rm(temp)
}


#At the moment this is only importing the first tab of the spreadsheet, need to import the whole thing
#think that I can use some of my scripting from Lizzie density reading script to read in each tab. 

#Also could just make this script into also reading in krill length data and saving it. Since I'm pretty much going to be there already. 


#in pseudo code

#if it has the format of KLF data from the PDC, to get the net names, 
#read in the data and then go to the event name column and find all the unique net names
#make a variable of cruise, event name, net number 
#add the cruise name on the front before adding them to an overall list of net names

#if it has the format of a spreadsheet, with each tab as a net, 
#read in the tab names and these are the event names
#add the cruise name on the front before adding them to an overall list of net names

#if it has another format
#print the cruise name with a message 'problematic format, net names to be extracted by hand

#and then will extract those net names outside of the function. 

for(i in 1: nrow(filepathinfo)) {
  
  cruise<- filepathinfo$Cruise_ID[i]
  filepath<-filepathinfo$KL_Data_Filepath[i]
  
  if(grepl(".csv", filepath)){ #at the mo this is only true for JR228 
    df <-read.csv((paste0(filepath)))
    
    df <- unite(df, common.code, c(Cruise,Event, Netno))
    
    netnames<- unique(df$common.code)
    
    #assign(print(paste0(cruise, "KLFrawdata")), df)
    
    #then will want to add those common codes, along with the cruise, to a blank dataframe that I will want to populate
    rm(df)
  }
  
  
  if(grepl(".xlsx", filepath)){
    
    netnames<- (paste0(cruise,"_", getSheetNames(filepath)))
    #assign(print(paste0(cruise, "KLFrawdata")), read_excel((paste0(filepath))))
  }
  
  
  if(grepl(".xls", filepath)){
    #Getting the net names
    netnames<-(paste0(cruise,"_", excel_sheets(filepath)))
    
    #Reading the data
    #assign(print(paste0(cruise, "KLFrawdata")), read_excel((paste0(filepath))))
    
    #testing out reading this data in
    #test<- lapply(excel_sheets(filepath), read_excel, path = filepath)
  }
  
  #now make dataframe 
  #cruise in one column
  #netnames in another column
  
  #Making dataframe of net names
  temp<- data.frame(netnames, 
                    cruise)
  
  #Binding to existing dataframe
  output<- rbind(output, temp)
  
  rm(temp)
}




#Rememberering what the excel sheets function does

test<- excel_sheets(filepathinfo$KL_Data_Filepath[15])
#okay so it literally just gets the names of the tabs out and makes them ito a vector 

path <- (filepathinfo$KL_Data_Filepath[15])
test<- lapply(excel_sheets(path), read_excel, path = path)
#this makes a list, with each tab under it/ 
#again i think go back to lizzie code and this will help here. 





