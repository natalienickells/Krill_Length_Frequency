#1 Listing nets and cruises where krill were measured


#Natalie Nickells
#3rd January 2024
#Aim: Identifying cruises & nets for which I have krill length data

#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency


#This script is an updated version of "ExtractingEventNAmes.R", at filepath D:\R code\ExtractingEventNAmes.R

#Known errors:
#this script does not yet give all the net names that I need. Not processing those in a difficult format. 
#Remember: I don't necessarily need to use every single cruise if some of them are
#in nightmare format... but want to finally decide which ones I am using!!

#SET UP=========================================================================
#Loading required packages
library(tidyverse)
library(readxl)
library(openxlsx)

#Setting working directory 
setwd("D:\\Cruise_data\\")

#LIST OF NETS ====================================================================
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
  print(filepath)
  if(grepl(".csv", filepath)){ #at the mo this is only true for JR228 
    #if it has the format of KLF data from the PDC, to get the net names, 
    #read in the data and then go to the event name column and find all the unique net name
    #make a variable of cruise, event name, net number 
    #add the cruise name on the front before adding them to an overall list of net names
    
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
  
  #if it has the format of a spreadsheet, with each tab as a net, 
  #read in the tab names and these are the event names
  #add the cruise name on the front before adding them to an overall list of net names
  
  if(grepl(".xlsx", filepath)){
    
    netnames<- (paste0(cruise,"_", getSheetNames(filepath)))
    #print(netnames)
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
  
  
  
  #Filtering net names
  
  #Making a vector of all the net names I don't want
 
  remove<- c("Summary", "SUMMARY", "plot",  "all", "combined", "Combined", 
             "comparison", "t-test", "Thys", "_T", "F", "RMT25",
            "frig", "thyso", "hist", "Frigida", "All", "Sheet", "corebox", 
             "Graphs" ) 
  netnames<- netnames[str_which(netnames, paste(remove, collapse= "|"), negate = TRUE)] #gives position

  print(str_which(netnames, paste(remove, collapse= "|"), negate = TRUE))
  netnames<- str_replace_all(netnames, "RMT8|Event|Ev|EV|E|e", "")
  netnames<- str_replace_all(netnames, "N", "_")

  #now make dataframe 
  #cruise in one column
  #netnames in another column
  
  #Making dataframe of net names
  temp<- data.frame(netnames, 
                       cruise)
  
  #Binding to existing dataframe
  output<- rbind(output, temp)
  
  rm(temp, netnames, cruise, filepath)
}

#Filtering net names

for(i in nrow(output)){
  netname<- output$netnames[i]
  
  
  #then do the filtering here
}


#Useful bits of filtering code from other script 
JR15004.KLF.events <- filter(JR15004.KLF.events, 
                             !(grepl("frig", 
                                     JR15004.KLF.events, ignore.case=T)) &
                               !(grepl("thy", 
                                       JR15004.KLF.events, ignore.case=T)) &
                               !(grepl("swarm",
                                       JR15004.KLF.events)) &
                               !(grepl("layer",
                                       JR15004.KLF.events)))
                             
                             
test<- str_remove(output$netnames, "Ev")#removing Ev from event numbers

JR177.KLF.events <- str_replace(JR177.KLF.events, "N", "_")

#Doing some extra net name filtering after the loop
#TKCould definitely even do some more filtering here, but will come back to (eg.r emoving spaces, removing JR100_JR100_)
pos<- str_which(output$netnames, "freq|_T|swarm|CB|comp|JR082|layr", negate=T ) 
#removing JR082 nets here because their format is unclear, to come back to. 

output<- output[pos,]

#Script refining/checking=====================================================
#Examining this output, what do I have.... (basically looking for errors to correct later.)
#Will basically need to go through cruise by cruise and check I have all the nets I expect to have.. 

#Comparing how many cruises I have...
unique(output$cruise) #have a cruise listed as NA... 
length(unique(output$cruise)) #have 19 cruises (well 18 actually, excluding NAs)


#Will go through and check the nets for each of the cruises I have
#Also need to identify- which cruises are missing?   

#DY098
(filter(output, cruise== "DY098"))$netnames
#some of these have RMT names in too...
#then I want to open the 

#JR096
filter(output, cruise== "JR096")

#JR100
filter(output, cruise== "JR096")

#JR116
filter(output, cruise== "JR096")

#JR177
filter(output, cruise== "JR096")

#JR200
filter(output, cruise== "JR096")

#JR228
filter(output, cruise== "JR096")

#JR230
filter(output, cruise== "JR096")

#JR255
filter(output, cruise== "JR096")

#JR260
filter(output, cruise== "JR096")

#JR280
filter(output, cruise== "JR096")

#JR291
filter(output, cruise== "JR096")

#JR304

#JR15002

#JR15004

#JR16003

#JR17002

#JR19001

#NA (still check this one, because what's gone wrong here? )






#Output=======================
write.csv (output, "netnames.csv", row.names=F)










#Some dicussion of some of the errors ======================================

#At the moment this is only importing the first tab of the spreadsheet, need to import the whole thing
#think that I can use some of my scripting from Lizzie density reading script to read in each tab. 

#Also could just make this script into also reading in krill length data and saving it. Since I'm pretty much going to be there already. 

#would want to use the tab names from this (although some won't be in exactly this format)

#if it has another format
#print the cruise name with a message 'problematic format, net names to be extracted by hand
#NEED TO GO THROUGH AND WORK OUT WHICH CRUISES THIS IS TRUE FOR.
#and then will extract those net names outside of the function. 





#Remembering what the excel sheets function does

test<- excel_sheets(filepathinfo$KL_Data_Filepath[15])
#okay so it literally just gets the names of the tabs out and makes them ito a vector 

path <- (filepathinfo$KL_Data_Filepath[15])
test<- lapply(excel_sheets(path), read_excel, path = path)
#this makes a list, with each tab under it/ 
#again i think go back to lizzie code and this will help here. 


#Code from Data read in latest 08 12 23 
#JR17002 data is in JR17002KLF (list) and for JR16003 in JR16003KLF (list )

files_length <- c("D:\\Cruise_data\\JR17002\\JR17002_krill_length_V2.xlsx",
                  "D:\\Cruise_data\\JR16003\\Krill_lengths_JR16003.xls")

library(readxl)     
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  # print data frame 
  print(data_frame) 
} 

## read in file 1 as a list
f1 <- multiplesheets(files_length[1])

## number of dataframes in the list: length(f1)
length(f1)
## get to data using f1[[1]]
## get to net name using names(f1)[1]

## data frame to store in
df_length <- data.frame()

## loop over each dataframe in the list to fill in df_length
for (i in 1:length(f1)) {
  # i <- 1  # testing
  
  ## get data
  i_df <- f1[[i]]
  ## get name
  i_name <- names(f1[i])
  i_name <- sub("^Ev", "JR17002_", i_name)
  
  ## make data frame from this net
  i_df2 <- data.frame(netname = i_name,
                      krill_length = i_df$Length)
  
  ## add it to the main results dataframe
  df_length <- rbind(df_length, i_df2)
  
  rm(i, i_df, i_name, i_df2)
}

head(df_length)
tail(df_length)
table(df_length$netname, useNA="ifany")

## now get the data from file 2

## read in file 2 as a list
f2 <- multiplesheets(files_length[2])
f2 <- f2[-which(names(f2)=="all")]

## loop over each dataframe in the list to fill in df_length
for (i in 1:length(f2)) {
  # i <- 1  # testing
  
  ## get data
  i_df <- f2[[i]]
  ## get name
  i_name <- names(f2[i])
  i_name <- sub("^Event ", "JR16003_", i_name)
  
  ## make data frame from this net
  i_df2 <- data.frame(netname = i_name,
                      krill_length = i_df$Length)
  
  ## add it to the main results dataframe
  df_length <- rbind(df_length, i_df2)
  
  rm(i, i_df, i_name, i_df2)
}

head(df_length)
tail(df_length)
table(df_length$netname, useNA="ifany")

## clean up f1 and f2
rm(f1, f2)

## split out netname into cruise and net
df_length$cruise <- substr(df_length$netname, 1, 7)
df_length$netnumber <- substr(df_length$netname, 9, 12)

## turn krill length into numeric
df_length$krill_length <- as.numeric(df_length$krill_length)
table(df_length$krill_length, useNA="always")
## remove NAs
df_length <- subset(df_length, !is.na(krill_length))
table(df_length$krill_length, useNA="always")

## keep only the nets that we're interested in
nets_to_keep <- unique(df_dbdiff$netname)
df_length <- subset(df_length, netname %in% nets_to_keep)
table(df_length$netname, useNA="ifany")




