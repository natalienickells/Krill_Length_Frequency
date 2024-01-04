#1 Listing nets and cruises where krill were measured


#Natalie Nickells
#4th January 2024
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


#need columns 1 to 20, and columns Cruise_ID, KL_Data_Format and KL_Data_Filepath, and KL_Data_Format_No
filepathinfo<- select(filepathinfo, Cruise_ID, KL_Data_Filepath, KL_Data_Format, KL_Data_Format_No) 
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
  netnames<- str_replace_all(netnames, "JR100_JR100", "JR100")
  netnames<- str_replace_all(netnames, " ", "")

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
output$netnames
for(i in nrow(output)){
  netname<- output$netnames[i]
  
  
  #then do the filtering here
}


#Useful bits of filtering code from other script 
#JR15004.KLF.events <- filter(JR15004.KLF.events, 
                             #!(grepl("frig", 
                                     #JR15004.KLF.events, ignore.case=T)) &
                               #!(grepl("thy", 
                                       #JR15004.KLF.events, ignore.case=T)) &
                               #!(grepl("swarm",
                                       #JR15004.KLF.events)) &
                               #!(grepl("layer",
                                       #JR15004.KLF.events)))
                             


#Doing some extra net name filtering after the loop
#TK Could definitely even do some more filtering here, but will come back to 
pos<- str_which(output$netnames, "freq|swarm|CB|comp|JR082|layr", negate=T ) 
#TK removing JR082 nets here because their format is unclear, to come back to. 
#TK- for everything i'm removing here, need to go back to the spreadsheets 
#and check what this actually represents, make sure i am not pointlessly losing data. 
output<- output[pos,]

#Script refining/checking=====================================================
#Examining this output, what do I have.... (basically looking for errors to correct later.)
#Will basically need to go through cruise by cruise and check I have all the nets I expect to have.. 

#Comparing how many cruises I have...
unique(output$cruise)  
length(unique(output$cruise)) #have 20 cruises
#I have 21 cruises to target, so missing 2: JR082 and JR245



#Go through EVERY CRUISE and identify which nets I should have. Are these correct?


#Will go through and check the nets for each of the cruises I have
#Also need to identify- which cruises are missing?   

#DY098
(filter(output, cruise== "DY098")) #29 nets
#TK RMT_14_2 is just 14_2, ditto RMT_28_1
#TK also some of these net names have double underscores...
#TK 78_T is euphausia tricantha, not superba, so remove. 
#78_1 very few euphausias measured... 79_1 only two measued.
#93 only 3 measured
#97_1 only 4 measured.
#109_1 only 2 measured
#TK 113_2_T is also not e superba. need to remove. 
#TK basically need a filter later for a minimum nu,ber measured. 



#some of these have RMT names in too...
#then I want to open the 

#JR082
#there is one tab for all nets, so I need to read this in and extract the unique combos of net and event

#JR096
filter(output, cruise== "JR096") #all the nets I was expecting
nrow(filter(output, cruise== "JR096"))#13 nets

#JR100
filter(output, cruise== "JR100") #all the nets I was expecting
nrow(filter(output, cruise== "JR100"))# 8 nets

#JR116
filter(output, cruise== "JR116") #3 nets, missing those associated with moorings and core box...
#TK come back to: I think I probably want to keep those mooring/core box nets. 

#JR177
filter(output, cruise== "JR177") # have removed 'event 28 all nets', but there were only 10 krill measured there so this is fine. 
#there are 'b' tabs included here, these are second measurings of the krill, but not fresh measurings so should discard, as per written on the tab. 
#TK discard nets including 'b' from JR177 data. 
nrow(filter(output, cruise== "JR177")) #51 nets for this cruise, good haul. 

#JR200
filter(output, cruise== "JR200") #all the nets I was expecting, 9 nets. 

#JR228
filter(output, cruise== "JR228") #hmmm.. this is correct but I have no udea how? Not in the format I was expecting when I opened the spreadhsset. 
# I obviously must have fixed this earlier. TK come back to. 7 nets. 
#I think possibly the data from JR228 is not going to work, the krill length data is not great and I'm not sure there are many nets where krill were measured? Confusing.. 


#JR230
filter(output, cruise== "JR230") #nets expected. 2 nets. 96_1 has low krill measured number, so this could be a candidate for removing. 

#JR245
#currently no net names
#complicated sheet layout, tread carefully. Use WCB and ECB tabs. 

#JR255
filter(output, cruise== "JR255")#nets expected, 14 nets

#JR260
filter(output, cruise== "JR260") #12 nets. nets expected except TK
#tk net numbers (not event numbers) are missing because they feature inside the tab instead of in the tab name. 
#need to fix this. maybe manually if quicker?

#JR280
filter(output, cruise== "JR280") #nets expected, 8 nets

#JR291
filter(output, cruise== "JR291") #nets expected, 10 nets
#first net name (JR291_81_2) looks like it has a space at the front.

#JR304
filter(output, cruise== "JR304") #nets expected, 7 nets

#JR15002
filter(output, cruise== "JR15002") #nets expected, 9 nets


#JR15004
filter(output, cruise== "JR15004") #nets expected, 19 nets
#this KLF data spreadsheet also has some good information , eg comparing top and bottom of swarms
#I won't be importing the data here but TK might be good to come back to. 


#JR16003
filter(output, cruise== "JR16003") #7 nets, nets expected


#JR17002
filter(output, cruise== "JR17002") #8 nets, nets expected

#JR19001
filter(output, cruise== "JR19001")
#48_1 has a tab but krill not measured so need to delete that one
# all other nets as expected, 10 nets total, 9 without 48_1.



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




