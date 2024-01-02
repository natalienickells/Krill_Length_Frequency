# 3 Gathering acoustic files needed

#Natalie Nickells
#2nd January 2024

#Aim: to gather the acoustic files I will need to make my EV files together into one folder.
#These will be from the net times where krill were measured, from various cruises. 


#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency

#This script is an updated version/draws heavily on "Selecting_raw_files040823.R", 
#at filepath "D:\R code\Selecting_raw_files040823.R"


#Since acoustic files use a huge amount of storage, and each cruise has a large
#amount of acoustic data, I will gather together only the acoustic data around 
#the nets where krill have been measured. 

#Data sources: 
#Raw data is from the SONA computer's Samba drive (TK actually at the mo from sophje samba drive, need to change this. )
#Net times currently coming from the file krillnets.A, filepath "E:/krillnets.A.txt"
#TK will want to change this file later, likely to one that incorporates depths too. 
#This file will be created in script 2. 

#Data output:
#the csv ref.A, filepath "D:\\Natalie\\ref.A.csv", is like krillnets.A but with added net times. 
#raw files to use for ev file crteation are saved to "D:/Natalie/JCRRawFiles"


#Current issues; 
#1 I don't know whether this is the version of the script with the 12/24 hr issue. need to check thisd
#2 will want to update the script to include ek80 files possibly 
#(or this might be a different script, as it would be adding to an existing Ev file... hmm.. 
#but would i still want a list of these files to be generated here? hmm)

#0 SET UP========================================================================
library(tidyverse)
library(fs)
#Loading in useful data

cruise.folders <- # read.csv("D:/Natalie/Cruises&theirfolders.csv") #filepath when on sona
  #using a different filepath when on  laptop
  read.csv("D:\\Copy.of.Natalie.SONA\\Cruises&theirfolders.csv")
#this csv contains a list of the folder name for each cruise. 
#CURRENTLY THIS ONLY INCLUDES CRUISES JR17002 jr16003 JR15004 jr15002 
#jr304 jr291 jr280 jr260 jr177
#TK NEED TO UPDATE THIS WHEN I AM ON SONA COMPUTER, WITH MISSING CRUISES. 

cruise.folders$Folder<-as.character(cruise.folders$Folder)

#1 Making a dataframe of all the available .raw files===========================
#Making dataframe column of source directories
cruise.folders$sourcedir <- paste0( "S:/cruise_data/", cruise.folders$Folder)
#TK will need to change this filepath too, so make it be using SONA rather than sof..

targetdir<- ("D:/Natalie/JCRRawFiles") 
#where the raw files will be saved to. 

rawfiles<- as.data.frame(NULL) #making blank df to fill in the loop

for(i in (1:(nrow(cruise.folders)))){ #for each cruise
  sourcedir<- paste0(cruise.folders$sourcedir[i]) #assign source directory
  
  cruiseraws <- list.files(sourcedir, pattern= 'D.+T.+raw$', full.names=TRUE, recursive=TRUE) #list all the raw files in that directory
  
  #then filter cruise raws to be ek60 only and no work
  
  cruiseraws <- as.data.frame(cruiseraws) #making into a dataframe so can filter
  
  cruiseraws <- filter(cruiseraws, 
                       (grepl("ek60", cruiseraws, ignore.case=T)) & #ek60 present in filepath
                         (!grepl("work",cruiseraws, ignore.case=T)) & #work not present in filepath
                         (!grepl("calibration", cruiseraws, ignore.case=T ))) #calibration not present in filepath
  
  rawfiles<- rbind(cruiseraws, rawfiles) #make a list of all these raw file names
}
#some error codes- overlong paths. 11 files affected. COME BACK TO
#TK check on these errors...? think they are now 'gone'.

#TK I will also need to check which cruises need EK80 data! 
#or those cruises, I will want to use something very similar, but filtering for EK80 instead of EK60. 

#==============================================================================
#2 Extracting time and date 

colnames(rawfiles)
str(rawfiles)
#Extracting T with time
rawfiles$time <- str_extract_all(rawfiles$cruiseraws, 'T\\d\\d\\d\\d\\d\\d.raw') 
rawfiles$time <- str_remove(rawfiles$time, "T")
rawfiles$time <- str_remove(rawfiles$time, ".raw")

#Extracting D with day
rawfiles$date <- str_extract_all(rawfiles$cruiseraws, 'D\\d\\d\\d\\d\\d\\d\\d\\d') 
rawfiles$date<- str_remove(rawfiles$date, "D")

#Making date.time variable
rawfiles$date.time<- str_c(rawfiles$date,'.', rawfiles$time)

#Maing date.time variable as Posixct structure
rawfiles <- mutate(rawfiles, 
                   date.time = as.POSIXct
                   (date.time, format= "%Y%m%d.%H%M%S"))

rawfiles<- select(rawfiles, cruiseraws, date.time) #making a dataframe of just raw file names, and date-time.

#Ordering dataframe by time
rawfiles<- rawfiles[order(rawfiles$date.time),]
#Adding rownumbers
rawfiles$row.no <- c(1:(nrow(rawfiles))) # TK do i actually ever use row numbers later? check if i need

#Adding day variable
rawfiles$date<- strftime(rawfiles$date.time,
                         format= "%d/%m/%Y")

#Would be good to have a just file name column too. 
rawfiles$filename <- basename(rawfiles$cruiseraws)

#================================================================================
#3 Matching time and dates of raws and nets
#Loading in useful data
#Times of krillnets
krillnets.A<- read.table("E:/krillnets.A.txt", header=T)
#TK as I change script 2 this name will likely change. 

#Making a day variable
krillnets.A$Start.of.Event<- as.POSIXct(krillnets.A$Start.of.Event, 
                                        format= "%d/%m/%Y %H:%M")
krillnets.A$End.of.event<- as.POSIXct(krillnets.A$End.of.event,
                                      format= "%d/%m/%Y %H:%M" )

krillnets.A$net.date<- strftime(krillnets.A$Start.of.Event, 
                                format= "%d/%m/%Y")

#removing rows with missing dates or times
#TK look into which rows these are further.. WHY are these times missing?
krillnets.A <- filter(krillnets.A, 
                      is.na(End.of.event)==FALSE)

krillnets.A <- mutate(krillnets.A, 
                      aim.start = Start.of.Event - hms(hm("3:00")), #aiming to start raws 3 hours before netting
                      aim.end = End.of.event + hms::hms(hm("00:15")), #aiming to finish raws 15 minutes after netting
                      
                      #Making the blank columns to fill in the loop
                      raw.start= NA, 
                      raw.end = NA,
                      startfilename= NA, 
                      endfilename= NA)


rawneeded<- (NULL) #making a blank vector to fill in the loop


for(i in (1:nrow(krillnets.A))){
  
  
  net.start <- krillnets.A$aim.start[i] #maybe should rename this , this isn't the true net sstart but three hours later. 
  net.end <- krillnets.A$aim.end[i] #ditto this
  
  net.date<- as.POSIXct(krillnets.A$net.date[i], format= "%d/%m/%Y")
  
  prev.date<-net.date - lubridate::days(1) #day before
  next.date<-net.date + lubridate::days(1) #day after net
  
  #making the dates back into characters
  net.date<- as.character(net.date, '%d/%m/%Y') 
  prev.date<- as.character(prev.date, '%d/%m/%Y')
  next.date<- as.character(next.date, '%d/%m/%Y')
  
  #Filtering raw files to make a dataframe of same day as net, and one day either side
  rawsubset<- filter(rawfiles, 
                     date == paste0(net.date) |
                       date== paste0(next.date) |
                       date== paste0(prev.date))
  
  
  tempv1<- (NULL)
  for(j in (1:nrow(rawsubset))) {
    if (net.start > rawsubset$date.time[j]) {
      tempv1<- c(tempv1,paste0(rawsubset$date.time[j]))
    }
    
  }
  raw.start<- paste0(max(tempv1))
  
  
  
  tempv2<- (NULL)
  for(j in (1:nrow(rawsubset))) {
    if (net.end < rawsubset$date.time[j]) {
      tempv2<- c(tempv2,paste0(rawsubset$date.time[j]))
    }
    
  }
  raw.end<- paste0(min(tempv2))
  
  print(raw.start<net.start)#will print true if loop has worked correctly
  print(raw.end>net.end) #will print true if loop has worked correctly
  
  krillnets.A$raw.start[i] <- raw.start #filling the krillnets dataframe with the start time of the first raw
  krillnets.A$raw.end[i] <- raw.end #filling the krillnets dataframe with the end time of the last raw
  
  rawsubsetstart <- filter(rawsubset, 
                           date.time== paste0(raw.start))
  rawsubsetend <- filter(rawsubset, 
                         date.time== paste0(raw.end))
  
  krillnets.A$startfilename[i]<- rawsubsetstart$filename
  krillnets.A$endfilename[i]  <- rawsubsetend$filename
  
  
  rawneeded4net <- filter(rawsubset,
                          date.time>= raw.start &
                            date.time<= raw.end)
  
  rawneeded4net<- rawneeded4net$cruiseraws
  
  
  rawneeded<- c(rawneeded, rawneeded4net) #making a vector of all the raw files needed. 
  
  print(paste0("Processed ", i, " out of ", nrow(krillnets.A)))
  
}

ref.A <- select(krillnets.A,
                common.code, Cruise.name, Start.of.Event, End.of.event, 
                raw.start, raw.end, startfilename, endfilename)


write.csv(ref.A, "D:\\Natalie\\ref.A.csv")


#===============================================================================

#FILE COPYING
file.copy(from= rawneeded, to= targetdir, overwrite=T)  


