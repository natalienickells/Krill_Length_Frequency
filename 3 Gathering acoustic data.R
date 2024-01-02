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

#0 SET UP========================================================================
library(tidyverse)
library(fs)
#Loading in useful data

cruise.folders <-  read.csv("D:/Natalie/Cruises&theirfolders.csv")
cruise.folders$Folder<-as.character(cruise.folders$Folder)

#1 Making a dataframe of all the available .raw files===========================
#Making dataframe column of source directories
cruise.folders$sourcedir <- paste0( "S:/cruise_data/", cruise.folders$Folder)

targetdir<- ("D:/Natalie/JCRRawFiles") 


rawfiles<- as.data.frame(NULL) #making blank df to fill in the loop

for(i in (1:(nrow(cruise.folders)))){
  sourcedir<- paste0(cruise.folders$sourcedir[i]) #assign source directory
  
  cruiseraws <- list.files(sourcedir, pattern= 'D.+T.+raw$', full.names=TRUE, recursive=TRUE)
  
  #then filter cruise raws to be ek60 only and no work
  
  cruiseraws <- as.data.frame(cruiseraws) #making into a dataframe so can filter
  
  cruiseraws <- filter(cruiseraws, 
                       (grepl("ek60", cruiseraws, ignore.case=T)) & #ek60 present in filepath
                         (!grepl("work",cruiseraws, ignore.case=T)) & #work not present in filepath
                         (!grepl("calibration", cruiseraws, ignore.case=T ))) 
  
  rawfiles<- rbind(cruiseraws, rawfiles)
}
#some error codes- overlong paths. 11 files affected. COME BACK TO


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

rawfiles<- select(rawfiles, cruiseraws, date.time)

#Ordering dataframe by time
rawfiles<- rawfiles[order(rawfiles$date.time),]
#Adding rownumbers
rawfiles$row.no <- c(1:(nrow(rawfiles))) #do i actually ever use row numbers later? check if i need

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

#Making a day variable
krillnets.A$Start.of.Event<- as.POSIXct(krillnets.A$Start.of.Event, 
                                        format= "%d/%m/%Y %H:%M")
krillnets.A$End.of.event<- as.POSIXct(krillnets.A$End.of.event,
                                      format= "%d/%m/%Y %H:%M" )

krillnets.A$net.date<- strftime(krillnets.A$Start.of.Event, 
                                format= "%d/%m/%Y")

#removing rows with missing dates or times

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
  
  
  net.start <- krillnets.A$aim.start[i]
  net.end <- krillnets.A$aim.end[i]
  
  net.date<- as.POSIXct(krillnets.A$net.date[i], format= "%d/%m/%Y")
  
  prev.date<-net.date - lubridate::days(1)
  next.date<-net.date + lubridate::days(1)
  
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
  
  
  rawneeded<- c(rawneeded, rawneeded4net)
  
  print(paste0("Processed ", i, " out of ", nrow(krillnets.A)))
  
}

ref.A <- select(krillnets.A,
                common.code, Cruise.name, Start.of.Event, End.of.event, 
                raw.start, raw.end, startfilename, endfilename)


write.csv(ref.A, "D:\\Natalie\\ref.A.csv")


#===============================================================================

#FILE COPYING
file.copy(from= rawneeded, to= targetdir, overwrite=T)  


