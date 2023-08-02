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


#2 TESTING OUTPUTTING NET TIMES FOR JUSTONE CRUISE, JR 15002==================

#the following is from JR15002_CombiningnetandKLFdata.R

#0 SET UP =====================================================================
# Install the package if not installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load the dplyr package
library(dplyr)
library(chron)#this will clash with lubridate
library(tidyverse)

setwd('C:\\Users\\nanick73\\OneDrive - NERC\\Documents\\Chapter2_KLF\\Data')

#Reading in data --------------------------------------------------------------
#Reading in the net data, blanks as NA
ND <- read.csv("JR15002_NETDATA.csv",  na.strings = c("", "NA"))

#Reading in KLF data, blanks as NA
KLF <- read.csv("JR15002_KRILLBASE_KLF.csv",  na.strings = c("", "NA"))


#Examining the data
colnames(KLF) #GMTtime is the time column
colnames(ND) #Start.of.event and End.of.event are the time columns


#1 CHANGING FORMAT OF TIME AND DATE ===========================================

#Need to change time format in KLF and ND by adding extra columns

#Splitting ND start time into date and time-----------------------------------
#starts = t(as.data.frame(strsplit(ND$Start.of.Event,' ')))
#colnames(starts) = c('start.date', 'start.time')
#rownames(starts)=NULL
#starts <- as.data.frame(starts)

#Slitting ND end time into date and time--------------------------------------
#ends = t(as.data.frame(strsplit(ND$End.of.event,' ')))
#colnames(ends) = c('end.date', 'end.time')
#rownames(ends)=NULL
#ends <- as.data.frame(ends)

#Adding split columns into ND-------------------------------------------------
#ND <- mutate(ND, 
# start.date= as.Date(starts$start.date,format="%d/%m/%Y"), 
#start.time = starts$start.time,
#end.date= as.Date(ends$end.date,format="%d/%m/%Y"), 
#end.time= ends$end.time)


#Changing times from characters to dates---------------------------------------
#Changing ND Start.of.Event and ND End.of.event to time structures (rather than character)
ND <- mutate(ND, 
             Start.of.Event = as.POSIXct
             (Start.of.Event, format= "%d/%m/%Y %H:%M"),
             End.of.event = as.POSIXct
             (End.of.event, format= "%d/%m/%Y %H:%M"))





#Making sure date and time are in the same format in ND and KLF------------------

KLF <- mutate( KLF, 
               Date= as.Date(Date, format = "%d-%b-%y"))


KLF <- mutate(KLF,
              Date.time = as.POSIXct
              (paste(KLF$Date, KLF$GMT.time), format="%Y-%m-%d %H:%M:%S"))



#2 CUTTING AND FILTERING THE DATA==============================================

#Leaving only the columns I want
hist(KLF$Total.krill.in.sample) #never less than 60 krill, all good.
unique(KLF$Net.mouthm2)
unique(KLF$Nettype) 
#Only RMT8 hauls here, good


#Cutting the KLF data to just the data I need----------------------------------
KLFcut <- select(KLF, Date.time, Cruise, Ref..haul.identifier., Mean.length..mm., Date, GMT.time)


#Cutting ND data to what I need---------------------------------------------
NDcut <- select(ND, 
                Start.of.Event,
                Lat,
                Lon, 
                End.of.event,
                End.lat,
                End.lon,
                Event.name)


#In JR15002.nd, have the same net data replicated for each species that was counted.
#Don't need the hauls replicated per species, just want the net data
#So removing columns about taxon names and abundance

#Removing duplicate rows
NDcut<- NDcut[!duplicated(NDcut), ]




#MATCHING UP THE NET AND KLF DATA===============================================

KLFcut <-mutate(KLFcut,
                netname= NA)
NDcut <- mutate(NDcut, 
                klfdata= NA)

for (i in 1: nrow(NDcut)){
  netstart <- NDcut$Start.of.Event[i]
  netend <- NDcut$End.of.event[i]
  netname <- NDcut$Event.name[i]
  
  if ((is.na(netstart)==TRUE) | (is.na(netend) ==TRUE)) {
    end
  } else {
    for (j in 1:nrow(KLFcut)) {
      if (KLFcut$Date.time[j] >= netstart &&
          KLFcut$Date.time[j] <= netend) {
        KLFcut$netname[j] <- netname
        NDcut$klfdata[i] <- 'YES'
      } 
    }
    
  }} 


#Joining the dataframes

NDcut <- mutate(NDcut, 
                netname= Event.name)


#Filtering for net data where only KLF data was collected
NDcut2 <- filter(NDcut, klfdata== 'YES')
KLFcut2 <- filter(KLFcut, !(is.na(netname)))
#Removing duplicate net data
NDcut2<- NDcut2[!duplicated(NDcut2), ]


fulldf<- left_join(KLFcut2, NDcut2)



fulldf <- select(fulldf, c(Cruise, Event.name, Start.of.Event, End.of.event, Mean.length..mm.))


write.table(fulldf, "JR15002_nettimings.txt", row.names= FALSE)






















#There are still some KLF nets that don't have net data


test <- filter(KLFcut, is.na(KLFcut$netname))

str(KLFcut)

which(abs(NDcut$Start.of.Event - test$Date.time[1] )) == min(abs(NDcut$Start.of.Event - test$Date.time[1], na.rm=TRUE))

test2<- mutate(NDcut, 
               closest = Start.of.Event- test$Date.time[1] )









#3 OUTPUTTING NET TIMES 

#Natalie Nickells 24th May 2023

#AIM1: Reading in krill length data format 1, to extract event numbers where krill were measured================

#Starting with JR17002 as an example dataset


#Loading required packages
library(tidyverse)
library(readxl)
library(openxlsx)

#Setting working directory
setwd("D:\\Cruise_data\\JR17002")

#Loading data
(JR17002.KLF.events <- getSheetNames('JR17002_krill_length_V2.xlsx'))
#using sheet names (assuming all sheets indicate krill measuring) to decide on KLF events
#& printing sheet names  to check for sheets like 'all'

JR17002.KLF.events<- str_remove(JR17002.KLF.events, "Ev")

#AIM2: Get times of those net events============================================

#Loading net data
JR17002.ND <- read.csv("JR17002_NETDATA.csv")


#Removing duplicate net data
#want to remove Taxon.name, Taxon.class, Abundm3, Abundm2
JR17002.ND<- select(JR17002.ND, !(c(Taxon.name, Taxon.class, Abundm3, Abundm2)))
JR17002.ND<- JR17002.ND[!duplicated(JR17002.ND), ] #then removing duplicates


#Structure of event names in ND data is CRUISE_EVENTNUMBER_NETTYPE_NETNUMBER
#Structure of event names in KLF data is EvEVENTNUMBER_NETNUMBER

#Making Separate Cols for Each
JR17002.ND[c ('Cruise', 'EventNo', 'NetType', 'NetNo')] <- (str_split_fixed(JR17002.ND$Event.name, "_",4))

#Removing"NET" off the start of Net Number 
JR17002.ND$NetNo<- str_remove(JR17002.ND$NetNo, "NET")

#Uniting event number and net number into the 'common code' 
JR17002.ND<- unite(JR17002.ND, common.code, c(EventNo, NetNo))



#Choosing only ND data for the KLF
JR17002.KLF.events <- as.data.frame(JR17002.KLF.events)

JR17002.krillnets<- merge(JR17002.ND, JR17002.KLF.events, by.x= 'common.code' , by.y= 'JR17002.KLF.events' )
colnames(JR17002.krillnets)

JR17002.krillnets <- select(JR17002.krillnets, c(Cruise, common.code, Start.of.Event, End.of.event, Event.name))

colnames(JR17002.krillnets)[2]<- "Event_Net"

write.table(JR17002.krillnets, "D:\\Cruise_data\\JR17002\\JR17002_nettimes.txt", row.names= FALSE)


view(JR17002.krillnets)

#Testing with whole group of cruises 
#Setting working directory
setwd("D:\\Cruise_data")

#Loading krill length data------------------------------------------------------
JR17002.KLF.events <- getSheetNames('.\\JR17002\\JR17002_krill_length_V2.xlsx') 
#using getSheetNames for xlsx files

JR16003.KLF.events<- excel_sheets(".\\JR16003\\Krill_lengths_JR16003.xls")
#using excel_sheets for xls files

JR15004.KLF.events<- excel_sheets(".\\JR15004\\JR15004_krill_length_frequency.xls")

JR15002.KLF.events<- excel_sheets(".\\JR15002\\krill_length_JR15002.xls")

JR304.KLF.events<- excel_sheets(".\\JR304\\krill_length_JR304.xls")

JR291.KLF.events<- excel_sheets(".\\JR291\\jr291 KRILL LENGTH.xls")

JR280.KLF.events<- excel_sheets(".\\JR280\\JR280_krill_lengths.xls")

JR260.KLF.events<- excel_sheets(".\\JR260\\JR260B_krill_length_frequency.xls")

JR177.KLF.events<- excel_sheets(".\\JR177\\jr177_krill_length_frequency.xls")

DY098.KLF.events<- getSheetNames(".\\DY098\\DY098_RMT8_krill_length.xlsx")


#================================================================================
#Standardising event and net names=============================================
#To structure CRUISEID_EVENTNO_NETNO

# JR17003-----------------------------------------------------------------------
JR17002.KLF.events<- str_remove(JR17002.KLF.events, "Ev")#removing Ev from event numbers
JR17002.KLF.events <- paste0("JR17002_", JR17002.KLF.events) #adding JR17002 to start

#JR16003-----------------------------------------------------------------------
JR16003.KLF.events<- str_remove(JR16003.KLF.events, "Event ")
JR16003.KLF.events <- paste0("JR16003_", JR16003.KLF.events)
#still need to remove 'all'

#JR15004------------------------------------------------------------------------
print(JR15004.KLF.events) #some interesting tab names, look in the excel sheet
JR15004.KLF.events<- str_remove(JR15004.KLF.events, "Event")#but can at least remove the word event
JR15004.KLF.events <- paste0("JR15004_", JR15004.KLF.events)
#this one will need a bit more processing, come back to. 

#Want to remove suffixes of frig or thyso, and tabs Thysanoessa and Frigida
JR15004.KLF.events<- as.data.frame(JR15004.KLF.events)

JR15004.KLF.events <- filter(JR15004.KLF.events, 
                             !(grepl("frig", 
                                     JR15004.KLF.events, ignore.case=T)) &
                               !(grepl("thy", 
                                       JR15004.KLF.events, ignore.case=T)) &
                               !(grepl("swarm",
                                       JR15004.KLF.events)) &
                               !(grepl("layer",
                                       JR15004.KLF.events))
)

JR15004.KLF.events<- as.vector(JR15004.KLF.events$JR15004.KLF.events)



#JR15002------------------------------------------------------------------------
print(JR15002.KLF.events)
JR15002.KLF.events<- str_remove(JR15002.KLF.events, "Event")
JR15002.KLF.events <- paste0("JR15002_", JR15002.KLF.events)
#still need to remove 'all'

#JR304-------------------------------------------------------------------------
print(JR304.KLF.events)
JR304.KLF.events<- str_remove(JR304.KLF.events, "Event")
JR304.KLF.events <- paste0("JR304_", JR304.KLF.events)
#still need to remove 'all'


#JR291-------------------------------------------------------------------------
print(JR291.KLF.events)
JR291.KLF.events<- str_remove(JR291.KLF.events, "EV")
JR291.KLF.events <- paste0("JR291_", JR291.KLF.events)
#still need to remove 'all'

#JR280--------------------------------------------------------------------------
print(JR280.KLF.events)
JR280.KLF.events<- str_remove(JR280.KLF.events, "Ev")
JR280.KLF.events <- paste0("JR280_", JR280.KLF.events)

#JR260--------------------------------------------------------------------------
print(JR260.KLF.events)
JR260.KLF.events<- str_remove(JR260.KLF.events, "Ev")
JR260.KLF.events <- paste0("JR260_", JR260.KLF.events)
#still need to remove 'all'


#JR177---------------------------------------------------------------------------
JR177.KLF.events<- str_remove(JR177.KLF.events, "E")
JR177.KLF.events <- str_replace(JR177.KLF.events, "N", "_")
#And there are some interesting tab names, look in excel sheet

#As part of the original work package, looks like they did 
#Some comparison between different types of nets ("t tests", "Net type comparison")
#And some comparison between measurers "Measurer comparison"
#nets with 'b' at the end are the second round of measuring
#As suggested in the spreadsheet, will only use the first round of measuring

#A good way to filter out all these rogue sheet names is to filter for underscores only
JR177.KLF.events <- as.data.frame(JR177.KLF.events)
JR177.KLF.events <- filter(JR177.KLF.events, 
                           (grepl("_", JR177.KLF.events)) &
                             !(grepl("b", JR177.KLF.events)) &
                             !(grepl("type", JR177.KLF.events))
)

JR177.KLF.events <- as.vector(JR177.KLF.events$JR177.KLF.events)
JR177.KLF.events <- paste0("JR177_", JR177.KLF.events)
print(JR177.KLF.events)
#TK still have JR177_28_all (both nets). Should rename to 1 and 2? hmm. Check net data.


#DY098--------------------------------------------------------------------------
print(DY098.KLF.events)
DY098.KLF.events<- str_remove(DY098.KLF.events, "RMT8_EV")
DY098.KLF.events<- str_remove(DY098.KLF.events, "RMT_EV")
DY098.KLF.events <- paste0("DY098_", DY098.KLF.events)
#Also some funky tab names
#Suffix T, ET, and EF, refer to species other than euphausia superba
#So will remove those
DY098.KLF.events <- as.data.frame(DY098.KLF.events)
DY098.KLF.events <- filter(DY098.KLF.events, 
                           !(grepl("T", DY098.KLF.events)) &
                             !(grepl("ET", DY098.KLF.events)) &
                             !(grepl("EF", DY098.KLF.events))
)
DY098.KLF.events<- as.vector(DY098.KLF.events$DY098.KLF.events)



#Combining all cruises----------------------------------------------------------

KLF.events <- c(JR17002.KLF.events, JR16003.KLF.events, JR15004.KLF.events, 
                JR15002.KLF.events, JR304.KLF.events,JR291.KLF.events,
                JR280.KLF.events, JR260.KLF.events, JR177.KLF.events, 
                DY098.KLF.events)

#Removing 'all' tabs 
KLF.events<- as.data.frame(KLF.events) 
KLF.events <- filter(KLF.events, (!grepl("all", KLF.events, ignore.case=T)))
KLF.events <- as.vector(KLF.events)

view(KLF.events)

#Net data=======================================================================

#Reading in net data & combining to one df-----------------------------------

JR17002.ND<- read.csv(".\\JR17002\\JR17002_NETDATA.csv")
JR16003.ND<- read.csv(".\\JR16003\\JR16003_NETDATA.csv")
JR15004.ND<- read.csv(".\\JR15004\\JR15004_NETDATA.csv")
JR15002.ND<- read.csv(".\\JR15002\\JR15002_NETDATA.csv")
JR304.ND<- read.csv(".\\JR304\\JR304_NETDATA.csv")
JR291.ND<- read.csv(".\\JR291\\JR291_NETDATA.csv")
JR280.ND<- read.csv(".\\JR280\\JR280_NETDATA.csv")
JR260.ND<- read.csv(".\\JR260\\JR260_NETDATA.csv")
JR177.ND<- read.csv(".\\JR177\\JR177_NETDATA.csv")
DY098.ND<- read.csv(".\\DY098\\DY098_NETDATA.csv")


#Checking structure of event names in my cruise by cruise dataset
JR17002.ND$Event.name #structure : "JR17002_100_RMT8_NET1"
JR16003.ND$Event.name #structure: "JR16003_113_RMT25_NET1" 
JR15004.ND$Event.name #"JR15004_060_RMT25_NET1" #so will need to remove 0s from numbers!
JR15002.ND$Event.name #"JR15002_78_RMT8_2_NET1" #have net number for net types
JR304.ND$Event.name #"JR304_81_RMT801_Net2"
JR291.ND$Event.name #HAVE NET NUMBERS
JR280.ND$Event.name #"JR280_10_RMT8_03_NET1" #have net numbers
JR260.ND$Event.name #"JR260B_87_RMT8_27_NET2" #have net numbers
JR177.ND$Event.name #"JR177_74_RMT25_03_NET1" #have net numbers
DY098.ND$Event.name #"DY098_101_RMT8+1_009_NET1_1" #nightmare!#COME BACK TO


#combining all those with additional net numbers
ND.A <-  rbind(JR15002.ND,JR291.ND, JR280.ND, JR260.ND, JR177.ND)
#and those with only 4 components to structure
ND.B <- rbind(JR17002.ND, JR16003.ND, JR15004.ND, JR304.ND)
#dy098 is a bit funky so leaving that off for now

(ND.A$Start.of.Event)#no 24 hour times
(ND.B$Start.of.Event)#no 24 hour times
#Removing duplicates
#want to remove Taxon.name, Taxon.class, Abundm3, Abundm2
ND.A<- select(ND.A, !(c(Taxon.name, Taxon.class, Abundm3, Abundm2)))
ND.A<- ND.A[!duplicated(ND.A), ] #then removing duplicates

ND.B<- select(ND.B, !(c(Taxon.name, Taxon.class, Abundm3, Abundm2)))
ND.B<- ND.B[!duplicated(ND.B), ] #then removing duplicates

#want to completely remove RMT25 net data
ND.A<- filter(ND.A, Net.type=="RMT8")
ND.B<- filter(ND.B, Net.type=="RMT8")

ND.A[c ('Cruise', 'EventNo', 'NetType', 'NetTypeNo', 'NetNo')] <- 
  (str_split_fixed(ND.A$Event.name, "_",5))

#different structure for group b
ND.B[c ('Cruise', 'EventNo', 'NetType', 'NetNo')] <- 
  (str_split_fixed(ND.B$Event.name, "_",4))

#Removing"NET" off the start of Net Number 
ND.A$NetNo<- str_remove(ND.A$NetNo, "NET")
ND.A$NetNo<- str_remove(ND.A$NetNo, "Net")

ND.B$NetNo<- str_remove(ND.B$NetNo, "NET")
ND.B$NetNo<- str_remove(ND.B$NetNo, "Net")

#turning event no into a numeric, to remove 0s at start
ND.B$EventNo<- as.numeric(ND.B$EventNo)
ND.B$EventNo<- as.character(ND.B$EventNo) #and back to character

ND.A<- unite(ND.A, common.code, c(Cruise,EventNo, NetNo))
ND.B<- unite(ND.B, common.code, c(Cruise,EventNo, NetNo))

ND.A$common.code #all looking good but TK need to replace JR260B with JR260 I think
ND.B$common.code #looking food apart from JR304_117_ES853... 
#Examining ND.B, looks like I can just delete that row.
ND.B <- filter(ND.B, common.code!= "JR304_117_ES853")












ND.A<- select(ND.A, 
              Cruise.name, common.code, Start.of.Event, End.of.event )
ND.B<- select(ND.B, 
              Cruise.name, common.code, Start.of.Event, End.of.event)

ND<- rbind(ND.A, ND.B)


#===============================================================================
#Joining net and klf data

KLF.events<- as.data.frame(KLF.events)
colnames(KLF.events)<- "common.code"

krillnetsA<- inner_join(KLF.events, ND)

write.table(krillnetsA, "D:\\Cruise_data\\krillnets.A.txt", row.names= FALSE)
