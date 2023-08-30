#Date: 30th August 2023
#Author:Natalie Nickells

#Aim: To create EVL files from net data in ACO and TPL formats. 
#These EVL files will show the 'line of travel' of the net and can then be read into Echoview. 


#PREPARATION==============================================================================
#Setting working directory 
setwd("D:/Cruise_data/")

#Loading required packages
library(tidyverse)
library(EchoviewR)

#Reading in ACO files
#IMPORTANT: Before reading in ACO files, need to open them in Notepad and save them as txt. 
#(Naming convention= "netmonitor.txt")
#If they are read in as aco, using the 'swatches' package, a vector of 00s is generated. (error!)

nm <- read.table("JR15002/netmonitor.txt", sep=",")
#17 columns, currently with no headers. Need to add these headers.
  
tplcolnames <- read.table("JR15002/netmonitor.tpl.colnames.txt")
tplcolnames<- unlist(as.vector(tplcolnames))


colnames(nm)<- c("Year", "J.Date", "J.Day", "J.Time", tplcolnames)


#Reading in an example EVL so I can see the format I need. 
exmpl.evl.r <- readevl2R("JR177/Net lines/net-27.evl")
str(exmpl.evl.r)
#the structure of exmpl.evl.r (R EVL format) seems much more complicated than 
#just creating a txt file like the EVR....
#either way, I'll need the same info to make: 
#date, in yyyymmdd format
#time in hhmmss0000 format
#depth to 6 decimal places

#then just a matter of formatting. 




