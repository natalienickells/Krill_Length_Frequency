#Date: 30th August 2023
#Author:Natalie Nickells

#Aim: To create EVL files from net data in ACO and TPL formats. 
#These EVL files will show the 'line of travel' of the net and can then be read into Echoview. 


#PREPARATION==============================================================================
#Setting working directory 
setwd("D:/Cruise_data/")

#Loading required packages
library(tidyverse)

#Reading in ACO files
#IMPORTANT: Before reading in ACO files, need to open them in Notepad and save them as txt. 
#(Naming convention= "netmonitor.txt")
#If they are read in as aco, using the 'swatches' package, a vector of 00s is generated. (error!)

nm <- read.table("JR15002/netmonitor.txt", sep=",")
#17 columns, currently with no headers. Need to add these headers.
  
tplcolnames <- read.table("JR15002/netmonitor.tpl.colnames.txt")
tplcolnames<- unlist(as.vector(tplcolnames))


colnames(nm)<- c("Year", "J.Date", "J.Day", "J.Time", tplcolnames)



