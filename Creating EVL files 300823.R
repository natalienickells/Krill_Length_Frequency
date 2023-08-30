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
#If they are read in as aco, using the 'swatches' package, a vector of 00s is generated. 




  