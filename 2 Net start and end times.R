# 2 Getting net start and end times


#Natalie Nickells
#3rd January 2024

#Aim: to get net start and end times from a list of nets where krill were measured. 


#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency

#This script is an updated version/draws heavily on "Testing_ExtractingEventNames_JR17002.R", 
#at filepath D:\R code\Testing_ExtractingEventNames_JR17002.R

#Data sources:
#netnames.csv, created in script '1 Lising nets and cruises where krill were measured', also in GitHub repo Krill_Length_Frequency


#Current known errors:
#SET UP================================================================================

#importing list of net names
netnames<- read.csv("netnames.csv")

#in script "Testing_ExtractingEventNames_JR17002.R", 
#was using a standard layout of script, net data provided by PDC. Now will have all different types.

#First of all, will want to set up a loop to import that net data like I had in klf data
#importing start and end times, and depths (maybe diff cols for the depth, min max mean etc. mnention i have tracked if i have)

#for now, can i scoot past this step? I already have net times for a lot of these cruises although it's not perfect. 
#would be easier to read it all off the one file (depth too) later but for now just stick with the net times I made earlier. 


#IF I CAN FIND SCRIPT I MADE THEM IN, PUT THE CODE HERE, AND THE FILEPATH TO THE SCRIPT. 