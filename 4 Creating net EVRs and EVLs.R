#4 Getting net EVRs and EVLs (from depth data)

#Natalie Nickells 
#2nd Jan 2024

#Aim: To create EVLs (EVRs?) for each net, using net start/end times and depth data. 

#This script will be written in R Studio but version control will be used, 
#in GitHub repo Krill_Length_Frequency
#This script is an updated version/draws heavily on Creating EVL files 311023.R, 
#at filepath "D:\R code\Creating EVL files 311023.R"

#Data sources:

#Data outputs:

#Current known errors: 




#TK the below is copied over from the Creating EVL files 31 10 23 script. Need to go through and refine. 
#As it is poster prep, does not deal with the full range of cruises that I want to deal with. 

#Aim: To create EVL files from net data in ACO and TPL formats. 
#These EVL files will show the 'line of travel' of the net and can then be read into Echoview. 

#For nets for which there is no data, net lines will be set at 50m. 
#PREPARATION==============================================================================
#Setting working directory 
setwd("E:/")

#Installing Echoview R
#install.packages(c('sp','geosphere','maptools','RDCOMClient','lubridate'))
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type = "win.binary")
#if (!requireNamespace("devtools",quietly=TRUE)) install.packages("devtools")
# Latest devtools syntax
#devtools::install_github("AustralianAntarcticDivision/EchoviewR", build_opts = c("--no-resave-data", "--no-manual"))
## For devtools v < 2.0
#devtools::install_github("AustralianAntarcticDivision/EchoviewR", build_vignettes = TRUE, force_deps=TRUE)

#Loading required packages
library(tidyverse)
library(EchoviewR)

#Reading in ACO files
#IMPORTANT: Before reading in ACO files, need to open them in Notepad and save them as txt. 
#(Naming convention= "netmonitorACO.txt")
#If they are read in as aco, using the 'swatches' package, a vector of 00s is generated. (error!)

#Reading in net monitor data: tis is data for the whole JR15002 cruise
nm <- read.table("/Cruise_data/JR15002/netmonitor.txt", sep=",")
#17 columns, currently with no headers. Need to add these headers.

tplcolnames <- read.table("/Cruise_data/JR15002/netmonitor.tpl.colnames.txt")
tplcolnames<- unlist(as.vector(tplcolnames))


colnames(nm)<- c("Year", "J.Date", "J.Day", "J.Time", tplcolnames)

#Figuring out format needed for EVL =======================================

#Reading in an example EVL so I can see the format I need. 

#hash out these lines if running on home laptop, wihtout using echoview r.
#exmpl.evl.r <- readevl2R("/Cruise_data/JR177/Net lines/net-27.evl")
#str(exmpl.evl.r)

#the structure of exmpl.evl.r (R EVL format) seems much more complicated than 
#just creating a txt file like the EVR. So I will approach using txt file rather than EchoviewR function. 
#either way, I'll need the same info: 
#date, in yyyymmdd format
#time in hhmmss0000 format
#depth to 6 decimal places

#then just a matter of formatting. 


#Formatting netmonitor information to match EVL formatting=========================================

#Changing Julian dates to standard date time values (Gregorian)
nm$G.Date <- as.POSIXct(as.Date(nm$J.Date, 
                                origin = as.Date(paste0(nm$Year, "-01", "-01"))))

#Adding columns for day and time correctly (Formatted to use for evl files)
nm<- mutate(nm, 
            F.day =format(as.POSIXct(G.Date), "%Y%m%d"), 
            F.time = format(as.POSIXct(G.Date), "%H%M%S"),
            F.time = paste0(F.time, "0000"))

#Tidying up to select just the columns I need, give them appropriate column names. 
nm.f <- select(nm, G.Date, F.day, F.time, `159,netmonitor-depth,m`)

colnames(nm.f) <- c("date", "f.day", "f.time", "f.depth.m")

#formatting depths to have 6 decimal places. 
nm.f$f.depth.m<- format(round(nm.f$f.depth.m, 6), nsmall = 6)



#Next: need to find the time the net opened/closed. Then pull out those depths.
#Currently testing so will just pull out JR15002, however when doing all, have one for full cruise. 

net.tims<- read.csv("krillnets.A.txt",  fill=T, sep= " ")
net.tims<- filter(net.tims,Cruise.name== "JR15002")
#for some reason missing closing time fo rnet JR15002_112_1, adding this from rmt_events doc
net.tims$End.of.event[7]<- "07/12/2015 23:04"

net.tims<- mutate(net.tims, 
                  Start.of.Event =as.POSIXct(Start.of.Event, format= "%d/%m/%Y %H:%M"), 
                  End.of.event =as.POSIXct(End.of.event, format= "%d/%m/%Y %H:%M"))


net.tims<- arrange(net.tims, Start.of.Event)#arranging nets in chronological order


#Identifying the times for which depths are needed (for loop)===================
#Currently, just using JR15002 for testing. 

#Making dataframes to populate with the for loop
net.tims.nm<-net.tims 
temp2<- as.data.frame(NULL)
temp4<- as.data.frame(NULL)
colnames(net.tims)


#For loop (just for one cruise, will expand to apply to all cruises?)
for(i in 1:nrow(net.tims)){ #for each net
  
  #i<- 6 #testing
  
  net<- net.tims$common.code[i] 
  start.time<- as.POSIXct(net.tims$Start.of.Event[i])
  end.time<- net.tims$End.of.event[i]
  
  for (j in 1: nrow(nm.f)) {
    if (start.time < nm.f$date[j] ){ #finding row after start time
      break}}
  
  #assigning row and time at which to start needing depths. 
  row.start<- j-1
  nm.start.time<- nm.f$date[row.start]
  
  #putting data from loop into dfs to bind/join post-loop
  temp<- data.frame(common.code= net, 
                    row.start=  row.start,
                    nm.start.time = nm.start.time)
  temp2<- rbind(temp, temp2)
  
  
  for (k in 1: nrow(nm.f)) { #this for loop does the same as prev
    #except for end times and rows instead of start
    if (end.time < nm.f$date[k] ){
      break}}
  
  row.end<- k
  nm.end.time<- nm.f$date[row.end]
  
  temp3<- data.frame(common.code= net, 
                     row.end=  row.end,
                     nm.end.time = nm.end.time)
  temp4<- rbind(temp3, temp4)
}

#binding dfs from loop 
temp5<- full_join(temp2, temp4) 
net.tims.nm<-full_join(net.tims.nm, temp5)


#Data problem: for JR15002 at least, there are big gaps where the data hasn't 
#been recorded once the net is open... triple check this. 
#possibly talk to sophie.. is there a fieldwork reason for thiss?
plot(nm.f$date)


#Making depth/time dataframe for each net=======================================
unique(net.tims.nm$common.code) #list of nets


for(i in 1: length(net.tims.nm$common.code)) { #for each net
  
  start.row <- net.tims.nm$row.start[i]
  print(start.row)
  end.row <- net.tims.nm$row.end[i]
  print(end.row)
  
  temp <- select(data.frame(nm.f[ start.row:end.row, ]), 
                 f.day, f.time, f.depth.m)
  
  temp<- mutate(temp, 
                v4= 3)
  
  assign(paste(net.tims.nm$common.code[i],".depths"), 
         data.frame(temp))
}



#Making depth/time dataframe for nets missing net monitor data
#just putting a constant line at depth 50m

`JR15002_79_1 .depths`<- mutate(`JR15002_79_1 .depths`, 
                                f.depth.m= "50.00000")

`JR15002_79_2 .depths`<- mutate(`JR15002_79_2 .depths`, 
                                f.depth.m= "50.00000")

`JR15002_106_1 .depths`<- mutate(`JR15002_106_1 .depths`, 
                                 f.depth.m= "50.00000")

`JR15002_106_2 .depths`<- mutate(`JR15002_106_2 .depths`, 
                                 f.depth.m= "50.00000")

`JR15002_107_1 .depths`<- mutate(`JR15002_107_1 .depths`, 
                                 f.depth.m= "50.00000")

`JR15002_107_2 .depths`<- mutate(`JR15002_107_2 .depths`, 
                                 f.depth.m= "50.00000")

`JR15002_112_1 .depths`<- mutate(`JR15002_112_1 .depths`, 
                                 f.depth.m= "50.00000")

#Although these are a constant depth, they still don't only start/end at net start/end times. Need to address that. 
#Will need to put net start/end times into formated days and times

#dataframe that has the times in is net.tims. 

net.tims.f<- net.tims #making dataframe i can reformat
library(clock)
net.tims.f<- mutate(net.tims.f, #reformatting all my start/end times to be appropriate to make EVL file
                    f.startday= date_format(Start.of.Event, format= "%Y%m%d"),
                    f.starttim = date_format(Start.of.Event, format= "%H%M%S"),
                    f.endday= date_format(End.of.event, format= "%Y%m%d"),
                    f.endtim = date_format(End.of.event, format= "%H%M%S"))

net.tims.f<- arrange(net.tims.f, Start.of.Event)# arrange net.tims.f by net order.

nm.f$f.depth.m<- format(round(nm.f$f.depth.m, 6), nsmall = 6)


`JR15002_79_1 .depths`<- mutate(`JR15002_79_1 .depths`, 
                                f.day= c(net.tims.f$f.startday[1], net.tims.f$f.endday[1]),
                                f.time= c(net.tims.f$f.starttim[1], net.tims.f$f.endtim[1]))

`JR15002_79_1 .depths`<- mutate(`JR15002_79_1 .depths`, 
                                f.time= paste0(f.time,"0000" ))

`JR15002_79_2 .depths`<- mutate(`JR15002_79_2 .depths`, 
                                f.day= c(net.tims.f$f.startday[2], net.tims.f$f.endday[2]),
                                f.time= c(net.tims.f$f.starttim[2], net.tims.f$f.endtim[2]))

`JR15002_79_2 .depths`<- mutate(`JR15002_79_2 .depths`, 
                                f.time= paste0(f.time,"0000" ))

`JR15002_106_1 .depths`<- mutate(`JR15002_106_1 .depths`, 
                                 f.day= c(net.tims.f$f.startday[3], net.tims.f$f.endday[3]),
                                 f.time= c(net.tims.f$f.starttim[3], net.tims.f$f.endtim[3]))

`JR15002_106_1 .depths`<- mutate(`JR15002_106_1 .depths`, 
                                 f.time= paste0(f.time,"0000" ))

`JR15002_106_2 .depths`<- mutate(`JR15002_106_2 .depths`, 
                                 f.day= c(net.tims.f$f.startday[4], net.tims.f$f.endday[4]),
                                 f.time= c(net.tims.f$f.starttim[4], net.tims.f$f.endtim[4]))
`JR15002_106_2 .depths`<- mutate(`JR15002_106_2 .depths`, 
                                 f.time= paste0(f.time,"0000" ))

`JR15002_107_1 .depths`<- mutate(`JR15002_107_1 .depths`, 
                                 f.day= c(net.tims.f$f.startday[5], net.tims.f$f.endday[5]),
                                 f.time= c(net.tims.f$f.starttim[5], net.tims.f$f.endtim[5]))
`JR15002_107_1 .depths`<- mutate(`JR15002_107_1 .depths`, 
                                 f.time= paste0(f.time,"0000" ))

`JR15002_107_2 .depths`<- mutate(`JR15002_107_2 .depths`, 
                                 f.day= c(net.tims.f$f.startday[6], net.tims.f$f.endday[6]),
                                 f.time= c(net.tims.f$f.starttim[6], net.tims.f$f.endtim[6]))
`JR15002_107_2 .depths`<- mutate(`JR15002_107_2 .depths`, 
                                 f.time= paste0(f.time,"0000" ))

`JR15002_112_1 .depths`<- mutate(`JR15002_112_1 .depths`, 
                                 f.day= c(net.tims.f$f.startday[7], net.tims.f$f.endday[7]),
                                 f.time= c(net.tims.f$f.starttim[7], net.tims.f$f.endtim[7]))
`JR15002_112_1 .depths`<- mutate(`JR15002_112_1 .depths`, 
                                 f.time= paste0(f.time,"0000" ))





#Formatting as EVL file=========================================================

print(exmpl.evl.r)
#this format looks quite complicated.. possibly easier to just try to format the txt the right way?

exmpl.evl.txt <- read.table("JR177/Net lines/net-27.evl", skip=2)
#structure is, 4 variables, (formatted date, formatted time, depth to 4dp, the number 3.)




#Appending 

fileConn<- file("JR15002_79_1.evl")
data1<- `JR15002_79_1 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_79_1.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_79_2.evl")
data1<- `JR15002_79_2 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_79_2.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_106_1.evl")
data1<- `JR15002_106_1 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_106_1.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_106_2.evl")
data1<- `JR15002_106_2 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_106_2.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_107_1.evl")
data1<- `JR15002_107_1 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_107_1.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_107_2.evl")
data1<- `JR15002_107_2 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_107_2.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_112_1.evl")
data1<- `JR15002_112_1 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_112_1.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)

fileConn<- file("JR15002_113_2.evl")
data1<- `JR15002_113_2 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_113_2.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)


fileConn<- file("JR15002_113_1.evl")
data1<- `JR15002_113_1 .depths` 
line1<- c("EVBD 3 4.0.75.6342", nrow(data1))
writeLines((line1), fileConn)
write.table(data1, "JR15002_113_1.evl", append= T, row.names= F, col.names=F, quote=F)
close(fileConn)






