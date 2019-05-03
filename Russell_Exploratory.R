# Processing start time
sink('events_terminal_output.txt')
timeStart = Sys.time()


# Start writing to an output file (before first "paste" command)
cat(paste("Processing start date and time", date(), "",sep="\n"))

# Russell Project Exploratory data

# Purpose is to take preprocessed spreadsheet data and put it into a form 
# That can be analyzed using R

# ====== OVERVIEW OF DATABASE ===========
# There are 51 variables (column names) for the raw data. 
# A data dictionary describing these variables is available 
# in the supplemental document "Proposed variables for database"

# The type of data in the variables include:
#       Integer
#       Date (day-month-yyyy)
#       Factor (What is typically called a category variable)
#       Time (in UTC in four digit format)
#       Character

# ADMINISTRATIVE NOTES
# Note: To describe database at any point, use str(*name*)
# Note: To clear R workspace, use rm(list = ls())
# Note: Searching R help files - RSiteSearch("character string")
# Note: To clear console, use CTRL + L 

# ==== STATISTICS PACKAGES =====
# Just in case needed in future

options(repos = c(CRAN = "http://cran.rstudio.com"))
if("e1071" %in% rownames(installed.packages()) == FALSE) 
{install.packages("e1071")}
library(e1071)

if("rmarkdown" %in% rownames(installed.packages()) == FALSE) 
{install.packages("rmarkdown")}
library(rmarkdown)

# ==== DATA INPUT AND CLEANING ====

# READ DATA INTO DATA FRAME
# Import raw data (data files online in local directory AirSafe Consulting/Russell reports/event_database.csv)
events = NULL
events.raw = NULL
events.range = NULL
events.raw <- read.csv("event_database.csv", header = TRUE)

# Ensure that working data is in a data frame 
# Note: The raw data in file 'events.raw' will remain on standby
events = as.data.frame(events.raw)

# Ensure that working data has only complete rows
events <- events[complete.cases(events), ]


# Make everything start formatted as characters
events = as.data.frame(apply(events, 2, as.character)) 


# REMOVE LEADING OR TRAILING SPACE CHARACTERS
# Using the function trimws removes leading and trailing space characters
events = as.data.frame(apply(events, 2, trimws)) 



# CONVERT VARIABLES TO THEIR APPROPRIATE TYPES
# There are several types of data in the data frame, including
#       integers, factors, character, and date. They will 
#       be transformed prior to any anaysis


# ==Convert integer variables==

integer.vars = c("Damage.Severity", "Aircraft.Cert.Year", "Production.Year", "Total.Crew", 
                 "Crew.Deaths", "Crew.Serious.Injuries", "Total.Passengers", 
                 "Passenger.Deaths", "Passenger.Serious.Injuries", 
                 "Other.Deaths", "Other.Serious.Injuries", "Total.Deaths", 
                 "Total.Serious.Injuries")

# Index of integer variables
integer.vars.ndx = which(colnames(events) %in% integer.vars)
         
# Convert these columns from factor to numeric

# Ensure integer variables are coerced to be numeric (will be integers)
for (i in 1:length(integer.vars.ndx)) {
        events[,integer.vars.ndx[i]] = as.numeric(as.character(events[,integer.vars.ndx[i]]))
}

# Identify missing or undefined data points for integer variables
# These integer values would have either 999 or 999 as the missing, unknown, or incompete integer values
integer.null.values = c(999,9999)

# Make missing or unknown integer data into NA data points
for (i in 1:length(integer.vars.ndx)) {
        events[which(events[,integer.vars.ndx[i]] %in% integer.null.values),integer.vars.ndx[i]] = NA
}

# Print names of integer variables
cat(paste("", "Integer variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

colnames(events[integer.vars.ndx])

# ==Convert Factor variables==
factor.vars = c("Event.Type", "Operator.Category", "Aircraft.Type", "Flight.Phase", 
                "Flight.Purpose","Event.Country", "Event.IATA", "Origin.IATA", "Destination.IATA", 
                "Loss.Severity", "International.Flight", "Passenger.Flight",          
                "Scheduled.Flight", "Event.Special",     
                "Aircraft.Model", "Original.Operator","Engine.Type",
                "Investigation.Status")

# Index of factor variables
factor.vars.ndx = which(colnames(events) %in% factor.vars)


# Convert these columns to factors

# Ensure factor variables are coerced to be factors
for (i in 1:length(factor.vars.ndx)) {
        events[,factor.vars.ndx[i]] = as.factor(events[,factor.vars.ndx[i]])
}

# Identify missing or undefined data points for factor variables
# These factor values would have the following values as the missing, unknown, or incompete factor values
factor.null.values = c( "", "99", "9","999","9999", 
                       "X", "XX", "XXX", "XXXX",
                       "x", "xx", "xxx", "xxxx")


# Make missing or unknown factor data into NA data points
for (i in 1:length(factor.vars.ndx)) {
        events[which(events[,factor.vars.ndx[i]] %in% factor.null.values),factor.vars.ndx[i]] = NA
}

# Evaluating all database events
# Factor variables are categorical variables that can 
# be either numeric or string variables. 
# The variable definition document provides detailed 
# Description of both the variable and the categories 
# associated with each variable.

# Print names of factor variables 

cat(paste("","Factor variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

colnames(events[factor.vars.ndx])


# ==Convert Character variables==
# Note:Time variable 'Event.Time' is treated as a character so that it may be
#       reformatted later.

char.vars = c("Event.ID", "Event.Time", "Tail.Number", "Event.Location",
        "Origin", "Destination", "Flight.End", "Aircraft.Manufacturer", "Aircraft.Submodel",     
        "Registration.Country", "Serial.Number","Engine.Manufacturer", "Engine.Model", "Engine.Submodel",           
        "Operator.Name", "Operator.Country", "Investigating.Authority", "Probable.Cause", "Notes")

# Index of character variables
char.vars.ndx = which(colnames(events) %in% char.vars)

# Print character variables

cat(paste("", "Character variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

colnames(events[char.vars.ndx])

# Convert these columns to character

# Ensure character variables are coerced to be characters
for (i in 1:length(char.vars.ndx)) {
        events[,char.vars.ndx[i]] = as.character(events[,char.vars.ndx[i]])
}


# Identify missing or undefined data points for character variables
# These character values would have the following values as the missing, unknown, or incompete factor values
char.null.values = c("X", "XX", "XXX", "XXXX", "9999",
                       "x", "xx", "xxx", "xxxx")


# Make missing or unknown factor data into NA data points
for (i in 1:length(char.vars.ndx)) {
        events[which(events[,char.vars.ndx[i]] %in% char.null.values),char.vars.ndx[i]] = NA
}


# ==Convert Date Variable ==

# There is only one date variable, Event.Date, with raw data format DD-Month-YYYY.
#       Will change format to R standard, so will turn it to character and
#       then to standard date format YYY-MM-DD.

events$Event.Date = as.character(events$Event.Date)
events$Event.Date = as.Date(events$Event.Date, "%d-%B-%Y")


# ==Convert Time Variable==

# The time variable is in the 24 hour clock format. CSV files save
#       it as an integer, so it is treated as such when it comes to
#       identifying a missing time value, which is coded as 9999. 

# The variable 'Event.Time' is in the 24 hour (HHMM) time format,  
#       but the leading zeros may have been eliminated during
#       initial uploading. The goal is to put it into the 
#       form HH:MM. Must do a combination of adding leading zeros 
#       and inserting a colon to get to HH:MM format

# Paste string "00:0 if one character long
one.long.ndx = which(nchar(events$Event.Time)==1)

events$Event.Time[one.long.ndx] = paste0("00:0",events$Event.Time[one.long.ndx])

# Add two leading zeros and colon if two characters long
# Becasue 'NA' is two characters long, need to 
#       find intersection of those not NA and those
#       two characters long

non.NA.values = which(!is.na(events$Event.Time))
two.long.all = which(nchar(events$Event.Time)==2)                     
two.long.ndx = intersect(non.NA.values, two.long.all)

events$Event.Time[two.long.ndx] = paste0("00:",events$Event.Time[two.long.ndx])

# Add one leading zero insert colon before last two characters if three characters long
three.long.ndx = which(nchar(events$Event.Time)==3)

events$Event.Time[three.long.ndx] = paste0("0",substr(events$Event.Time[three.long.ndx], 1,1),":",substr(events$Event.Time[three.long.ndx], 2,3))


# Insert colon after second character if time is four characters long (1000 to 2359)
four.long.ndx = which(nchar(events$Event.Time)==4)

events$Event.Time[four.long.ndx] = paste0(substr(events$Event.Time[four.long.ndx], 1,2),":",substr(events$Event.Time[four.long.ndx], 3,4))


# ====Post cleaning actions====
# After cleaning the database, including setting missing
# or unknown variables to NA, will summarize both the entire database and the 
# portion of the database that is of greatest interest.
# For the initial study, the is the period 2008-2018 inclusive.

# === Summary of complete database ===

cat("\n")
paste("=== Summary of the complete database ===")
cat("\n")

paste("Aircraft event date range for full database:", 
      min(events$Event.Date),"to",
      max(events$Event.Date))
cat("\n")


paste("Date variable is Event.Date")
cat("\n")

paste("Number of aircraft events in total database -",nrow(events))
cat("\n")

paste("Number of unique events in total database -",length(which(substr(events$Event.ID, nchar(events$Event.ID)-1, nchar(events$Event.ID))=="01")))

# Can determine population of multiple events indirectly.
# The last two characters are from variables that can be 
# reasonably summarized include all factor and integer 
# variables. Using the sorted index values will result 
# in summaries in the same order as in the spreadsheet 
# that makes up the database.
# The ordered index is sort(c(integer.vars.ndx,factor.vars.ndx))

# paste("Summaries of data from factor and integer variables")
# apply(events[,sort(c(integer.vars.ndx,factor.vars.ndx))],2,table, useNA = "always")

# === Summary of the study period ===
cat("\n")
paste("=== Summary of the study period ===")
cat("\n")

# Create an index of records of aircraft events that fall in the study period
events.range.ndx = events$Event.Date>=as.Date("2008-01-01") & events$Event.Date<=as.Date("2018-12-31")
events.range = events[events.range.ndx,]
events.range.unique.ndx = which(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1, nchar(events.range$Event.ID))=="01")


# ==== ENSURE Event.ID VARIABLES IN STUDY PERIOD ARE UNIQUE ====

# Variable designated identifier variable (Event.ID) variable
# is unique (number of unique values equal to the number of records)

# First step is to see if there is only one variable with unique values

unique.ndx = which(lapply(lapply(events.range,unique),length)==nrow(events.range))

if (length(unique.ndx)==0){
        print("No variable can serve as a unique identifier.")
}

if (length(unique.ndx)==1)paste("Variable '", colnames(events)[unique.ndx],
                                "' can serve as a unique identifier.", sep = "")

if (length(unique.ndx)>1){
        print("The following variables that can serve as unique identifiers:")
        colnames(events)[unique.ndx]
}
# ======= 


cat("\n")
paste("Aircraft event date range for study period:", 
      min(events.range$Event.Date),"to",
      max(events.range$Event.Date))

cat("\n")
paste("Number of aircraft events during the study period -",nrow(events.range))

cat("\n")
paste("Number of unique events during the study period -",length(events.range.unique.ndx))

# Number of events involving two or more aircraft
num.dup.events.study.period = length(which(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1, nchar(events.range$Event.ID))=="02"))

cat("\n")
paste("Number of events involving two or more aircraft during the study period -",num.dup.events.study.period)

# Table of events by year (during study period)
cat("\n")
paste("Table of events by year (during study period)")
table(as.integer(format(as.Date(events.range$Event.Date, format="%Y-%m-%d"),"%Y")))

# Table of events by Loss.Severity (during study period)
cat("\n")
paste("Table of events by Loss.Severity (during study period) where M is Major Loss, A is Attrition Loss, and U is undamaged")
cat("\n")
sort(table(events.range$Loss.Severity, useNA = "ifany"), decreasing = TRUE)


# For events with and loss severity considered to be a Major Loss
#       (detailed definition of Major Loss in Proposed Variables for Database document)
#       there was a range range of percentage of aircraft value lost.

cat("\n")
paste(length(which(events.range$Damage.Severity>0)), "events had a known Damage.Severity value >= 0, with",
      length(which(events.range$Damage.Severity==0)), "of them equal to zero")

cat("\n")
paste("Sorted distribution of events with a known Damage.Severity value for", length(which(events.range$Damage.Severity>=0)), "total events")
sort(table(events.range$Damage.Severity[which(events.range$Damage.Severity>=0)], useNA = "always"), decreasing = TRUE)

cat("\n")
paste("Sorted distribution of events with a known Damage.Severity value for Major loss events")
sort(table(events.range$Damage.Severity[which(events.range$Damage.Severity>=0 & events.range$Loss.Severity=="M")], useNA = "always"), decreasing = TRUE)

# SUMMARY OF TOTAL DEATHS AND TOTAL SERIOUS INJURIES IN PERIOD OF INTEREST

# Total.Deaths overview
cat("\n")
paste(length(which(events.range$Total.Deaths>=0)), "events from 2008-2018 had a known Total.Deaths value of zero or greater, with",
      length(which(events.range$Total.Deaths>0)), "of them with at least one fatality")

cat("\n")
paste("Sorted distribution of percent of Total.Deaths value")
sort(table(events.range$Total.Deaths[which(events.range$Total.Deaths>0)], useNA = "always"), decreasing = TRUE)

#Total.Serious.Injuries overview
cat("\n")
paste(length(which(events.range$Total.Serious.Injuries>=0)), "events from 2008-2018 had a known Total.Serious.Injuries value of zero or greater, with",
      length(which(events.range$Total.Serious.Injuries>0)), "of them with at least one serious injury")

cat("\n")
paste("Sorted distribution of percent of Total.Serious.Injuries value")
sort(table(events.range$Total.Serious.Injuries[which(events.range$Total.Serious.Injuries>0)], useNA = "always"), decreasing = TRUE)

#Overview of events with deaths and serious injuries
cat("\n")
paste(length(which(events.range$Total.Serious.Injuries>=0 & events.range$Total.Deaths>=0)), "events from 2008-2018 had a known value for deaths and serious injuries, with",
      length(which(events.range$Total.Serious.Injuries>0 & events.range$Total.Deaths>0)), "of them with at least one serious injury and one fatality")

both.fatal.and.injury.ndx = which(events.range$Total.Serious.Injuries>0 & events.range$Total.Deaths>0)

both.fatal.and.injury = events.range$Total.Serious.Injuries[both.fatal.and.injury.ndx] + events.range$Total.Deaths[both.fatal.and.injury.ndx]

cat("\n")
paste("Sorted distribution of events with both serious injuries and fatalities")
sort(table(both.fatal.and.injury, useNA = "always"), decreasing = TRUE)



# Summary of data from the factor and integer variables

# Variables that can be reasonably summarized include
# all factor and integer variables. Using the sorted 
# index values will result in summaries in the same
# order as in the spreadsheet that makes up the database
# The ordered index is sort(c(integer.vars.ndx,factor.vars.ndx))

cat("\n")
cat(paste("","Summaries of data from factor and integer variables for study period","",sep="\n"))
apply(events.range[,sort(c(integer.vars.ndx,factor.vars.ndx))],2,table, useNA = "always")

# Can indirectly compute number of multiple events.
# Do a table of the last two Event.ID characters. 
cat(paste("","Table of the last two Event.ID characters (multiple event indicator)","",sep="\n"))
table(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1,nchar(events.range$Event.ID)))

# Note: The following gives the index of events that have an EventID
#       ending in '02', indicating a multiple aircraft event
# which((substr(events.range$Event.ID, nchar(events.range$Event.ID)-1,nchar(events.range$Event.ID))=="02"))

# === FACTOR COMBINATIONS ===
# Three factor variables, Flight.Purpose, Flight.Phase, and Event.Special,
#       have a limited number of categories, a combination of two or three
#       of these factors will serve to put the database of records into
#       general categories that when put into table form could
#       help identify accident and incident trends.

# The following factor combination summaries are limited to
#       the study period, which is in data frame events.range
# First, must identify non-NA factor values
flight.phase.ndx = !is.na(events.range$Flight.Phase)
flight.purpose.ndx = !is.na(events.range$Flight.Purpose)                                 
event.special.ndx = !is.na(events.range$Event.Special)                                 

# COMBINATION #1
# First combination is Flight.Purpose and Flight.Phase
# The next variable is the index of those records with a non-NA input 
#       for both the Flight.Purpose and Flight.Phase variables.

purpose.phase.ndx = flight.purpose.ndx & flight.phase.ndx
aaa = c(as.character(events.range$Flight.Purpose[purpose.phase.ndx]))
bbb = c(as.character(events.range$Flight.Phase[purpose.phase.ndx]))

purpose.phase.vector = paste(aaa,bbb, sep="-")

cat("\n")
paste("COMBINATION #1: In the following ordered table of the combination of the Flight.Purpose and Flight.Phase factors,",
      length(purpose.phase.vector), "aircraft were involed in",
      length(table(purpose.phase.vector)), "unique combinations of Flight.Purpose and Flight.Phase categories")

sort(table(purpose.phase.vector), decreasing = TRUE)

# COMBINATION #2
# The second combination will be of Flight.Purpose and Event.Special
# The next variable is the index of those records with a non-NA entry
#       for both the Flight.Purpose and Event.Special variables.

purpose.event.ndx = flight.purpose.ndx & event.special.ndx
ccc = c(as.character(events.range$Flight.Purpose[purpose.event.ndx]))
ddd = c(as.character(events.range$Event.Special[purpose.event.ndx]))

purpose.event.vector = paste(ccc, ddd, sep="-")

cat("\n")
paste("COMBINATION #2: In the following ordered table of the combination of Flight.Purpose and Event.Special factors,",
      length(purpose.event.vector), "aircraft were involved in",
      length(table(purpose.event.vector)), "unique combinations of Flight.Purpose and Event.Special categories.")

sort(table(purpose.event.vector), decreasing = TRUE)

# COMBINATION #3
# The third combination will be of Flight.Phase and Event.Special
# The next variable is the index of those records with a non-NA entry
#       for both the Flight.Phase and Event.Special variables.

phase.event.ndx = flight.phase.ndx & event.special.ndx
eee = c(as.character(events.range$Flight.Phase[phase.event.ndx]))
fff = c(as.character(events.range$Event.Special[phase.event.ndx]))

phase.event.vector = paste(eee, fff, sep="-")

cat("\n")
paste("COMBINATION #3: In the following ordered table of the combination of Flight.Phase and Event.Special factors,",
      length(phase.event.vector), "aircraft were involved in",
      length(table(phase.event.vector)), "unique combinations of Flight.Phase and Event.Special categories.")

sort(table(phase.event.vector), decreasing = TRUE)


# COMBINATION #4
# Third combination is Flight.Purpose, Flight.Phase, and Event.Special
# The next variable is the index of those records with a non-NA input
#       for the Flight.Purpose, Flight.Phase, and Event.Special variables.

purpose.phase.event.ndx = flight.purpose.ndx & flight.phase.ndx & event.special.ndx
ggg = c(as.character(events.range$Flight.Purpose[purpose.phase.event.ndx]))
hhh = c(as.character(events.range$Flight.Phase[purpose.phase.event.ndx]))
jjj = c(as.character(events.range$Event.Special[purpose.phase.event.ndx]))

purpose.phase.event.vector = paste(ggg,hhh,jjj, sep="-")

cat("\n")
paste("COMBINATION #4: In the following ordered table of the combination of the Flight.Purpose and Flight.Phase factors,",
      length(purpose.phase.event.vector), "aircraft were involed in",
      length(table(purpose.phase.event.vector)), "unique combinations of Flight.Purpose, Flight.Phase, and Event.Special categories")

sort(table(purpose.phase.event.vector), decreasing = TRUE)

# === AIRCRAFT CATEGORIES ===
# Each aircraft in this database has a number of characteristics, some of which
#       can occur only in specific combinations. For example, a particular aircraft model is associated with a 
#       specific manufacturer and aircraft type. The following table lists the population of each
#       combination of the three variables Aircraft.Manufacturer, Aircraft.Model, and Aircraft Type
#  
# First, must identify non-NA values for the manufacturer, model, and type
maker.ndx = !is.na(events.range$Aircraft.Manufacturer)
model.ndx = !is.na(events.range$Aircraft.Model)                                 
type.ndx = !is.na(events.range$Aircraft.Type)                                 

maker.model.type.ndx = maker.ndx & model.ndx & type.ndx
nnn = c(as.character(events.range$Aircraft.Manufacturer[maker.model.type.ndx]))
ppp = c(as.character(events.range$Aircraft.Model[maker.model.type.ndx]))
rrr = c(as.character(events.range$Aircraft.Type[maker.model.type.ndx]))

maker.model.type.vector = paste(nnn, ppp, rrr, sep="-")

cat("\n")
paste("MANUFACTUER, MODEL, AND TYPE COMPBINATIONS: In the following ordered table of the combination of the Aircraft.Manufacturer, Aircraft.Model, and Aircraft.Type,",
      length(maker.model.type.vector), "aircraft were divided among",
      length(table(maker.model.type.vector)), " groupings.")

sort(table(maker.model.type.vector), decreasing = TRUE)[1:10]

# === BAR PLOT OF HOURS ===
# Using the 24-hour clock, so hours range from 0 to 23
# For the vector of hours, convert the first two characters of the 
# Event.Time variable to numeric form
# Will only use unique events

hour.vector = as.numeric(substr(events.range$Event.Time[events.range.unique.ndx],1,2))

barplot(table(hour.vector),
        main = "Unique events by hour (2008-2018)",
        xlab = "Hour",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF DAYS ===
# Bar plot of events by day of the week and ensuring order of days
# Will only use unique events

day.vector = weekdays(events.range$Event.Date[events.range.unique.ndx], abbreviate = TRUE)
day.vector = factor(day.vector,levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered=TRUE)

barplot(table(day.vector),
        main = "Unique events by weekday (2008-2018)",
        xlab = "Day",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF Months ===
# Bar plot of events by month and ensuring the order of months
# Will only use unique events

month.vector = months(events$Event.Date[events.range.unique.ndx], abbreviate = TRUE)
month.vector = factor(month.vector,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered=TRUE)

barplot(table(month.vector),
        main = "Unique events by month (2008-2018)",
        xlab = "Month",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF Years ===
# Bar plot of events by year
# Will only use unique events

year.vector = as.numeric(format(events.range$Event.Date[events.range.unique.ndx],'%Y'))

barplot(table(year.vector),
        main = "Unique events by year (2008-2018)",
        xlab = "Year",
        ylab = "Unique events",
        col = "dodgerblue")

# Histogram of all events by year for years of interest
hist(as.integer(format(as.Date(events.range$Event.Date, format="%Y-%m-%d"),"%Y")), 
     main = "Aircraft Events by year: 2008-2018",
     xlab = "Year (2008-2018)",
     ylab = "Events",
     las = 2)
# ==========
#Save cleaned and processed data as CSV and Excel files

write.csv(events, file = "events_cleaned.csv")



# Processing end time
timeEnd = Sys.time()

# Processing date and total processing time
cat(paste("","Processing end date and time",date(),"","",sep="\n"))
paste("Total processing time =",round(difftime(timeEnd,timeStart), digits=2),"seconds",sep=" ")


# Stop writing to an output file
sink()

################
