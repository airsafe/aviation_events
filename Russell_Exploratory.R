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

# ENSURE Event.ID VARIABLE IS UNIQUE

# Variable designated identifier variable (Event.ID) variable
# is unique (number of unique values equal to the number of records)

# First step is to see if there is only one variable with unique values

unique.ndx = which(lapply(lapply(events,unique),length)==nrow(events))

if (length(unique.ndx)==0){
        print("No variable can serve as a unique identifier.")
}

if (length(unique.ndx)==1)paste("Variable '", colnames(events)[unique.ndx],
                         "' can serve as a unique identifier.", sep = "")

if (length(unique.ndx)>1){
        print("The following variables that can serve as unique identifiers:")
        colnames(events)[unique.ndx]
}
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

#Print out integer variables
paste("Integer variables")
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

# Print factor variables
paste("Factor variables")
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
paste("Character variables")
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


# ==Convert Date Variable==

# There is only one date variable, Event.Date, with raw data format DD-Month-YYYY.
#       Will change format to R standard, so will turn it to character and
#       then to standard date format YYY-MM-DD.

events$Event.Date = as.character(events$Event.Date)
events$Event.Date = as.Date(events$Event.Date, "%d-%B-%Y")

paste("Date variable is Event.Date")


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

paste("Aircraft event date range for full database:", 
      min(events$Event.Date),"to",
      max(events$Event.Date))

paste("Summary of complete database")

paste("Number of aircraft events in total database -",nrow(events))

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

paste("Study period: 2008-2018")

# Create an index of records of aircraft events that fall in the study period
events.range.ndx = events$Event.Date>=as.Date("2008-01-01") & events$Event.Date<=as.Date("2018-12-31")
events.range = events[events.range.ndx,]
events.range.unique.ndx = which(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1, nchar(events.range$Event.ID))=="01")

paste("Aircraft event date range for study period:", 
      min(events.range$Event.Date),"to",
      max(events.range$Event.Date))

paste("Number of aircraft events during the study period -",nrow(events.range))

paste("Number of unique events during the study period -",length(events.range.unique.ndx))


# Table of events by year (during study period)
paste("Table of events by year (during study period)")
table(as.integer(format(as.Date(events.range$Event.Date, format="%Y-%m-%d"),"%Y")))

# For events with and loss severity considered to be major
# (over $5M, ) there was a range range of percentage of arcraft value lost.

paste("Distribution of percent of aircraft damaged")
table(events.range$Damage.Severity[which(events.range$Damage.Severity>0)])

paste("Distribution of percent of aircraft damaged for major events (over $5M losses)")
table(events.range$Damage.Severity[which(events.range$Damage.Severity>0 & events.range$Loss.Severity=="M")])


# Summary of data from the factor and integer variables

# Variables that can be reasonably summarized include
# all factor and integer variables. Using the sorted 
# index values will result in summaries in the same
# order as in the spreadsheet that makes up the database
# The ordered index is sort(c(integer.vars.ndx,factor.vars.ndx))

paste("Summaries of data from factor and integer variables for study period")
apply(events.range[,sort(c(integer.vars.ndx,factor.vars.ndx))],2,table, useNA = "always")

# Can indirectly compute number of multiple events.
# Do a table of the last two Event.ID characters. 
paste("Table of the last two Event.ID characters (multiple event indicator)")
table(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1,nchar(events.range$Event.ID)))

# Note: The following gives the index of events that have an EventID
#       ending in '02', indicating a multiple aircraft event
# which((substr(events.range$Event.ID, nchar(events.range$Event.ID)-1,nchar(events.range$Event.ID))=="02"))


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

################
