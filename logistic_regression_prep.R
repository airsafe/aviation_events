# Processing start time
sink('regression_prep_output.txt')
timeStart = Sys.time()


# Start writing to an output file (before first "paste" command)
cat(paste("Processing start date and time", date(), "",sep="\n"))

# Event database regression preparation

# Purpose is to take preprocessed spreadsheet data and put it into a form 
# That can be then be used to create a logistic regression prediction model using R

# ====== OVERVIEW OF DATABASE ===========
# There are 51 variables (column names) for the raw data. 
# A data dictionary describing these variables is available 
# in the supplemental document "Proposed variables for database"

# The type of data in the variables include:
#       Numeric
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

if("ggplot2" %in% rownames(installed.packages()) == FALSE) 
{install.packages("ggplot2")}
library(ggplot2)

if("repr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("repr")}
library(repr)

if("dplyr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("dplyr")}
library(dplyr)

if("caret" %in% rownames(installed.packages()) == FALSE) 
{install.packages("caret")}
library(caret)

if("MASS" %in% rownames(installed.packages()) == FALSE) 
{install.packages("MASS")}
library(MASS)

if("MLmetrics" %in% rownames(installed.packages()) == FALSE) 
{install.packages("MLmetrics")}
library(MLmetrics)

# ====  FUNCTIONS ====

# Basic ggplot histogram for numeric features
basic_hist = function(df, numcols){
        options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
        for(col in numcols){
                if(is.numeric(df[,col])){
                        p = ggplot(df, aes_string(col)) +
                                geom_bar(color = 'black', fill= 'dodgerblue') +
                                ggtitle(paste('Bar plot of', col, 'feature')) +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5)) +
                                theme(axis.text.x = element_text(angle=65, vjust=0.6))
                        print(p)
                }
        }
}

# Basic ggplot boxplot for numeric features
basic_boxplot = function(df, numcols, col_x = 'Loss.Severity'){
        options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
        for(col in numcols){
                if(is.numeric(df[,col])){
                        p = ggplot(df, aes_string(col_x, col)) +
                                geom_boxplot(color = 'black', fill= 'dodgerblue') +
                                ggtitle(paste('Box plot of', col, 'vs.', col_x)) 
                       print(p)
                }
        }
}

# Normalizing numeric data

# Numeric features and their number of NA values

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

# Check to see if any record has missing values
ifelse(nrow(events)==nrow(events.raw), 
        paste("No missing values detected in any record"), 
        paste("Missing values detected in one or more records"))


# CONVERT VARIABLES TO THEIR APPROPRIATE TYPES
# There are several types of data in the data frame, including
#       numeric, factor, character, and date. They will 
#       be transformed prior to any anaysis


# ==Manage time Variable==

# The time variable is in the 24 hour clock format. CSV files save
#       it as an numeric, so it is treated as such when it comes to
#       identifying a missing time value, which is coded as 9999. 

# The variable 'Event.Time' is in the 24 hour (HHMM) time format,  
#       but the leading zeros may have been eliminated during
#       initial uploading. The goal is to put it into the 
#       form HH:MM. Must do a combination of adding leading zeros 
#       and inserting a colon to get to HH:MM format

events$Event.Time = as.character(events$Event.Time)
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


# Insert colon after second character if time is four characters long (1000 to 2359, as well as missing time value of 9999)
four.long.ndx = which(nchar(events$Event.Time)==4)

events$Event.Time[four.long.ndx] = paste0(substr(events$Event.Time[four.long.ndx], 1,2),":",substr(events$Event.Time[four.long.ndx], 3,4))

#==Create factor variable for time of day==
# Will also create a factor variable of time by hour of the day
# Convert hour vector so that time is put into 25 bins
#       with bin 1 (coded as "1) including events from 0000 to 0059 hours, etc. until bin 24 
#       Bin 25 will be coded as "25" and will include those events with an unknown time.
#       Then create an ordered factor variable from 1-24, plus 25

hour.category=as.integer(substr(events$Event.Time,1,2))
hour.category=hour.category+1
hour.category[which(hour.category==100)]=25
hour.category=as.factor(hour.category) 
events$Hour.Category = hour.category

# ==Convert Date Variable == XXXY move this

# There is only one date variable, Event.Date, with raw data format DD-Month-YYYY.
#       Will change format to R standard, so will turn it to character and
#       then to standard date format YYYY-MM-DD (%Y-%b-%DD).

events$Event.Date = as.character(events$Event.Date)
events$Event.Date = as.Date(events$Event.Date, "%d-%b-%Y")
# Create a feature with the year of the event
events$Event.Year = as.numeric(format(events$Event.Date,'%Y'))


#==Create factor variable for multi-aircraft events

# EventID values are unique, but multi-aircraft events share
#       the same first eight characters, so first find combinations
#       where the first Event.ID characters are duplicated
matching.first.eight.event.id = which(duplicated(substr(events$Event.ID,1,8)))

# Will now match those combinations of the first eight characters among all records
multi.event.ndx = which(substr(events$Event.ID,1,8) %in% substr(events$Event.ID[matching.first.eight.event.id],1,8))

# Will now add a binary categorical variable indicating if the aircraft was part of a multi-aircraft event
events$Multi.Aircraft = substr(events$Event.ID,1,8) %in% substr(events$Event.ID[matching.first.eight.event.id],1,8)

paste("There were", length(matching.first.eight.event.id), "events involving one or more aircraft.")
paste("There were", length(multi.event.ndx), "aircraft involved in multi-aircraft events.")
cat(paste("\n"))


# ==Exclude non-covered models== 

# Only certain aircraft models, specifically those most likely to be insured
#       by the major insurance companies or syndicates in major insurenace markets
#       such as those in the US and UK, will be used in this analysis because
#       other models are not likely to have either significant value in the 
#       commercial aircraft market or are unlikely to be involved in an insurance 
#       industry payout that would qualify as a Major loss (definition in supporting documentation).

#       Note that non-covered models would be in raw database only if there was
#       a multi-aircraft event involving a covered aircraft.

covered.models = c("A220", "A300", "A310", "A318", "A319", "A320", "A321", "A330", "A340", "A350", "A380",
                   "ATR 42", "ATR 72", "BAe 146", "ATP", "RJ85", "RJ100",
                   "707",  "720", "727", "737", "747", "757", "767", "777", "787",
                   "717", "DC-8", "DC-9", "DC-10", "MD-10", "MD-11", "MD-80", "MD-81", "MD-82",
                   "MD-83", "MD-88", "MD-90",
                   "Challenger", "CRJ", "Global 5000", "Global 6000", "Global Express", "Dash 8",
                   "EMB 120", "ERJ 135", "ERJ 140", "ERJ 145", "ERJ 170", "ERJ 175", "ERJ 190", "ERJ 195",
                   "D328", "D328Jet", "F28", "F50", "F70", "F100", "L-1011", "Saab 340", "Saab 2000",
                   "Superjet 100", "ARJ21")

# First, identify those records that involve included models
included.models.ndx = events$Aircraft.Model %in% covered.models

paste(sum(included.models.ndx), "of the", length(included.models.ndx), "events involve models of interest and will be evaluated further." )
cat(paste("\n"))

events = events[included.models.ndx,]


#==Manage factor variables==
factor.vars = c("Event.Type", "Hour.Category", "Multi.Aircraft", "Operator.Category", "Aircraft.Type", "Flight.Phase", 
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

# NOTE: Important to evaluate some categorical features including NA values
#       when developing prediction algorithm, so following index
#       excludes categorical features of possible interest.
#       It also excludes "Hour.Category" because its missing values
#       were coded as factor that may figure into the prediction algorithm.
#       It also has a factor value (9) which may be mistaking for an NA value.
#       Excluded features include:
#       Hour.Category
#       Investigation.Status

factor.vars.missing.modeled = c("Hour.Category","Investigation.Status", "Original.Operator", "Event.Special", "International.Flight")
factor.vars.not.modeled.ndx = factor.vars[!factor.vars %in% factor.vars.missing.modeled]

# Make missing or unknown values from categorical features
#       that will not be modeled into NA data points
for (i in 1:length(factor.vars.not.modeled.ndx)) {
        events[which(events[,factor.vars.not.modeled.ndx[i]] %in% factor.null.values),factor.vars.not.modeled.ndx[i]] = NA
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


# ==Manage numeric variables==
# Note that numeric variable Event.Year to be added later

numeric.vars = c("Damage.Severity", "Aircraft.Cert.Year", "Production.Year", "Total.Crew", 
                 "Crew.Deaths", "Crew.Serious.Injuries", "Total.Passengers", 
                 "Passenger.Deaths", "Passenger.Serious.Injuries", 
                 "Other.Deaths", "Other.Serious.Injuries", "Total.Deaths", 
                 "Total.Serious.Injuries", "Event.Year")

# Index of numeric variables
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)

# Convert these columns from factor to numeric

# Ensure numeric variables are coerced to be numeric
for (i in 1:length(numeric.vars.ndx)) {
        events[,numeric.vars.ndx[i]] = as.numeric(as.character(events[,numeric.vars.ndx[i]]))
}

# Identify missing or undefined data points for numeric variables
# These numeric values would have either 999 or 999 as the missing, unknown, or incompete numeric values
numeric.null.values = c(999,9999)

# Make missing or unknown numeric data into NA data points
for (i in 1:length(numeric.vars.ndx)) {
        events[which(events[,numeric.vars.ndx[i]] %in% numeric.null.values),numeric.vars.ndx[i]] = NA
}

# Print names of numeric variables
cat(paste("", "Numeric variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

colnames(events[numeric.vars.ndx])

# ==Manage character variables==

char.vars = c("Event.ID", "Tail.Number", "Event.Location",
              "Origin", "Destination", "Flight.End", "Aircraft.Manufacturer", "Aircraft.Submodel",     
              "Registration.Country", "Serial.Number","Engine.Manufacturer", "Engine.Model", "Engine.Submodel",           
              "Operator.Name", "Operator.Country", "Investigating.Authority", "Probable.Cause", "Notes")

# Index of character variables
char.vars.ndx = which(colnames(events) %in% char.vars)

# Print character variables

cat(paste("", "Character variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

colnames(events[char.vars.ndx])

# Convert Event.Time to character  for later processing
events[,"Event.Time"] = as.character(events[,"Event.Time"])

# Convert these columns to character

# Ensure character variables are coerced to be characters
for (i in 1:length(char.vars.ndx)) {
        events[,char.vars.ndx[i]] = as.character(events[,char.vars.ndx[i]])
}


# Identify missing or undefined data points for character variables
# These character values would have the following values as the missing, unknown, or incompete factor values
char.null.values = c("X", "XX", "XXX", "XXXX", "9999",
                     "x", "xx", "xxx", "xxxx")


# Make missing or unknown character data into NA data points
for (i in 1:length(char.vars.ndx)) {
        events[which(events[,char.vars.ndx[i]] %in% char.null.values),char.vars.ndx[i]] = NA
}



#==Eliminate  records with NA values for selected features ==
# Several features are combined to produce values relevant to a prediction algorithm
#       If a record has an NA value for one of more of the combined features, it should be 
#       disregarded in the development of the prediction algorithm.


# Identify and exclude records with missing values for:
#       Aircraft.Cert.Year
#       Production.Year
#       Event.Year
#       both Total.Deaths and Total.Serious.Injuries 
disregard.ndx = is.na(events$Aircraft.Cert.Year) 
disregard.ndx = disregard.ndx | is.na(events$Loss.Severity)
disregard.ndx = disregard.ndx | is.na(events$Production.Year) 
disregard.ndx = disregard.ndx | is.na(events$Event.Year) 
disregard.ndx = disregard.ndx | (is.na(events$Total.Deaths) & is.na(events$Total.Serious.Injuries))

# Eliminate the records with missing selected data
events = events[!disregard.ndx,]


#==Create numeric variables ===
# Create feature Aircraft.Age
#       This is an integer value of Event.Year - Production.Year
#       except when the difference is zero. In that case, change the value to 0.5
events$Aircraft.Age = events$Event.Year - events$Production.Year
paste(sum((events$Event.Year - events$Production.Year)==0),"events occurred same year as  delivery year, adjust age from zero to 0.5")
events$Aircraft.Age[(events$Event.Year - events$Production.Year)==0] = 0.5

# Add "Aircraft.Age" to numeric variables list
numeric.vars=c(numeric.vars,"Aircraft.Age")
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)
     
# Create feature Major.Casualties
# This is the total number of major and fatal injuries
# The "sum" function (done elementwise using 'apply') is used with na.rm =  TRUE so that if the number of either 
#       Total.Deaths or Total.Serious.Injuries is NA, the non-NA value is used.
#       Records were both Total.Deaths Total.Serious.Injuries were NA were previously eliminated.
events$Major.Casualties = apply(cbind(events$Total.Deaths,events$Total.Serious.Injuries),1,sum,na.rm = TRUE)

# Add "Major.Casualties" to numeric variables list
numeric.vars=c(numeric.vars,"Major.Casualties")
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)

# Check that all variables properly processed
#       There were two variables, Event.Time and Event.Date, not in 
#       an index vector for character, numeric, and factor variables.
#       The total length of these vectors (plus the two variables not in
#       one of these indices) should equal the number of data frame columns.

ifelse(sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))) == 0,
        paste("All data frame columns completed initial processing"),
        paste(sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))),"variables were not processed")
      )


# Print out all column names, then by type
cat(paste("\n\n"))
paste("Column names are:")

colnames(events)

cat(paste("\n\n"))
paste("Time variable is ","Event.Time")

cat(paste("\n\n"))
paste("Numeric variables are")
colnames(events[,numeric.vars.ndx])

cat(paste("\n\n"))
paste("Factor variables are")
colnames(events[,factor.vars.ndx])

cat(paste("\n\n"))
paste("Character variables are")
colnames(events[,char.vars.ndx])

# ====Post initial cleaning actions====
# After cleaning the database, including setting missing
# or unknown variables to NA, will summarize both the entire database and the 
# portion of the database that is of greatest interest.
# For the initial study, that period was 2008-2018 inclusive.
# Hower, this logisitc regression prototype analysis will include every event.

# === Summary of complete database ===

cat("\n")
paste("=== Summary of the complete database ===")
str(events)
cat("\n")


paste("Aircraft event date range for full database:", 
      min(events$Event.Date),"to",
      max(events$Event.Date))
cat("\n")


paste("Date variable is Event.Date")
cat("\n")

paste("Number of aircraft events used for model -",nrow(events))
cat("\n")

paste("Number of unique events in total database -",length(which(substr(events$Event.ID, nchar(events$Event.ID)-1, nchar(events$Event.ID))=="01")))

# The factor variable "Multi.Aircraft" flags all aircraft involved
#       in a multiple aircraft event. If the other aircraft was
#       not a covered model, it was excluded from this prediction algorithm development process.


# ===Data summaries===
paste("Summaries of data from factor and numeric variables")
apply(events[,sort(c(numeric.vars.ndx,factor.vars.ndx))],2,table, useNA = "always")

paste("Plots of production year by manufacturer")
p1 = ggplot(events, aes(x = Aircraft.Manufacturer,y = Production.Year)) + geom_boxplot()

# === Summary of the study period ===
cat("\n")
paste("=== Summary of the study period ===")
cat("\n")

# Create an index of records of aircraft events that fall in the study period
# Original study period was from 2008-2018, will now include all
# events.range.ndx = events$Event.Date>=as.Date("2008-01-01") & events$Event.Date<=as.Date("2018-12-31")
events.range.ndx = events$Event.Date>=as.Date("1997-01-01") & events$Event.Date<=as.Date("2028-12-31")
events.range = events[events.range.ndx,]
events.range.unique.ndx = which(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1, nchar(events.range$Event.ID))=="01")

cat(paste("\n\n"))
paste("Data includes",nrow(events), "records from",min(events$Event.Date),"to",max(events$Event.Date))
cat(paste("\n\n"))

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

# NOTE: will have to recompute these
# cat("\n")
# paste("Number of aircraft events during the study period -",nrow(events.range))
# 
# cat("\n")
# paste("Number of unique events during the study period -",length(events.range.unique.ndx))


# # Number of events involving two or more aircraft
# num.dup.events.study.period = length(which(substr(events.range$Event.ID, nchar(events.range$Event.ID)-1, nchar(events.range$Event.ID))=="02"))
# 
# cat("\n")
# paste("Number of events involving two or more aircraft during the study period -",num.dup.events.study.period)

# Table of events by year (during study period)
cat("\n")
paste("Table of events by year (during study period)")
table(as.numeric(format(as.Date(events.range$Event.Date, format="%Y-%m-%d"),"%Y")))

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


# AIRCRAFT BY MANUFACTURER
# To sort this graph, first must tell the feature how to rank the levels
#	They will be ranked by popularity (ensuring that manufacturer names are factors)
events$Aircraft.Manufacturer = factor(events$Aircraft.Manufacturer, levels = names(sort(table(events$Aircraft.Manufacturer), decreasing=TRUE)))

ggplot(events, aes(Aircraft.Manufacturer)) +
        geom_bar(width = 0.5,  color = 'dodgtserblue', fill= 'dodgerblue') +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))

# AIRCRAFT PRODUCTION YEARS
ggplot(events, aes(Production.Year)) +
        +     geom_bar(color = 'black', fill= 'dodgerblue') +
        +     theme(axis.text.x = element_text(angle=65, vjust=0.6))

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



# Summary of data from the factor and numeric variables

# Variables that can be reasonably summarized include
# all factor and numeric variables. Using the sorted 
# index values will result in summaries in the same
# order as in the spreadsheet that makes up the database
# The ordered index is sort(c(numeric.vars.ndx,factor.vars.ndx))

cat("\n")
cat(paste("","Summaries of data from factor and numeric variables for study period","",sep="\n"))
apply(events.range[,sort(c(numeric.vars.ndx,factor.vars.ndx))],2,table, useNA = "always")

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


# Convert hour vector so that time is put into 25 bins
#       with bin 1 (coded as "1) including events from 0000 to 0059 hours 
#       and bin 25 (coded as "99") being all those events with an unknown time.
#       Then create an ordered factor variable from 1-24, plus 99

barplot(table(hour.vector),
        main = "Unique events by hour",
        xlab = "Hour",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF DAYS ===
# Bar plot of events by day of the week and ensuring order of days
# Will only use unique events

day.vector = weekdays(events.range$Event.Date[events.range.unique.ndx], abbreviate = TRUE)
day.vector = factor(day.vector,levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered=TRUE)

barplot(table(day.vector),
        main = "Unique events by weekday",
        xlab = "Day",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF Months ===
# Bar plot of events by month and ensuring the order of months
# Will only use unique events

month.vector = months(events$Event.Date[events.range.unique.ndx], abbreviate = TRUE)
month.vector = factor(month.vector,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered=TRUE)

barplot(table(month.vector),
        main = "Unique events by month",
        xlab = "Month",
        ylab = "Unique events",
        col = "dodgerblue")

# === BAR PLOT OF Years ===
# Bar plot of events by year
# Will only use unique events

year.vector = as.numeric(format(events.range$Event.Date[events.range.unique.ndx],'%Y'))

barplot(table(year.vector),
        main = "Unique events by year",
        xlab = "Year",
        ylab = "Unique events",
        col = "dodgerblue")

# Histogram of all events by year for years of interest
hist(as.integer(format(as.Date(events.range$Event.Date, format="%Y-%m-%d"),"%Y")), 
     main = "Aircraft Events by year",
     xlab = "Year",
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
