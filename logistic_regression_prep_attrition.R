# Processing start time
sink('regression_prep_output_attrition.txt')
timeStart = Sys.time()


# Start writing to an output file (before first "paste" command)
cat(paste("Processing start date and time", date(), "",sep="\n"))

# Event database regression preparation

# Purpose is to take preprocessed spreadsheet data and put it into a form 
# That can be then be used to create a logistic regression prediction model using R
# The intent is to systematically predict whether an event will result in an attrion loss.

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


if("gridExtra" %in% rownames(installed.packages()) == FALSE) 
{install.packages("gridExtra")}
library(gridExtra)

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

if("gridExtra" %in% rownames(installed.packages()) == FALSE) 
{install.packages("gridExtra")}
library(gridExtra)

if("ROCR" %in% rownames(installed.packages()) == FALSE) 
{install.packages("ROCR")}
library(ROCR)

if("pROC" %in% rownames(installed.packages()) == FALSE) 
{install.packages("pROC")}
library(pROC)

if("nnet" %in% rownames(installed.packages()) == FALSE) 
{install.packages("nnet")}
library(nnet)

# ====  FUNCTIONS ====

basic_hist = function(df, numcols){
# Basic ggplot histogram for numeric features
        options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
        for(col in numcols){
                if(is.numeric(df[,col])){
                        pdf(paste("histogram_",col,".pdf",sep = ""))  
                        p = ggplot(df, aes_string(col)) +
                                geom_bar(color = 'black', fill= 'dodgerblue') +
                                ggtitle(paste('Bar plot of', col, 'feature')) +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5)) +
                                theme(axis.text.x = element_text(angle=90, vjust=1))
                        print(p)
                        dev.off()
                }
        }
}

basic_boxplot = function(df, numcols, col_x){
# Basic ggplot boxplot for numeric features comparing to the label Loss.Severity value
        
        options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
        for(col in numcols){
                if(is.numeric(df[,col])){
                        pdf(paste("basic_boxplot_",col,".pdf",sep = ""))   
                        p = ggplot(df, aes_string(col_x, col)) +
                                geom_boxplot(color = 'black', fill= 'dodgerblue') +
                                ggtitle(paste('Box plot of', col, 'vs.', col_x))+
                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
                       print(p)
                       dev.off()
                }
        }
}

plot_factor = function(df, cat_cols){
# Barplot of feature that is a factor
        options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
        for(col in cat_cols){
                if(is.factor(df[,col])){
                        pdf(paste("factor_",col,".pdf",sep = ""))   
                        p = ggplot(df, aes_string(df[,col])) +
                                geom_bar(width = 0.5,  color = 'black', fill= 'dodgerblue') +
                                theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
                                ggtitle(col) 
                        print(p)
                        dev.off()
                }
        }
}

plot_bars = function(df, cat_cols){
# Produces three barplots, one with the population of the entire feature, 
#       and two others comparing two subpopulation,
#       Attrition.Loss.Label is 'ATTRITION' or 'other')
# cat_cols are the names of the category columns for review ( selected factor features)
        options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
        temp_all = df
        temp0 = df[df$Attrition.Loss.Label == "ATTRITION",]
        temp1 = df[df$Attrition.Loss.Label == "other",]
        for(col in cat_cols){
                pdf(paste("factor_",col,"_by_attrition_loss.pdf",sep = ""))   
                p_all = ggplot(temp_all, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of entire feature', col)) +  
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                p1 = ggplot(temp0, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of', col, 'for Attrtion Loss')) +  
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                p2 = ggplot(temp1, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of', col, 'for non-Attritin Loss')) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                grid.arrange(p_all, p1,p2, nrow = 3)
                dev.off()
        }
}

make_factor = function(input.num.vec, num.limits, factor.levels){
        # This function takes a numeric vector, values that define value ranges
        #       to be associated with a new factor vector and returns a factor vector
        #       of the same length.
        
        # First step is to change numbers to specific number levels
        # Second step is to define that vector as factor and simultaneously
        #       relabel the factor levels
        num.limits = sort(num.limits) # Ensure number limits are in order
        
        if((length(num.limits)) != length(factor.levels)){paste("Incorrect number of factor levels given the number limits")}
        
        for(i in 1:length(factor.levels)){
                
                if(i==1){
                        input.num.vec[input.num.vec <= num.limits[1]] = -1      
                }
                else if (i!=1){
                        input.num.vec[(input.num.vec > num.limits[i-1]) & (input.num.vec <= num.limits[i])]  = -i     
                }
        }
        input.num.vec = -input.num.vec
        new.factor.vec = factor(input.num.vec, labels=factor.levels, ordered = TRUE)
}

na_to_factor_level = function(df, cat_cols){
        # Turns any NA value in a vector into a factor level
        for(col in cat_cols){
                df[,col] = addNA(df[,col])
        }
}
# ==========
# Normalizing numeric data

# Numeric features and their number of NA values

# ==== DATA INPUT AND CLEANING ====

# READ DATA INTO DATA FRAME
# Import raw data (data files online in local directory AirSafe Consulting/Russell reports/event_database.csv)
events = NULL
events.raw = NULL
# events.range = NULL # Not relevant because all records used in developing algorithm
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

# Check to see if any records were incomplete
ifelse(nrow(events)==nrow(events.raw), 
        paste("No incomplete records detected"), 
        paste("One or more records are incomplete"))


# MANAGING VARIABLE TYPES
# There are several types of data in the data frame, including
#       numeric, factor, character, and date. They will 
#       be transformed prior to any anaysis

# ==Manage the label variable==
# The label for this logistic regression will be based on the Loss.Severity feature, which is a factor.
# Ensure ordering of Loss.Severity label is in following sequence:
#       Undamaged (U), Attrition (A), Major (M)
# First, include only those  records with one of these values
events = events[events$Loss.Severity %in% c("U", "A", "M"),]
events$Loss.Severity = ordered(events$Loss.Severity, levels = c("U", "A", "M"))


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

# ==Convert Date Variable == 

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
#       industry payout that would qualify as an Attrition loss (definition in supporting documentation).

#       Note that non-covered models would be in raw database only if there was
#       a multi-aircraft event involving a covered aircraft.
#       Also note that the factor variable "Multi.Aircraft" flags all aircraft involved
#       in a multiple aircraft event. If the other aircraft was
#       not a covered model, it was excluded from this prediction algorithm development process.


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


#==Manage factor features==
factor.vars = c("Event.Type", "Hour.Category", "Multi.Aircraft", "Operator.Category", "Aircraft.Type", "Flight.Phase", 
                "Flight.Purpose","Event.Country", "Event.IATA", "Origin.IATA", "Destination.IATA", 
                "Loss.Severity", "International.Flight", "Passenger.Flight",          
                "Scheduled.Flight", "Event.Special",     
                "Aircraft.Model", "Original.Operator","Engine.Type",
                "Investigation.Status", "Aircraft.Manufacturer","Registration.Country", "Operator.Country", "Investigating.Authority")

# Index of factor features
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
#       For example, "Hour.Category" because its missing values
#       were coded as factor that may figure into the prediction algorithm.
#       It also has a factor value (9) which may be mistaking for an NA value.
#       Excluded features include:
#       Hour.Category
#       Investigation.Status

factor.vars.missing.modeled = c("Hour.Category","Investigation.Status", "Original.Operator", "Flight.Purpose", "International.Flight", "Event.Special",
                                        "Scheduled.Flight", "Passenger.Flight", "Aircraft.Manufacturer", "Flight.Phase", "Operator.Category", "Flight.Phase",
                                        "Registration.Country", "Operator.Country", "Investigating.Authority")

factor.vars.not.modeled.ndx = factor.vars[!factor.vars %in% factor.vars.missing.modeled]

# Make missing or unknown values from these selected  
#       features have the value NA
for (i in 1:length(factor.vars.not.modeled.ndx)) {
        events[which(events[,factor.vars.not.modeled.ndx[i]] %in% factor.null.values),factor.vars.not.modeled.ndx[i]] = NA
}

# Print names of factor features 

cat(paste("","Initial factor features","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

sort(colnames(events[factor.vars.ndx]))


# ==Manage numeric variables==
# Note that numeric variable Event.Year to be added later

numeric.vars = c("Damage.Severity", "Aircraft.Cert.Year", "Production.Year", "Total.Crew", 
                 "Crew.Deaths", "Crew.Serious.Injuries", "Total.Passengers", 
                 "Passenger.Deaths", "Passenger.Serious.Injuries", 
                 "Other.Deaths", "Other.Serious.Injuries", "Total.Deaths", 
                 "Total.Serious.Injuries", "Event.Year")

# Index of numeric variables
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)

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
cat(paste("", "Initial numeric variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

sort(colnames(events[numeric.vars.ndx]))

# ==Manage character variables==

char.vars = c("Event.ID", "Tail.Number", "Event.Location",
              "Origin", "Destination", "Flight.End",  "Aircraft.Submodel",     
              "Serial.Number","Engine.Manufacturer", "Engine.Model", "Engine.Submodel",           
              "Operator.Name", "Probable.Cause", "Notes")

# Index of character variables
char.vars.ndx = which(colnames(events) %in% char.vars)

# Print character variables

cat(paste("", "Character variables","",sep="\n\n"))
# Used cat(paste(...)) to put new lines before and after heading)

sort(colnames(events[char.vars.ndx]))

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
        #       Loss.Severity
        #       Production.Year
        #       Event.Year
        #       Operator.Category
        #       both Total.Deaths and Total.Serious.Injuries 
disregard.ndx = is.na(events$Aircraft.Cert.Year) 
disregard.ndx = disregard.ndx | is.na(events$Loss.Severity)
disregard.ndx = disregard.ndx | is.na(events$Production.Year) 
disregard.ndx = disregard.ndx | is.na(events$Event.Year) 
disregard.ndx = disregard.ndx | is.na(events$Operator.Category) 
disregard.ndx = disregard.ndx | (is.na(events$Total.Deaths) & is.na(events$Total.Serious.Injuries))

# Eliminate the records with missing selected data
events = events[!disregard.ndx,]


#==Create numeric features ===
# Create features Aircraft.Age, Industry.Maturity, Years.Since.Cert
#       This is an integer value of Event.Year - Production.Year
#       except when the difference is zero. In that case, change the value to 0.5

# Aircraft.Age
events$Aircraft.Age = events$Event.Year - events$Production.Year
paste(sum((events$Event.Year - events$Production.Year)==0),"events occurred same year as  delivery year, adjust age from zero to 0.5")
events$Aircraft.Age[(events$Event.Year - events$Production.Year)==0] = 0.5

# Industry.Maturity (from 1960 to event year)
events$Industry.Maturity = events$Event.Year - 1960


# Years.Since.Cert (1960 to year of certification)
events$Years.Since.Cert = 2019 - events$Aircraft.Cert.Year
     
# Create feature Major.Casualties
# This is the total number of major and fatal injuries
# The "sum" function (done elementwise using 'apply') is used with na.rm =  TRUE so that if the number of either 
#       Total.Deaths or Total.Serious.Injuries is NA, the non-NA value is used.
#       Records were both Total.Deaths Total.Serious.Injuries were NA were previously eliminated.
events$Major.Casualties = apply(cbind(events$Total.Deaths,events$Total.Serious.Injuries),1,sum,na.rm = TRUE)

# Add Aircraft.Age, Industry.Maturity, Years.Since.Cert and Major.Casualties to numeric variables list
numeric.vars=c(numeric.vars,"Aircraft.Age", "Industry.Maturity", "Years.Since.Cert", "Major.Casualties")
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)


# LABEL CREATION
# After initial review, a number of numeric features were transformed for use in 
#       the prediction algorithm. 

# A binary label will be created for use in that algorithm.
#       That label will be based on the feature Loss.Severity,
#       Specifically those events coded with the value 'A' for attrition
#       loss will represent the positive cases, all others will represent a negative case.

model.label = "Attrition.Loss.Label"
events$Attrition.Loss.Label = ifelse(events$Loss.Severity == 'A', 'ATTRITION', 'other')
events$Attrition.Loss.Label = factor(events$Attrition.Loss.Label, levels = c('other', 'ATTRITION'), ordered = TRUE)
events$Attrition.Loss.Label[1:50]


# CREATING FACTORS FROM SELECTED NUMERIC VARIABLES
# Create a factor feature based on Major.Casualties
# This heavily skewed feature was used to create a factor 
#       feature with three levels: One for no major causualties, 
#       one for 1 to 9 major casualties, and one for 10 and above
#       major casualties.

num.limits= c(0,9,1000)
factor.levels = c("none","1to9","10plus")
events$Major.Casualties.factor = make_factor(events$Major.Casualties,num.limits,factor.levels)

# Add this variable to the list of factor variables
factor.vars = c(factor.vars, "Major.Casualties.factor")


# Create a factor feature based on Damage.Severity
# This heavily skewed feature was used to create a factor 
#       feature with five levels: 
#       - zero percent
#       - 1 to 10 percent
#       - 11 to 50 percent
#       - 51 to 99 percent
#       - 100 percent  for no major causualties, 
#       To consider the NA cases, first code them as 1000, and
#       give them the label "no_info"

severity.temp = events$Damage.Severity
severity.temp[which(is.na(severity.temp))] = 1000
num.limits= c(0,10,50,99,100,1000)
factor.levels = c( "no_info", "zero","1to10","11to50","51to99","total_loss")
events$Damage.Severity.factor = make_factor(severity.temp,num.limits,factor.levels)

# Add this variable to the list of factor variables
factor.vars = c(factor.vars, "Damage.Severity.factor")

# The numeric variable Aircraft.Age is also skewed. From a 
#       review of the distribution, a factor variable was created
#       with five age levels:
#       - 0 to 2
#       - 3 to 10
#       - 11-20
#       - 21 and over (21 to 100)
num.limits= c(2,10,20,100)
factor.levels = c("new_to2","3to10","11to20","21_plus")
events$Aircraft.Age.factor = make_factor(events$Aircraft.Age,num.limits,factor.levels)

# Add this variable to the list of factor variables
factor.vars = c(factor.vars, "Aircraft.Age.factor")


# ===Turn NA into factor level for selected factors===
# For some features, having no in information may be informative, so will make it a new level
#       Selected features with NA turned into a factor level include:
#       - Damage.Severity.factor
#       - Event.Special
#       - International.Flight
#       - Scheduled.Flight
#       - Passenger.Flight
#       - Flight.Phase
#       - Flight.Purpose
#       - Aircraft.Type
#       - Operator.Category

na.as.factor = c("Damage.Severity.factor", "Event.Special", 
                 "International.Flight", "Scheduled.Flight", 
                 "Passenger.Flight", "Flight.Phase", "Aircraft.Type", 
                 "Flight.Purpose", "Operator.Category")

na_to_factor_level(events,na.as.factor) # Function call 

numeric.vars.ndx = which(colnames(events) %in% numeric.vars)
factor.vars.ndx = which(colnames(events) %in% factor.vars)

# Check that all variables properly processed
#       There were the label Attrition.Loss.Label and two features, Event.Time and Event.Date,
#       not in an index vector for character, numeric, and factor variables.
#       The total length of these vectors (plus the two variables not in
#       one of these indices) should equal the number of data frame columns.

if (sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))) == 0) {
        paste("All data frame columns completed initial processing") 
} else { paste(sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))),"variables were not processed")
}

# PRE-SPLITTING ACTIONS
paste("Prior to splitting the data into a training and test set, let's see where the true NA values are")
sapply(events,function(x) sum(is.na(x)))

paste("Prior to splitting the data into a training and test set, let's see which factors have missing levels")
sapply(events,function(x) setdiff(levels(x),x))

paste("Will now remove unused levels")
events = droplevels(events)

paste("Checking after removal of unused levels to see if any missed")
sapply(events,function(x) setdiff(levels(x),x))

#===REMOVING LOW VARIANCE FEATURES
# The dummyVars function from the Caret package creates 
#       coding to transform categorical variables to binary
#       or dummy variables. 

# Only some of the categorical variables are of interest, 
#       and others will not be considered

factors.of.interest = c("Attrition.Loss.Label", "Aircraft.Age.factor",  "Aircraft.Type", "Damage.Severity.factor", "Event.Type",
                        "Flight.Phase", "Flight.Purpose", "Hour.Category", "International.Flight","Investigation.Status",
                        "Major.Casualties.factor", "Multi.Aircraft", "Operator.Category", "Scheduled.Flight")

dummies = dummyVars(Attrition.Loss.Label ~ . , data = events[,factors.of.interest])
attrition_loss_dummies = data.frame(predict(dummies, newdata = events[,factors.of.interest]))
head(attrition_loss_dummies)
names(attrition_loss_dummies)
dim(attrition_loss_dummies)

# The code in the cell below applies the nearZeroVar function and then filters for zero variance or near-zero variance features. 
near_zero = nearZeroVar(attrition_loss_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
low_variance_cols = near_zero[(near_zero$zeroVar == TRUE) | (near_zero$nzv == TRUE), ]
low_variance_cols

# Based on these results, low variance dummy variables were identified, and the 
#       following changes were made
#       - Hour.Category levels, only level 25 (unknown hour) was not low variance
#       - Operator.Category, only levels A (airline passenger) or C (airline cargo) are not low variance
#       - Flight.Purpose, only three levels: AC (airline cargo), AP (airline passenger), 
#               and NF (not flying) not low variance
#       - Flight.Phase, only three levels: LG (landing), PP (parked), TG (takeoff), 
#               and TT (taxi related to flight) not low variance
#       - Event.Type, only AF (fatal accident) and AN (nonfatal accident) were not low variance
#       Replacement binary dummy variables replaced these features

events$Operator.Category.A = factor(ifelse(events$Operator.Category == "A","T","F"), levels = c("T","F"), ordered = TRUE)
events$Operator.Category.C = factor(ifelse(events$Operator.Category == "C","T","F"), levels = c("T","F"), ordered = TRUE)
events$Hour.Category.25 = factor(ifelse(events$Hour.Category == "25","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Purpose.AC = factor(ifelse(events$Flight.Purpose == "AC","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Purpose.AP = factor(ifelse(events$Flight.Purpose == "AP","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Purpose.NF = factor(ifelse(events$Flight.Purpose == "NF","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Phase.LG = factor(ifelse(events$Flight.Phase == "LG","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Phase.PP = factor(ifelse(events$Flight.Phase == "PP","T","F"), levels = c("T","F"), ordered = TRUE)
events$Flight.Phase.TG = factor(ifelse(events$Flight.Phase == "TG","T","F"), levels = c("T","F"), ordered = TRUE)
events$Event.Type.AF = factor(ifelse(events$Event.Type == "AF","T","F"), levels = c("T","F"), ordered = TRUE)
events$Event.Type.AN = factor(ifelse(events$Event.Type == "AN","T","F"), levels = c("T","F"), ordered = TRUE)

factor.var.dummies = c("Operator.Category.A", "Operator.Category.C",
                      "Hour.Category.25", "Flight.Purpose.AC", "Flight.Purpose.AP", "Flight.Purpose.NF",
                      "Flight.Phase.LG", "Flight.Phase.PP", "Flight.Phase.TG",
                      "Event.Type.AF","Event.Type.AN" )
factor.vars = c(factor.vars, factor.var.dummies)
factor.vars.ndx = which(colnames(events) %in% factor.vars)



# Aternately, can alter the previously created data frame as follows:
# drops = rownames(low_variance_cols)
# attrition_loss_dummies = attrition_loss_dummies[ , !(names(attrition_loss_dummies) %in% drops)]
# names(attrition_loss_dummies)
# dim(attrition_loss_dummies)

#===SPLITTING THE DATASET===
# Before any scaling of the features, the database will be partitioned based on the 
# prevelance of the label.  
table(events$Attrition.Loss.Label, useNA = "always")

percent.positive = sum(events$Attrition.Loss.Label=='ATTRITION')/nrow(events)
paste("The data will be partitioned based on having ", round(100*percent.positive, digits=2),"% of events associated with an attrition loss (a positive label value)", sep="")

#===SPECIAL CASE REMOVALS===
# During model evaluation, the feature Event.Type had a single
#       occurrence of the factor level "NN", which caused an
#       error during the prediction phase. That event is removed below:

# Remove cases with rare Event.Type and Flight.Purpose levels was done more
#       effectively in the earlier section where dummy variables of low
#       variance were identified and removed
# events = events[-which(events$Event.Type %in% c("NN","CC","DF","DN","MF","NF")),]
# events = events[-which(events$Flight.Purpose %in% c("AT","CC","MN","UF")),]
# events = events[-which(events$Operator.Category %in% c("M","P","X")),]

set.seed(1955)
## Randomly sample cases to create independent training and test data, with 70% of the data
##      dedicated to training and 30% to testing
partition = createDataPartition(events[,'Attrition.Loss.Label'], times = 1, p = 0.7, list = FALSE)
training = events[partition,] # Create the training sample
dim(training)
test = events[-partition,] # Create the test sample
dim(test)


# Print out all column names, then by type
cat(paste("\n\n"))
paste("Column names are:")

sort(colnames(training))


cat(paste("\n\n"))
paste("Numeric labels are")
sort(colnames(training[,numeric.vars.ndx]))

cat(paste("\n\n"))
paste("Factor lables are")
sort(colnames(training[,factor.vars.ndx]))

cat(paste("\n\n"))
paste("Character labels are")
sort(colnames(training[,char.vars.ndx]))


cat(paste("\n\n"))
paste("Other labels are:")
sort(colnames(training[,-c(factor.vars.ndx, char.vars.ndx, numeric.vars.ndx)]))

# ====Post initial cleaning actions====
# After cleaning the database, including setting missing
# or unknown variables to NA, will summarize both the entire database and the 
# portion of the database that is of greatest interest.
# For the initial study, that period was 2008-2018 inclusive.
# Hower, this logisitc regression prototype analysis will include every event.

# === Summary of complete database ===

cat("\n")
paste("=== Summary of the training database ===")
str(training)
cat("\n")


paste("Aircraft event date range for full database:", 
      min(training$Event.Date),"to",
      max(training$Event.Date))
cat("\n")


paste("Date variable is Event.Date")
cat("\n")

paste("Number of aircraft events used for model -",nrow(training))
cat("\n")

paste("Number of unique events in training database -",nrow(training) - length(which(duplicated(substr(training$Event.ID,1,8)))) )
cat("\n")

# ===Data summaries===
paste("===Summaries of all data from factor and numeric features===")
apply(training[,c(numeric.vars.ndx,factor.vars.ndx)],2,table, useNA = "always")

# NUMERIC FEATURES HISTOGRAM AND PLAIN TEXT TABLES
# Boxplots for selected numeric variable by label (Attrition.Loss.Label) value
basic_boxplot(training,numeric.vars,model.label)  


# Histograms for selected numeric variables
basic_hist(training,numeric.vars)


# FACTOR FEATURES

# RANKING ALL FACTOR FEATURES
# For a consistent look, barplots for factor variables will 
#       be decreasing in size from left to right. To do that,
#       they will be ranked by popularity

for (i in 1:length(factor.vars.ndx)){
        training[,factor.vars.ndx[i]] = factor(training[,factor.vars.ndx[i]], levels = names(sort(table(training[,factor.vars.ndx[i]]), decreasing=TRUE)))
}

# Now that they are ranked, the following calls
#       to a function will display selected
#       factors in various configurations: 



#  BOXPLOTS AND PLAIN TEXT TALBES OF SELECTED FACTOR FEATURES
# Boxplots for factor variables
plot_factor(training, factor.vars[c(3,5,13:18,20,25:38)])   

# Boxplots for factor variable by Loss.Severity value compared to Loss.Severity subpopulations
plot_bars(training, factor.vars[c(3,5,13:18,20,25:38)])   

# Text tables of factor variables
paste("===Text tables of factor variables===")
apply(training[,factor.vars.ndx],2,table, useNA="always")

# Spread of production year by aircraft manufacuer
pdf(paste("boxplot_production_year_by_manufacturer.pdf"))   
p = ggplot(training, aes_string("Aircraft.Manufacturer", "Production.Year")) +
        geom_boxplot(color = 'black', fill= 'dodgerblue') +
        ggtitle(paste('Box plot of Production.Year vs. Aircraft.Manufacturer'))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
print(p)
#dev.off()

#====RUN LOGISTIC MODEL===
# This inital run will include all features assumed to be useful for prediction (see cttest)

cttest= c(factor.var.dummies, "Aircraft.Age.factor", "Aircraft.Type", "Damage.Severity.factor",
            "Industry.Maturity", 
          "International.Flight", "Investigation.Status", "Major.Casualties.factor",
          "Multi.Aircraft",  "Scheduled.Flight", "Years.Since.Cert") 

paste("Factor features for logistic regression")
sort(cttest)

# Check for NA values in training or test
# sapply(training,function(x) sum(is.na(x)))
# sapply(test,function(x) sum(is.na(x)))

# SCALE NUMERIC FEATURES
# Prior to running the model, numeric features will be scaled (normalized)
# Both numeric and factor features will be evaluated to see if one or 
#       more will be transformed prior to use in the prediction algorithm.
# Prior to this point in the processing, none of the transformations required actions 
#       that relied on summary information about the feature as a whole, such as the sum.
# The code in the cell below does the following:
#       - Calls a caret package preProcess object for centering and scaling the data. 
#               Notice that these computations are done only with the training data.
# The transformations are applied to both the training and test dataset.

num_cols = c('Industry.Maturity', 'Years.Since.Cert')
preProcValues <- preProcess(training[,num_cols], method = c("center", "scale"))

training[,num_cols] = predict(preProcValues, training[,num_cols])
test[,num_cols] = predict(preProcValues, test[,num_cols])
head(training[,num_cols])

set.seed(5566)
logistic_mod = glm(Attrition.Loss.Label ~ Aircraft.Type + Damage.Severity.factor + Event.Type.AF + 
                           Event.Type.AN + 
                           Flight.Purpose.AC + Flight.Purpose.AP + Flight.Purpose.NF +        
                           International.Flight + 
                           Scheduled.Flight, 
                   family = binomial, data = training)

summary(logistic_mod)

test$probs = predict(logistic_mod, newdata = test, type = 'response')
test[1:20, c('Attrition.Loss.Label','probs')]

# SCORE AND EVALUATE THE MODEL

score_model = function(df, threshold){
# Function to score the model
        df$score = ifelse(df$probs > threshold, 'ATTRITION', 'other')
        df
}

test = score_model(test, 0.5)
test[1:20, c('Attrition.Loss.Label','probs','score')]

# CREATE CONFUSION MATRIX
# The function below creates a confusion matrix to measure how well the prediction worked

logistic.eval <- function(df,label.name){ 
        # First step is to identify the factor levels of the label
        label.factors = sort(levels(df[,label.name]))
        # Next step is to find the TP, FP, TN, FN cases
        df$conf = ifelse(df[,label.name] == label.factors[1] & df$score == label.factors[1], 'TP',
                         ifelse(df$Attrition.Loss.Label == label.factors[1] & df$score == label.factors[2], 'FN',
                                ifelse(df$Attrition.Loss.Label == label.factors[2] & df$score == label.factors[2], 'TN', 'FP')))
        
        # Elements of the confusion matrix
        TP = length(df[df$conf == 'TP', 'conf'])
        FP = length(df[df$conf == 'FP', 'conf'])
        TN = length(df[df$conf == 'TN', 'conf'])
        FN = length(df[df$conf == 'FN', 'conf'])
        
        ## Confusion matrix as data frame
        out = data.frame(Negative = c(TN, FN), Positive = c(FP, TP))
        row.names(out) = c('Actual Negative', 'Actual Positive')
        print(out)  
        
        # Compute and print metrics
        P = TP/(TP + FP)
        R = TP/(TP + FN)  
        F1 = 2*P*R/(P+R)  
        cat('\n')
        cat(paste('accuracy  =', as.character(round((TP + TN)/(TP + TN + FP + FN), 3)), '\n'))      
        cat(paste('precision =', as.character(round(P, 3)), '\n'))     
        cat(paste('recall    =', as.character(round(R, 3)), '\n'))
        cat(paste('F1        =', as.character(round(F1,3)),'\n'))
        
        roc_obj <- roc(df$Attrition.Loss.Label, df$probs)
        cat(paste('AUC       =', as.character(round(auc(roc_obj),3)),'\n'))
}
logistic.eval(test,"Attrition.Loss.Label")      

ROC_AUC = function(df, label.name){
        
        options(repr.plot.width=5, repr.plot.height=5)
        pred_obj = prediction(df[,'probs'], df[,label.name])
        perf_obj <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
        AUC = performance(pred_obj,"auc")@y.values[[1]] # Access the AUC from the slot of the S4 object
        pdf(paste("AUC_plot_",label.name,".pdf",sep = ""))  
        p = plot(perf_obj)
        abline(a=0, b= 1, col = 'red')
        text(0.8, 0.2, paste('AUC = ', as.character(round(AUC, 3))))
        
        print(p)
        dev.off()
}

ROC_AUC(test,"Attrition.Loss.Label")


# Processing end time
timeEnd = Sys.time()

# Processing date and total processing time
cat(paste("","Processing end date and time",date(),"","",sep="\n"))
paste("Total processing time =",round(difftime(timeEnd,timeStart), digits=2),"seconds",sep=" ")


# Stop writing to an output file
sink()

# Copyright (c) 2019 Todd Curtis, All rights reserved
################
