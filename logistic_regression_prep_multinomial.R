# Processing start time
sink('regression_prep_output_multinomial.txt')
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


if("haven" %in% rownames(installed.packages()) == FALSE) 
{install.packages("haven")}
library(haven)
# ====  FUNCTIONS ====
basic_multinom = function(training_label_vec,model_vars_training, label_and_vars_test){
# General linear model with a label that is a factor with two or more levels.
# Function runs the model using the label and all remaining colums in the data frame.
# Inputs: 
#       training_label_vec: Labels from training set
#       model_vars_training: Features from training data
#       label_and_vars_test: The labels and features from the test data
# Outputs: No output, function directly calls a function to create the confustion matrix.
        

set.seed(5656)

multinom_mod = multinom(training_label_vec ~., data = model_vars_training)
        
print(summary(multinom_mod))
        
# Calculate p-values using Wald tests (here z-tests)
z <- summary(multinom_mod)$coefficients/summary(multinom_mod)$standard.errors
       predicted_scores <- predict (multinom_mod,  label_and_vars_test, "probs") # predict on new data
        
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)
        
print(head(predicted_scores))
        
predicted_class <- predict(multinom_mod, label_and_vars_test)

print(table(predicted_class))

        
# multi.logistic.eval is a function that takes the predicted labels
#       and actual labels and creates the confusion matrix
multi.logistic.eval(predicted_class, label_and_vars_test[,1])
 
# NOTE: Can combine  this function with multi.logistic.eval by setting:
#       predict.vec = predicted_class and 
#       label.vec = label_and_vars_test[,1]
}



multi.logistic.eval <- function(predict.vec,label.vec){
# This function below creates a confusion matrix and evaluates the model 
# For a multinomial logistic regression, takes the label and the prediction
#       vector and creates both the confusion matrix, overall accuracy, 
#       and both the precision and recall values for each factor level of the label
        
# First step is to print the confusion matrix
        cat(paste("\n\n"))
        cat(paste('Confusion matrix','\n'))
        print(table(predict.vec, label.vec, dnn = c("Predicted","Actual")))
        
        
        # Next step is to get precision and recall values for each factor level
        # First initialize precision and recall vectors
        label.vec.pop = NULL
        precision.vec = NULL
        recall.vec = NULL
        f1.vec = NULL
        for(i in 1:length(levels(label.vec))){
                TP = sum(predict.vec==levels(label.vec)[i] & label.vec==levels(label.vec)[i])
                FN = sum(predict.vec!=levels(label.vec)[i] & label.vec==levels(label.vec)[i])
                FP = sum(predict.vec==levels(label.vec)[i] & label.vec!=levels(label.vec)[i])
                
                # F1 calculation
                P = TP/(TP + FP)
                R = TP/(TP + FN)  
                F1 = 2*P*R/(P+R)
                
                label.vec.pop = c(label.vec.pop, (TP + FN))
                precision.vec = c(precision.vec, round(TP/(TP + FP), digits=3))
                recall.vec = c(recall.vec, round(TP/(TP + FN), digits=3))
                f1.vec = c(f1.vec, round(F1, digits=3))
                
                cat(paste('Number of occurrences for loss type ', levels(label.vec)[i], ' is ', (TP + FN),'\n'))   
                cat(paste('Precision for loss type ', levels(label.vec)[i], ' is ', round(TP/(TP + FP), digits=3),'\n'))   
                cat(paste('Recall for loss type ', levels(label.vec)[i], ' is ', round(TP/(TP + FN), digits=3),'\n'))  
                cat(paste('F1 for loss type ', levels(label.vec)[i], ' is ', round(F1, digits=3),'\n'))  
                cat('\n')
        }
        
        eval.df = as.data.frame(cbind(label.vec.pop,precision.vec, recall.vec, f1.vec))
        colnames(eval.df) = c("Occurrences", "Precision", "Recall", "F1")
        rownames(eval.df) = levels(label.vec)
        #eval.df = as.numeric(eval.df)
        cat('Precision, recall, and F1 averages for each class')
        cat('\n')
        print(eval.df)
        cat('\n')
        cat(paste('Accuracy =', round(mean(as.character(predict.vec) == as.character(label.vec)), digits=3), '\n'))    
        cat('\n')
        cat('Unweighted average precision, recall, and F1 values')
        cat('\n')
        cat(paste('Average precision is', round(mean(eval.df$Precision), digits = 3)))
        cat('\n')
        cat(paste('Average recall is', round(mean(eval.df$Recall), digits = 3)))
        cat('\n')
        cat(paste('Average F1 score is', round(mean(eval.df$F1), digits = 3)))
        cat('\n')
        cat('\n')
        
        
        cat('Precision, recall, and F1 values weighted by proportion of each class')
        cat('\n')
        cat(paste('Weighted average precision is', round(sum(eval.df$Precision*eval.df$Occurrences)/sum(eval.df$Occurrences), digits = 3)))
        cat('\n')
        cat(paste('Weighted average recall is', round(sum(eval.df$Recall*eval.df$Occurrences)/sum(eval.df$Occurrences), digits = 3)))
        cat('\n')
        cat(paste('Weighted average F1 score is', round(sum(eval.df$F1*eval.df$Occurrences)/sum(eval.df$Occurrences), digits = 3)))
        cat('\n')
        cat('\n')
        
        
}
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
        #       and two others comparing subpopulations,
        #       Loss.Severity is 'U', 'A', or 'M')
        # cat_cols are the names of the category columns for review ( selected factor features)
        options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
        temp_all = df
        temp0 = df[df$Loss.Severity == "U",]
        temp1 = df[df$Loss.Severity == "A",]
        temp2 = df[df$Loss.Severity == "M",]
        for(col in cat_cols){
                pdf(paste("factor_",col,"_by_loss.severity.pdf",sep = ""))   
                p_all = ggplot(temp_all, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of entire feature', col)) +  
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                p1 = ggplot(temp0, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of', col, 'for Undamaged')) +  
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                p2 = ggplot(temp1, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of', col, 'for Attrition Loss')) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                p3 = ggplot(temp1, aes_string(col)) + 
                        geom_bar(color = 'black', fill= 'dodgerblue') +
                        ggtitle(paste('Bar plot of', col, 'for Major Loss')) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                grid.arrange(p_all, p1,p2,p3, nrow = 4)
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
cat(paste("\n\n"))

ifelse(nrow(events)==nrow(events.raw), 
       paste("No incomplete records detected"), 
       paste("One or more records are incomplete"))


# MANAGING VARIABLE TYPES
# There are several types of data in the data frame, including
#       numeric, factor, character, and date. They will 
#       be transformed prior to any anaysis

# ==Manage the label variable==
# The label for this multinomial logistic regression is Loss.Severity, which is a factor.
# Ensure ordering of Loss.Severity label is in following sequence:
#       Undamaged (U), Attrition (A), Major (M)
# First, include only those  records with one of these values
events = events[events$Loss.Severity %in% c("U", "A", "M"),]

# Order the categories in Loss.Severity variable by damage level
events$Loss.Severity = ordered(events$Loss.Severity, levels = c("U", "A", "M"))


# ==Manage time variable==

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

# Will turn this from a logical vector to one with 1's and 0's
events$Multi.Aircraft = as.integer(as.logical(events$Multi.Aircraft))

cat(paste("\n\n"))
paste("There were", length(matching.first.eight.event.id), "events involving one or more aircraft.")

cat(paste("\n\n"))
paste("There were", length(multi.event.ndx), "aircraft involved in multi-aircraft events.")


# ==Exclude non-covered models== 

# Only certain aircraft models, specifically those most likely to be insured
#       by the major insurance companies or syndicates in major insurenace markets
#       such as those in the US and UK, will be used in this analysis because
#       other models are not likely to have either significant value in the 
#       commercial aircraft market or are unlikely to be involved in an insurance 
#       industry payout that would qualify as an Attrition loss (definition in supporting documentation).

#       Note that non-covered models would be in raw database only if there was
#       a multi-aircraft event involving a covered aircraft.
#       Also note that the binary variable "Multi.Aircraft" flags all aircraft involved
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

# Second, summarize the number of records with included models

cat(paste("\n\n"))
paste(sum(included.models.ndx), "of the", length(included.models.ndx), "events involve models of interest and will be evaluated further." )

events = events[included.models.ndx,]

#==Manage factor features==
factor.vars = c("Event.Type", "Hour.Category", "Operator.Category", "Aircraft.Type", "Flight.Phase", 
                "Flight.Purpose","Event.Country", "Event.IATA", "Origin.IATA", "Destination.IATA", 
                "International.Flight", "Passenger.Flight",          
                "Scheduled.Flight", "Event.Special", "Multi.Aircraft", 
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
#       were coded as a factor that may figure into the prediction algorithm.
#       It also has a factor value (9) which may be mistaken for an NA value.
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

# Somewhat paradoxically, disregard.ndx are the records that have no NAs for critical features
cat(paste("\n\n"))
paste("There were", sum(disregard.ndx), "records with NA value for critical variables (features).")
cat(paste("\n\n"))

# Eliminate the records with missing selected data
events = events[!disregard.ndx,]

paste("There are", format(nrow(events), digits=5, big.mark = ","), "records with no NA values for critical variables (features).")
cat(paste("\n\n"))

#==Create numeric features ===
# Create features Aircraft.Age, Industry.Maturity, Years.Since.Cert
#       All of these will have an A.D. type dating system where 
#       the earliest year is the year one.
#       A normalized, or scaled and normalized version of these features will be used,
#       via the formulat scale(log(<feature>))

events$Aircraft.Age = ((events$Event.Year - events$Production.Year) + 1)

# Looking to see how many events happened in first two years
paste("A total of", sum(events$Aircraft.Age <= 2),"events occurred same year as  delivery year or the following year.")

# Industry.Maturity will define year one as the first year
#       of either certification or manufacture for the aircraft being analyzed
#       It acts as a relative meaurement of the event year relative to the first year of the industry
events$Industry.Maturity = (events$Event.Year - min(c(events$Aircraft.Cert.Year,events$Production.Year))) + 1

# Years.Since.Cert (Current year -year of certification) + 1
#       Incremented by one to have an A.D. type dating system
events$Years.Since.Cert = (as.integer(format(Sys.Date(), "%Y")) - events$Aircraft.Cert.Year) + 1

# Create feature Major.Casualties
# This is the total number of major and fatal injuries
# The "sum" function (done elementwise using 'apply') is used with na.rm =  TRUE so that if the number of either 
#       Total.Deaths or Total.Serious.Injuries is NA, the non-NA value is used.
#       Records were both Total.Deaths Total.Serious.Injuries were NA were previously eliminated.
events$Major.Casualties = apply(cbind(events$Total.Deaths,events$Total.Serious.Injuries),1,sum,na.rm = TRUE)

# Add newly created numeric features, and any transformed versions, to numeric variables list
numeric.vars=c(numeric.vars,"Aircraft.Age","Industry.Maturity", "Years.Since.Cert","Major.Casualties")
numeric.vars.ndx = which(colnames(events) %in% numeric.vars)

# LABEL CREATION

# The feature Loss.Severity, which has three levels, Undamaged (U),
#       Attrition loss (A) and Major loss (M).

model.label = "Loss.Severity"


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

# Call function that turns NA into a factor level
na_to_factor_level(events,na.as.factor)  

numeric.vars.ndx = which(colnames(events) %in% numeric.vars)
factor.vars.ndx = which(colnames(events) %in% factor.vars)

# Check that all variables properly processed
#       The label Loss.Severity and two features, Event.Time and Event.Date, are
#       not in an index vector for character, numeric, or factor variables.
#       The total length of these vectors (plus the two variables not in
#       one of these indices) should equal the number of data frame columns.

if (sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))) == 0) {
        cat(paste("\n\n"))
        paste("All data frame columns completed initial processing") 
} else { cat(paste("\n\n"))
        paste(sum(!(colnames(events) %in% c(numeric.vars,factor.vars, char.vars))),"variables were not processed")
}

# PRE-SPLITTING ACTIONS
cat(paste("\n\n"))
paste("Prior to splitting the data into a training and test set, let's see where the true NA values are")
sapply(events,function(x) sum(is.na(x)))

cat(paste("\n\n"))
paste("Prior to splitting the data into a training and test set, let's see which factors have missing levels")
sapply(events,function(x) setdiff(levels(x),x))

cat(paste("\n\n"))
paste("Will now remove unused levels")
events = droplevels(events)

cat(paste("\n\n"))
paste("Checking after removal of unused levels to see if any missed")
sapply(events,function(x) setdiff(levels(x),x))

#===REMOVING LOW VARIANCE FEATURES
# The dummyVars function from the Caret package creates 
#       coding to transform categorical variables to binary
#       or dummy variables. 

# Only some of the categorical variables are of interest, 
#       and others will not be considered

# Will use fullRank = FALSE and will remove linear dependencies later if needed

# Note the label, Loss.Severity doesn't need dummy variables
#       Those that do are categorigal variables that are suspected to have predictive value.
#       The dummyVars function helps to identify those dummy variables
#       that are too rare or that have too little variance to be useful.
# Note: Took out "Aircraft.Age.factor" because used transformed and scaled
#       "Aircraft.Age.Scaled" instead
factors.of.interest = c("Aircraft.Type", "Damage.Severity.factor", "Event.Type",
                        "Flight.Phase", "Flight.Purpose", "Hour.Category", "International.Flight","Investigation.Status",
                        "Major.Casualties.factor", "Operator.Category", "Scheduled.Flight")


# One feature of interest, "Multi.Aircraft" is not in this list because it is already a binary variable. 
#       All of the others have three or more levels, and will be made into dummy variables

# Full rank option was false (creating linear dependent dummy variables) due to the expectation 
#       that one or more of the dummy variables for each feature would be of low or no variance.
#       Therefore, if at least one is of low or no rank, no linear dependencies
#       If that is not the case, at least one dummy variable will be removed
#       from each categorical feature to prevent linear dependencies.

#       If all make the cut, the reference level will be removed manually

dummies = dummyVars("~ ." , data = events[,factors.of.interest], fullRank = F)
loss_severity_dummies = data.frame(predict(dummies, newdata = events[,factors.of.interest]))
head(loss_severity_dummies)
names(loss_severity_dummies)
dim(loss_severity_dummies)

cat(paste("\n\n"))
paste("The",length(factors.of.interest), "categorical features of interest put through the dummyVars process resulted in", dim(loss_severity_dummies)[2], "dummy variables to evaluate.")

# The code in the cell below applies the nearZeroVar function and then filters for zero variance or near-zero variance features. 
# Looking to ignor dummy variables that have the folowing undesirable characteristics:
#       - Frequency ratio (freqCut) of over 95/5 from most likely to second most likely category
#       - Number of unique values divided by number of samples (uniqueCut) is below 10
# The main effect is to ignore rare codings
near_zero = nearZeroVar(loss_severity_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
low_variance_cols = near_zero[(near_zero$zeroVar == TRUE) | (near_zero$nzv == TRUE), ]
low_variance_cols

cat(paste("\n\n"))
paste("A total of",dim(low_variance_cols)[1], "of the", dim(loss_severity_dummies)[2], "dummy variables had low or no variance.")

cat(paste("\n\n"))
paste("The following",nrow(low_variance_cols),"low or no variance dummy variables should be excluded:")
rownames(low_variance_cols)
# Index of near_zero columns to ignore
dummies_excluded_ndx = which((rownames(near_zero) %in%  rownames(low_variance_cols) ))

cat(paste("\n\n"))
paste("The following",length(rownames(near_zero)) - length(dummies_excluded_ndx),"dummy variables should be included for further evaluation:")
dummy_vars_included = rownames(near_zero)[-dummies_excluded_ndx]
dummy_vars_included
cat(paste("\n\n"))
paste("A manual review of the non-low and non-low variance dummy variables from using the fullRank = FALSE option revealed that the following features had all dummy variables pass muster:")

cat(paste("\n\n"))
paste("International.Flight, Investigation.Status, Scheduled.Flight")

cat(paste("\n\n"))
paste("In each case, the reference category dummy variable (most likely category) was removed.")

dummy_vars_included = dummy_vars_included[!(dummy_vars_included %in% c("International.Flight.N", "Scheduled.Flight.Y","Investigation.Status.9"))]

# Will combine the surviving dummy variables 
#       from the data frame loss.severity.dummies
#       to the events database


# combining with events database
events = cbind(events,loss_severity_dummies[,dummy_vars_included])

# Columns to analyze, numerical plus the dummy variables
# Appropriately scaled versionsl of the following will be added to regress.vars after data is split:
#       "Aircraft.Age", "Industry.Maturity", "Years.Since.Cert.Scaled"
regress.vars = c("Multi.Aircraft", dummy_vars_included )



#===SPLITTING THE DATASET===
# Before any scaling of the features, the database will be partitioned based on the 
# prevelance of the label.  
table(events$Loss.Severity, useNA = "always")

set.seed(1955)
## Randomly sample cases to create independent training and test data, with 70% of the data
##      dedicated to training and 30% to testing
partition = createDataPartition(events[,'Loss.Severity'], times = 1, p = 0.7, list = FALSE)

# Training set
training = events[partition,] # Create the training sample
dim(training)

# Test set
test = events[-partition,] # Create the test sample
dim(test)

# Scaling selected features in the training and test sets
# Note: Aircraft.Age normalized only because scaled and normalized looked less normally distributed

training$Aircraft.Age.Scaled = scale(training$Aircraft.Age)
training$Industry.Maturity.Scaled = scale(log(training$Industry.Maturity))
training$Years.Since.Cert.Scaled = scale(log(training$Years.Since.Cert))

test$Aircraft.Age.Scaled = scale(test$Aircraft.Age)
test$Industry.Maturity.Scaled = scale(log(test$Industry.Maturity))
test$Years.Since.Cert.Scaled = scale(log(test$Years.Since.Cert))

regress.vars = sort(c("Aircraft.Age.Scaled", "Years.Since.Cert.Scaled", "Industry.Maturity.Scaled", regress.vars))

cat(paste("\n\n"))
paste("A total of ",format(nrow(events), digits=5, big.mark = ","), "records were used in this analysis.")

cat(paste("\n\n"))
paste("Training set has",format(nrow(training), digits=5, big.mark = ","), "records")

cat(paste("\n\n"))
paste("Test set has",format(nrow(test), digits=5, big.mark = ","), "records")


# Print out all feature names
cat(paste("\n\n"))
paste("Features names are:")
sort(regress.vars)

cat(paste("\n\n"))
paste("Feature summaries of the training set are:")
str(training[,regress.vars])
# 
# 
cat(paste("\n\n"))
paste("Numeric features in model:")
sort(c("Aircraft.Age.Scaled", "Industry.Maturity.Scaled","Years.Since.Cert.Scaled"))

cat(paste("\n\n"))
paste("Factor features in model:")
sort(c("Multi.Aircraft", dummy_vars_included ))

# ====Post splitting actions====

cat(paste("\n\n"))
paste("=== Summary of the training database ===")
str(training)


cat(paste("\n\n"))
paste("Aircraft event date range for full database:", 
      min(training$Event.Date),"to",
      max(training$Event.Date))


cat(paste("\n\n"))
paste("Number of aircraft events in the training database -",nrow(training))

cat(paste("\n\n"))
paste("Number of unique events in training database -",nrow(training) - length(which(duplicated(substr(training$Event.ID,1,8)))) )


# ===Data summaries===
cat(paste("\n\n"))
paste("===Summaries of all data from factor and numeric features===")
apply(training[,regress.vars],2,table, useNA = "always")

# HISTOGRAMS AND BOX PLOTS FOR TRAINING SET FEATURES
# Boxplots for numeric variable by label (Loss.Severity) value
# Boxplots only sensible for numeric labels
regress.vars.numeric = c("Aircraft.Age.Scaled","Industry.Maturity.Scaled", "Years.Since.Cert.Scaled")
basic_boxplot(training,regress.vars.numeric,model.label)  

# Histograms for selected numeric variables
basic_hist(training,regress.vars.numeric)


# FACTOR FEATURES

# RANKING ALL FACTOR FEATURES
# For a consistent look, barplots for factor variables will 
#       be decreasing in size from left to right. To do that,
#       they will be ranked by popularity

# for (i in 1:length(factor.vars.ndx)){
#         training[,factor.vars.ndx[i]] = factor(training[,factor.vars.ndx[i]], levels = names(sort(table(training[,factor.vars.ndx[i]]), decreasing=TRUE)))
# }

# Now that they are ranked, the following calls
#       to a function will display selected
#       factors in various configurations: 



#  BOXPLOTS AND PLAIN TEXT TALBES OF SELECTED FACTOR FEATURES

# Text tables of factor variables
cat(paste("\n\n"))
paste("===Text tables of factor variables===")
apply(training[,factor.vars.ndx],2,table, useNA="always")

# # Spread of production year by aircraft manufacuer
# pdf(paste("boxplot_production_year_by_manufacturer.pdf"))   
# p = ggplot(training, aes_string("Aircraft.Manufacturer", "Production.Year")) +
#         geom_boxplot(color = 'black', fill= 'dodgerblue') +
#         ggtitle(paste('Box plot of Production.Year vs. Aircraft.Manufacturer'))+
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# print(p)
# dev.off()

#====RUN LOGISTIC MODEL===
# This inital run will include all features assumed to be useful for prediction (see cttest)

# cttest= c(factor.var.dummies, "Aircraft.Age.factor", "Aircraft.Type", "Damage.Severity.factor",
#             "Industry.Maturity", 
#           "International.Flight", "Investigation.Status", "Major.Casualties.factor",
#           "Multi.Aircraft",  "Scheduled.Flight", "Years.Since.Cert") 

# cttest = c("Multi.Aircraft", "Aircraft.Type.A", "Aircraft.Type.B",        
#            "Aircraft.Type.C", "Aircraft.Type.D", "Damage.Severity.factor.L",
#            "Damage.Severity.factor.Q", "Damage.Severity.factor.C", "Damage.Severity.factor.4",
#            "Damage.Severity.factor.5", "Event.Type.AF", "Event.Type.AN", "Flight.Phase.CL",       
#            "Flight.Phase.LG", "Flight.Phase.PP", "Flight.Phase.TG", "Flight.Phase.TT",         
#            "Flight.Purpose.AC", "Flight.Purpose.AP", "Flight.Purpose.NF", "Hour.Category.25",        
#            "International.Flight.X", "International.Flight.Y", "Investigation.Status.1",
#            "Investigation.Status.2", "Major.Casualties.factor.L", "Major.Casualties.factor.Q",
#            "Operator.Category.A", "Operator.Category.C", "Scheduled.Flight.N", "Scheduled.Flight.X")
# cat(paste("\n\n"))
# paste("Factor features for logistic regression")
# sort(cttest)

cat(paste("\n\n"))
paste("Numeric features for logistic regression")
sort(regress.vars.numeric)

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

# num_cols = c('Industry.Maturity', 'Years.Since.Cert')
# preProcValues <- preProcess(training[,num_cols], method = c("center", "scale"))
# 
# training[,num_cols] = predict(preProcValues, training[,num_cols])
# test[,num_cols] = predict(preProcValues, test[,num_cols])
# head(training[,num_cols])

# Traning input includes only regression features
model_vars_training = training[,regress.vars]
# Test input includes regression features and label
label_and_vars_test = test[,c(model.label,regress.vars)]

# set.seed(5656)
# multinom_mod = multinom(Loss.Severity ~ Multi.Aircraft + Aircraft.Age.Scaled + Industry.Maturity.Scaled +
#                                 Years.Since.Cert.Scaled + Aircraft.Type.A + Aircraft.Type.B +         
#                                 Aircraft.Type.C + Aircraft.Type.D + Damage.Severity.factor.L + 
#                                 Damage.Severity.factor.Q + Damage.Severity.factor.C + Damage.Severity.factor.4 + 
#                                 Damage.Severity.factor.5 + Event.Type.AF + Event.Type.AN + Flight.Phase.CL +        
#                                 Flight.Phase.LG + Flight.Phase.PP + Flight.Phase.TG + Flight.Phase.TT +          
#                                 Flight.Purpose.AC + Flight.Purpose.AP + Flight.Purpose.NF + Hour.Category.25 +         
#                                 International.Flight.X + International.Flight.Y + Investigation.Status.1 + 
#                                 Investigation.Status.2 + Major.Casualties.factor.L + Major.Casualties.factor.Q +
#                                 Operator.Category.A + Operator.Category.C + Scheduled.Flight.N + Scheduled.Flight.X, 
#                         data = training)
# 
# 
# summary(multinom_mod)
# xxy
# Will create testing and training data frames with only the model label and selected features for the model

# The following function takes as input the vector of training labels, the data frame of the training features,
#       and a data frame that combines the test labels and test features to create the confusion matrix and associated statistics.
basic_multinom(training[,model.label],model_vars_training,label_and_vars_test)

# multinom_mod = multinom(model_vars_training[,1] ~., data = model_vars_training)
# 
# summary(multinom_mod)

# Calculate p-values using Wald tests (here z-tests)
# z <- summary(multinom_mod)$coefficients/summary(multinom_mod)$standard.errors
# predicted_scores <- predict (multinom_mod, test, "probs") # predict on new data
# predicted_scores <- predict (multinom_mod, model_vars_test, "probs") # predict on new data

# 2-tailed z test
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p


# head(predicted_scores)

# predicted_class <- predict(multinom_mod, test)
# predicted_class <- predict(multinom_mod, model_vars_test)
# === Confusion Matrix and Misclassification Error ===
# table(predicted_class, test$Loss.Severity, dnn = c("Predicted","Actual"))
# 
# # Same as accuracy?
# mean(as.character(predicted_class) == as.character(test$Loss.Severity))

###########

# multi.logistic.eval is a function that 
# multi.logistic.eval(predicted_class, test$Loss.Severity)
# multi.logistic.eval(predicted_class, model_vars_test[,1])

# Processing end time
timeEnd = Sys.time()

# Processing date and total processing time
cat(paste("","Processing end date and time",date(),"","",sep="\n"))
paste("Total processing time =",round(difftime(timeEnd,timeStart), digits=2),"seconds",sep=" ")


# Stop writing to an output file
sink()

# Copyright (c) 2019 Todd Curtis, All rights reserved
################
