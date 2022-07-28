#Step 1: Load the data

#Read in the following JSON dataset
#http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

#Call the libraries
library("RCurl")
library("RJSONIO")
library("jsonlite")

link<-'https://opendata.maryland.gov/resource/pdvh-tf2u.json'
JSONData<-fromJSON(link)

hmData<-data.frame(JSONData,stringsAsFactors = FALSE)

str(JSONData)
View(hmData)

#Step 2: Clean the data

#After you load the data, remove the first 8 columns, and then, to make it 
#easier to work with, name the rest of the columns as follows:

#namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME",
#"ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT",
#"DIST_DIRECTION","CITY_NA ME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT",
#"PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")

#Columns already removed in this data set

rownames(hmData) <- NULL

#Columns already properly named

#Step 3: Understand the data using SQL (via SQLDF)

#Answer the following questions:
#• How many accidents happen on SUNDAY

hmData$day_of_week <- gsub("\\ ","",hmData$day_of_week)

sqldf("select count(day_of_week) from hmData where day_of_week = 'SUNDAY'")

#• How many accidents had injuries (might need to remove NAs from the data)

sqldf("select count(injury) from hmData where injury = 'YES'")

#• List the injuries by day

sqldf("select day_of_week, count(injury) from hmData where injury = 'YES' group by day_of_week")

#Step 4: Understand the data using tapply

#Answer the following questions (same as before) – compare results:
#• How many accidents happen on Sunday

tapply(hmData$day_of_week, hmData$day_of_week=="SUNDAY", length)

#• How many accidents had injuries (might need to remove NAs from the data)

tapply(hmData$injury, hmData$injury=="YES", length)

#• List the injuries by day

tapply(hmData$injury=="YES", hmData$day_of_week, length)

