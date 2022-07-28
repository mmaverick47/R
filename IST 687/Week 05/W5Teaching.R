#-----------------Demonstration
#install libraries if not installed yet
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("sqldf")
install.packages("jsonlite")

#Call the libraries
library("RCurl")
library("RJSONIO")
library("jsonlite")
library("sqldf")
library("curl")

#tapply(Summary Variable, Group Variable (and a condition), Function)

mycars <- mtcars
#attach so you donn't have to call the df for each attribute
attach(mycars)

mean(mpg)

tapply(mpg, cyl , mean)

tapply(mpg, cyl, range)

tapply(cyl, cyl, length)

tapply(wt, cyl, mean)

tapply(wt, cyl==8, mean)

tapply(mpg, list(cyl == 8, am == 0), mean)
tapply(mpg, list(cyl, am, gear), mean)

tapply(mpg, list(cyl, mpg >25), mean)

tapply(wt, mpg>mean(mpg), length)

x <- tapply(mpg, mpg>mean(mpg), length)

x["TRUE"]

#sql functions:
#select
#from
#where
#and
#group by
#count

sqldf("SELECT mpg, am FROM mycars where am=0")

sqldf("SELECT avg(mpg), cyl FROM mycars where am=1 group by cyl")

sqldf("SELECT avg(MPG), cyl, gear FROM mycars where cyl > 4 group by cyl, gear")


#sqldf("select #### from #### where ####")

#play with sqldf library
TestQuery <- sqldf("select count(mpg), cyl from mycars group by cyl")


#HW5 ------------------------------------------------------------------

#Step 1
#install libraries if not installed yet
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("jsonlite")
#Call the libraries
library("RCurl")
library("RJSONIO")
library("jsonlite")

link<-'https://opendata.maryland.gov/resource/pdvh-tf2u.json'
JSONData<-fromJSON(link)

#MyData<-data.frame(JSONData,stringsAsFactors = FALSE)

#LOOK into the data
str(JSONData)
View(MyData)

#JSON JavaScript Object Notation
#Start Cleaning
#there are some none values that you need to replace with NA
#You need to replace the null values with NA
#get rid of the NAs or Manage them in a good way
#Remove the 1st 8 columns
#Name the columns
#when searching for the day of "Sunday", you need to manage the space that may exist, you may decide to remove it

View(MyData)

#Step 3 using sqldf

FirstQuery <- sqldf("select sum(x13) from MyData where Day = 'Sunday'")
SecondQuery <- sqldf("select Count(###) from ### where ###")
ThirdQuery <- sqldf("select Count(###), day from ### Group By day")

#Step 4 do the same using tapply
#use similar to what I used in  tapply(mpg, cyl==8, length)


