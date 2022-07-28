
#################################
#               Week 7 
#################################

install.packages("maps")
install.packages("openintro") # states data
install.packages("ggmap")
install.packages("readxl")
install.packages("gdata") # to reformat some data sets, such as using cbindX function, can also be used to read xls files
install.packages("zipcode")

library(gdata) 
library(ggplot2)
library(openintro) 
library(ggmap)
library(readxl)
library(sqldf)
library(zipcode)

#1. Read the data
setwd("G:\\My Drive\\Applied data science\\Jan 2021\\W7")
df <- read_xlsx("MedianZIP.xlsx")
head(df)
str(df)
#View(df)
#2. Clean up the dataframe
#2a. Remove unneeded information
df <- df[-1,]
#2b. Update column names (zip, median, mean, population)
colnames(df) <- c("zip","median","mean","population")
#remove commas and make numeric
str(df)

df$median <- as.numeric(df$median)
df$mean <- as.numeric(df$mean)
df$population <- as.numeric(df$population)

#3. Load the 'zipcode' package

data(zipcode)

df$zip <- clean.zipcodes(df$zip) #reformat the zip codes

#4. Merge the zip code information from two data frames
dfNew <- merge(df, zipcode, by ="zip")
head(dfNew)

#5. Remove Hawaii and Alaska
dfNew <- dfNew[dfNew$state != "HI", ]
dfNew <- dfNew[dfNew$state != "AK", ]
dfNew <- dfNew[dfNew$state != "DC", ]

# or the following
'%!in%' <- function(x,y)!('%in%'(x,y))
NoStates <- c("HI", "AK")
dfNew <- dfNew[dfNew$state %!in% NoStates, ]

#Create a simpler dataframe with median income and population
income <- tapply(dfNew$median, dfNew$state, median)
state <- rownames(income)
medianIncome <- data.frame(state, income)

pop <- tapply(dfNew$population, dfNew$state, sum)
state <- rownames(pop)
statePop <- data.frame(state,pop)

dfSimple <- merge(medianIncome, statePop, by="state")
head(dfSimple)

# previous steps can be done using sql instead and scaling the income at the state level
dfSimple<- sqldf("select state, avg(median) as income, sum(population) as pop from dfNew group by state")
#dfSimple<- sqldf("select state, (income/pop) as income, pop from dfSimple")

head(dfSimple)

#Add state abbreviations and state names (lower case)
#use match(dfSimple$state,state.abb)
dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]
#convert to lower case
dfSimple$stateName <- tolower(dfSimple$stateName)

#Show us map, representing color with average median income
us <- map_data("state")

MapIncome <- ggplot(dfSimple, aes(map_id=stateName))
MapIncome <- MapIncome + geom_map(map=us, aes(fill=dfSimple$income))
MapIncome <- MapIncome + expand_limits(x=us$long, y=us$lat)

MapIncome <- MapIncome + ggtitle("average median income by state") + theme(plot.title = element_text(hjust=0.5))
MapIncome <- MapIncome + guides(fill=guide_legend(title="Income")) + ditch_the_axes
MapIncome


#Function To remove axis formats from the heatmaps
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Draw each zip code on map where color of dot is based on median income
dfNew$stateName <- state.name[match(dfNew$state,state.abb)]
dfNew$stateName <- tolower(dfNew$stateName)
head(dfNew)

MapZip <- ggplot(dfNew, aes(map_id=stateName))
MapZip <- MapZip + geom_map(map=us, fill="black", color="white")
MapZip <- MapZip + expand_limits(x=us$long, y=us$lat)

MapZip <- MapZip + geom_point(data=dfNew, aes(x=dfNew$longitude, y=dfNew$latitude, color=dfNew$median))

MapZip <- MapZip 
MapZip <- MapZip + ggtitle("income per zip code") + theme(plot.title=element_text(hjust=0.5))
MapZip <- MapZip  + ditch_the_axes
MapZip
head(dfNew)

##############################

# Generate a map showing density of zip codes
MapDensity <- ggplot(dfNew, aes(map_id=stateName))
MapDensity <- MapDensity + geom_map(map=us, fill="black", color="white")

MapDensity <- MapDensity + expand_limits(x=us$long, y=us$lat)

MapDensity <- MapDensity + stat_density_2d(data=dfNew, aes(x=dfNew$longitude, y=dfNew$latitude))

MapDensity <- MapDensity + ggtitle("zip code density") + theme(plot.title=element_text(hjust=0.5))
MapDensity

#########################################
#Show income per zip code around Harvard University
#Define the area and create limits to be used in the heatmap

# if the following key doesn't work then you have to generate your own key, please refer to the steps
# in the following web site
# https://developers.google.com/+/web/api/rest/oauth

MyKey = "AIzaSyBeMMwbvSh-MSF1kUcmMaegpWiO0pIYRXk"
register_google(key = MyKey)

zoomGeo <- geocode("Syracuse University, NY", key=MyKey)

#lat 43.088947
#long  -76.154480

zoomAmount <- 5

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
centerx <- -71.154480
centery <-  42.374443

ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

#Show income per zip code around NYC
MapZipZoom <- MapZip + xlim(xlimit) + ylim(ylimit)

MapZipZoom <- MapZipZoom + geom_point(aes(x=centerx, y=centery), color="darkred", size=3)

MapZipZoom <- MapZipZoom + ggtitle("Income by Zip around Harvard University ") + theme(plot.title=element_text(hjust=0.5))
MapZipZoom

