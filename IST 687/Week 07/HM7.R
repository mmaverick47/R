#Call the libraries 

install.packages("openintro")
install.packages("ggmap")
install.packages("readxl")
install.packages("gdata")
install.packages("~/Downloads/zipcode_1.0.tar", repos = NULL, type="source")


library(openintro)
library(ggmap)
library(readxl)
library(gdata)
library(zipcode)


#Step 1: Load the data

#1. Read the data - gdata package previously used (excel file)

hm7 <-  read_excel("Documents/01-Education/Syracuse/2021/Winter/IST 687/Week 7/MedianZIP_2_2_2_2_2_2_2_2_2_2_2_2_2_2_2_2.xlsx")

#2. Clean up the data frame
  #a. Remove any info at the front of the file that's not needed

hm7 <- hm7[-1,]
rownames(hm7) <- NULL

  #b. Update the column names (zip, median, mean, population)

colnames(hm7)
names(hm7)[names(hm7) == "Data from: http://www.psc.isr.umich.edu/dis/census/Features/tract2zip/"] <- "zip"
names(hm7)[names(hm7) == "...2"] <- "median"
names(hm7)[names(hm7) == "...3"] <- "mean"
names(hm7)[names(hm7) == "...4"] <- "population"

hm7$median <- as.numeric(hm7$median)
hm7$mean <- as.numeric(hm7$mean)
hm7$population <- as.numeric(hm7$population)

#3.  Load the 'zipcode' package

data(zipcode)

hm7$zip <- clean.zipcodes(hm7$zip)

#4. Merge the zip code information from the two data frames

hm7new <- merge(hm7, zipcode, by = "zip")
head(hm7new)

#5. Remove Hawaii and Alaska

hm7new <- hm7new[hm7new$state != "HI", ]
hm7new <- hm7new[hm7new$state != "AK", ]

#Step 2: Show the income & population per state

#1. Create a simpler data frame, w/ just avg median income and the pop per state

income <- tapply(hm7new$median, hm7new$state, median)
state <- rownames(income)
medianincome <- data.frame(state, income)

pop <- tapply(hm7new$population, hm7new$state, sum)
state <- rownames(pop)
statepop <- data.frame(state, pop)

hm7Simple <- merge(medianincome, statepop, by="state")
head(hm7Simple)

#2. Add the state abbrevs and the state names as new columns (all lower case)

hm7Simple$statename <- state.name[match(hm7Simple$state, state.abb)]
hm7Simple$statename <- tolower(hm7Simple$statename)

hm7Simple$state <- tolower(hm7Simple$state)

hm7Simple <- hm7Simple[hm7Simple$state != "dc", ]

#3. Show the US map, representing the color w/ avg median income of that state

us <- map_data("state")

mapIncome <- ggplot(hm7Simple, aes(map_id=statename))
mapIncome <- mapIncome + geom_map(map=us, aes(fill=hm7Simple$income))
mapIncome <- mapIncome + expand_limits(x=us$long, y=us$lat)

mapIncome <- mapIncome + ggtitle("average median income by state") + theme(plot.title = element_text(hjust = 0.5))
mapIncome <- mapIncome + guides(fill=guide_legend(title="Income")) + ditch_the_axes

mapIncome

  #function to remove axis formats from the heatmaps
ditch_the_axes <- theme(
  axis.text=element_blank(),
  axis.line=element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#4. Create a second map w/ color representing the pop per state

mapPop <- ggplot(hm7Simple, aes(map_id=statename))
mapPop <- mapPop + geom_map(map=us, aes(fill=hm7Simple$pop))
mapPop <- mapPop + expand_limits(x=us$long, y=us$lat)

mapPop <- mapPop + ggtitle("average population by state") + theme(plot.title = element_text(hjust = 0.5))
mapPop <- mapPop + guides(fill=guide_legend(title="Population")) + ditch_the_axes

mapPop

#Step 3: Show the income per zip code

#1. Draw each zip code on the map, where color of 'dot' is based on median income.
#Have background of map be black

  #Add full state name and make lowercase
  hm7new$statename <- state.name[match(hm7new$state, state.abb)]
  hm7new$statename <- tolower(hm7new$statename)

  hm7new$state <- tolower(hm7new$state)

#Create the map

mapZip <- ggplot(hm7new, aes(map_id=statename))
mapZip <- mapZip + geom_map(map=us, fill="black", color="white")
mapZip <- mapZip + expand_limits(x=us$long, y=us$lat)

mapZip <- mapZip +geom_point(data=hm7new, aes(x=hm7new$longitude, y=hm7new$latitude, color=hm7new$median))
mapZip <- mapZip + ggtitle("income per zip code") + theme(plot.title = element_text(hjust = 0.5))
mapZip <- mapZip + ditch_the_axes

mapZip

#Step 4: Show zip code density

#1. Generate a diff map, one where we can easily see where there are lots of
#zip codes, and where there are a few (using the 'stat_density2d' function)

mapDensity <- ggplot(hm7new, aes(map_id=statename))
mapDensity <- mapDensity + geom_map(map=us, fill="black", color="white")
mapDensity <- mapDensity + expand_limits(x=us$long, y=us$lat)
mapDensity <- mapDensity + stat_density_2d(data=hm7new, aes(hm7new$longitude, y=hm7new$latitude))
mapDensity <- mapDensity + ggtitle("zip code density") + theme(plot.title = element_text(hjust = 0.5))
mapDensity <- mapDensity + ditch_the_axes

mapDensity

#Step 5: Zoom in to region around NYC

#1. Repeat steps 3 & 4, but have the image/map be of the NE US (centered around NY)

#Create zoom parameters

zoomamount <- 5

centerx <- -74.0060
centery <- 40.7128

ylimit <- c(centery-zoomamount, centery+zoomamount)
xlimit <- c(centerx-zoomamount, centerx+zoomamount)

#Create income by zip zoomed map

mapZipZoom <- mapZip + xlim(xlimit) + ylim(ylimit)
mapZipZoom <- mapZipZoom + geom_point(aes(x=centerx, y=centery), color="darkred", size=3)
mapZipZoom <- mapZipZoom + ggtitle("Income by zip around NYC") + theme(plot.title = element_text(hjust = 0.5))
mapZipZoom

#Create zip density zoomed map

mapDensityZoom <- mapDensity + xlim(xlimit) + ylim(ylimit)
mapDensityZoom <- mapDensityZoom + geom_point(aes(x=centerx, y=centery), color="darkred", size=3)
mapDensityZoom <- mapDensityZoom + ggtitle("Zip code density around NYC") + theme(plot.title = element_text(hjust = 0.5))
mapDensityZoom


##### from Katie

#Creates Map 
zipcodeDensityMap <- ggplot(incomeBYzipcode, aes(map_id=stateName)) +
  geom_map(map=us, fill="white", color="black") +
  expand_limits(x=us$long, y=us$lat) +
  #stat(level) creates a scale based off density level created by stat_density_2d
  stat_density_2d(data = incomeBYzipcode, aes(x=longitude, y=latitude, fill = stat(level)), geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map() + ggtitle("Zipcode Density") + 
  theme(plot.title = element_text(hjust=0.5)) +
  #remove axes
  ditch_the_axes
zipcodeDensityMap
