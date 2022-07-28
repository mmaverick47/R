#Step 1: Load the data 

#Use airquality data set

#Call in the data set

air <- airquality

#Step 2: Clean the data

#There will be some NAs, need to figure out what to do with them

library("tidyr")
air <- replace_na(air,as.list(colMeans(air,na.rm=T)))

#Step 3: Understand the data distribution

#Create the following visualizations using ggplot:

#Call the library
library("ggplot2")

#• Histograms for each of the variables

ggplot(air, aes(x=Ozone)) + geom_histogram()
ggplot(air, aes(x=Solar.R)) + geom_histogram()
ggplot(air, aes(x=Wind)) + geom_histogram()
ggplot(air, aes(x=Temp)) + geom_histogram()

#• Boxplot for Ozone

ggplot(air) + geom_histogram(aes(x=Ozone)) + geom_line(aes(x=Ozone,y=Wind))

ggplot(air, aes(x=Ozone)) + geom_line(aes(y=Wind)) + geom_histogram()

ggplot(air, aes(y=Ozone)) + geom_boxplot()

#• Boxplot for wind values (round the wind to get a good number of "buckets")

ggplot(air, aes(y=Wind)) + geom_boxplot()

#Step 4: Look at all the data via a heatmap

#Create a heatmap, with each day along the x-axis and ozone, temp, wind, 
#and solar along the y-axis, and days as rows along the y-axis. Create the heatmap
#using geom_tile (this defines the ggplot geometry to be 'tiles' as opposed to 'lines' and 
#the other geometry we had previously used).


ggplot(data=air, aes(x=Ozone, y=Temp, color=Month)) + geom_line() + stat_smooth()

#Step 5: Look at all the data via a scatter chart

#Create a scatter chart (using ggplot geom_point), with the x-axis representing
#the wind, and y-axis representing the temperature, the size of each dot representing
#the ozone and the color representing the solar.

ggplot(air) + geom_point(aes(x=Wind, y=Temp, size=Ozone, color=Solar.R))

#Step 6: Final analysis

#• Do you see any patterns after exploring the data?

air$Date <- paste("1973", air$Month, air$Day, sep="-")
air$Date <- as.Date(air$Date, "%Y-%m-%d")
ggplot(data=air, aes(x=Date, y=Ozone)) + geom_line() + theme_classic(base_size=10)

ggplot(air, aes(x=Date)) + 
  geom_line(aes(y=Ozone, color="Ozone")) +
  geom_line(aes(y=Temp, color="Temp")) +
  geom_line(aes(y=Wind, color="Wind")) +
  geom_line(aes(y=Solar.R, color="Solar.R"))

#I don't see any obvious patterns or correlations.

#• What was the most useful visualization?

#I found the heat map to be useful, as well as the one with stats smoothing.