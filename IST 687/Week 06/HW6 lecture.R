install.packages("ggplot2")
library(ggplot2)

air <- airquality

##Clean the data set from NAs
any(is.na(air)) # do any NA's exist in Ozone col? returns T or F

length(air[air=='NA'])
colnames(air)[colSums(is.na(air))>0]

# Replace NA's with mean of column
air$Ozone[is.na(air$Ozone)] <- mean(air$Ozone,na.rm=TRUE) # replace NA's in Ozone col with mean of col (where NA is discarded when calculating the mean)
air$Solar.R[is.na(air$Solar.R)] <- mean(air$Solar.R,na.rm=TRUE) # replace NA's in Solar.R col with mean of col

MyPlot <- ggplot(air) + aes(x=Month) + geom_point(aes(y=Temp)) + geom_histogram()
MyPlot

ggplot(air) + geom_point(aes(x=Ozone, y= Solar.R, colour=Month))

p <- ggplot(air) + geom_histogram(aes(x=Ozone)) + geom_line(aes(x=Ozone,y=Wind))
p


ggplot(air, aes(x=Ozone)) + geom_line(aes(y=Wind)) + geom_histogram()


p <- ggplot(data=air, aes(x=Ozone, y=Temp, color=Month)) + geom_line() + stat_smooth()
p + stat_smooth()

##Generate charts, histogram, box plot, line plot, point plot, and tile plot for heat map
MyGraph <- ggplot(air, aes(x=Ozone)) + geom_histogram(binwidth=20, color="white", fill="red")

MyGraph + ggtitle("MyTestingTitle")

MyGraph <- MyGraph + ggtitle("Test") + theme(plot.title = element_text(hjust = 0.5)) + xlab("OZ") + ylab("Frequency")
MyGraph

#The next is to create a boxplot
gbox.ozone <- ggplot(air, aes(y=Ozone)) + geom_boxplot() 
gbox.ozone
# create a boxplot for wind


#Explore how the data changes over time
# Paste value in Month and Day columns and 1973 and assign to column "Date"
air$Date <- paste("1973", air$Month, air$Day, sep="-")
air$Date <- as.Date(air$Date,"%Y-%m-%d")

str(air) # check structure to see if conversion works
air <- air[,-5:-6]
str(air) 

# create line charts for ozone, temp, wind and Solar.R
# line chart for ozone
gline.ozone <- ggplot(data = air, aes(x=Date, y=Ozone)) + geom_line() + theme_classic(base_size = 10)
gline.ozone
# create for the other variables
#
#
#
# create one chart with 4 lines

ggplot(air, aes(x=Date)) + 
  geom_line(aes(y=Ozone, color="Ozone")) + 
  geom_line(aes(y=Temp, color="Temp")) +  
  geom_line(aes(y=Wind, color="Wind")) +
  geom_line(aes(y=Solar.R, color="Solar.R")) +  theme(plot.title=element_text(hjust=.5)) + 
  labs(title="give it a name") + scale_color_manual(values=c("green4", "orange", "blue", "red"))  

#  ------------------------------------------ A different approach
# use melt function to transform the data frame
#to use melt install.packages("reshape2") 
library(reshape2)
airLong <- melt(air, id="Date")
head(airLong[order(airLong$Date),])

# create chart with all 4 variables
gline.all <- ggplot(airLong, aes(x=Date, y=value, color=variable)) + geom_line()
gline.all # inspect chart
# -------------- End of different approach

## Step 4: Look at all the data via a Heatmap
htmap <- ggplot(airLong, aes(x=Date, y=variable)) + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="red")
htmap # inspect heatmap

## Step 5: Look at all the data via a scatter chart
gscatter <- ggplot(air) +  geom_point(aes(x=Wind, y=Temp, size=Ozone, color=Solar.R))
gscatter # inspect scatter plot

# ------------------------------------- end of the other approach
## Step 6: Final Analysis
#
#
#
#
# --------------- The following is for you, not required 
#set up your working directory
setwd("D:\\")
# save a plot as a jpg file
ggsave(gscatter,filename="SavedFromRClass.jpg",  width = 12, height = 12)

#the following is an example of creating excel sheets and storing in them 
#data and plots, it may not work at your end until you install the library 
#and have the right names of data and plots
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "WeeklyDifferencer")
addWorksheet(wb, "WeeklyDifference_Bus")
addWorksheet(wb, "Charts")
conditionalFormatting(wb, "Charts", cols= 1:10, rows = 1:30, style = negStyle)
writeData(wb, "WeeklyDifferencer", OverallComPayerWeek)
writeData(wb, "WeeklyDifference_Bus", OverallComWeek)
insertImage(wb, "Charts", "WeeklyDiff_Payer.jpg", width=5.82, height=5.82, units="in", startRow = 2, startCol = 1)
insertImage(wb, "Charts", "WeeklyDiff_Payer_Bus.jpg", width=5.82, height=5.82, units="in", startRow = 2, startCol =12)
saveWorkbook(wb, paste(Ext, "_AMs.xlsx",sep = ""), overwrite=TRUE)

