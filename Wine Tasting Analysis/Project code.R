# First, set your working directory so you don't have to type out the full 
# file path each time you pull a file from the same folder

setwd("/Users/Documents/01-Education/Syracuse/2021/01-Winter/IST 687/Project/Archive")

# Import the first csv file

wine1 <- read.csv(file = 'winemag-data_first150k.csv')

# Import the second csv file

wine2 <- read.csv(file = 'winemag-data-130k-v2.csv')

# Remove X column from wine1, set rownames to NULL, remove duplicate rows

wine1 <- wine1[,-1]
rownames(wine1) <- NULL
wine1[!duplicated(wine1$description),]

# Remove X column from wine 2, set rownames to NULL, remove duplicate rows

wine2 <- wine2[,-1]
rownames(wine2) <- NULL
wine2[!duplicated(wine2$description),]

# Remove taster, and variety columns from wine2 so it can be combined with wine1 

wine2 <- wine2[,-9:-11]

#M erge data frames into one data frame

wineData <- merge(wine1, wine2,by=c('country', 'description', 'designation', 'points', 'price'
                          , 'province', 'region_1', 'region_2', 'variety', 'winery'), all.x=T)

# Remove duplicate rows

cleanWine <- wineData[!duplicated(wineData$description),]

# Remove periods

cleanWine$description <- gsub("\\.","",cleanWine$description)

# Remove blank country values

cleanWine <- cleanWine[-1:-3,]

# Points and Price analysis

# Moving them into their own data frame for easier analysis
pp <- data.frame(cleanWine$points, cleanWine$price)

# Renaming columns

names(pp)[names(pp) == "cleanWine.points"] <- "points"
names(pp)[names(pp) == "cleanWine.price"] <- "price"


# Counting NAs

sum(is.na(pp$points))
sum(is.na(pp$price))   #8713 blank prices!!!

# Omitting blanks, since it's too many to replace with the mean
pp <- na.omit(pp)

# Making values numeric to run stats
as.numeric(pp$points)
as.numeric(pp$price)

# Measures of central tendency
mean(pp$points)
median(pp$points)
sd(pp$points)
min(pp$points)
max(pp$points)

mean(pp$price)
median(pp$price)
sd(pp$price)
min(pp$price)
max(pp$price)

library(ggplot2)

# Histograms

# Fancy price histogram with all data points
priceHist <- ggplot(pp, aes(x=price)) + geom_histogram(fill = "red4", color = "black")
priceHist <- priceHist + ggtitle("Price Distribution")
priceHist <- priceHist+ xlab("Price")
priceHist <- priceHist+ ylab("Frequency")
priceHist

# Fancy price histogram with outliers removed

# Create a new data frame removing all rows with prices over 200

pp2 <- pp[!rowSums(pp > 200),]

adjpriceHist <- ggplot(pp2, aes(x=price)) + geom_histogram(fill = "red4", color = "black")
adjpriceHist <- adjpriceHist + ggtitle("Price Distribution")
adjpriceHist <- adjpriceHist+ xlab("Price")
adjpriceHist <- adjpriceHist+ ylab("Frequency")
adjpriceHist
  
# Fancy points histogram
  
pointsHist <- ggplot(pp, aes(x=points)) + geom_histogram(fill = "red4", color = "black")
pointsHist <- pointsHist + ggtitle("Points Distribution")
pointsHist <- pointsHist+ xlab("Points")
pointsHist <- pointsHist+ ylab("Frequency")
pointsHist


#S  atter plots


# Basic scatter plot

ggplot(pp) + geom_point(aes(x=points, y=price))

# Fancy scatter plot

pPlot <- ggplot(data = pp, aes(x=price, y=points)) + geom_point(color = "red4") + geom_smooth(method = "lm", color = "dodgerblue2")
pPlot <- pPlot + ggtitle("Price vs Points")
pPlot <- pPlot+ xlab("Price")
pPlot <- pPlot+ ylab("Points")
pPlot

# Fancy scatter plot with price outliers removed

pPlot2 <- ggplot(data = pp2, aes(x=price, y=points)) + geom_point(color = "red4") + geom_smooth(method = "lm", color = "dodgerblue2")
pPlot2 <- pPlot2 + ggtitle("Price vs Points")
pPlot2 <- pPlot2 + xlab("Price")
pPlot2 <- pPlot2 + ylab("Points")
pPlot2

# Line plot

ggplot(pp, aes(x=points)) + geom_line(aes(y=price))

# Box plots

ggplot(pp, aes(y=points)) + geom_boxplot()
ggplot(pp, aes(y=price)) + geom_boxplot()

#Box plot with outliers removed

ggplot(pp2, aes(y=price)) + geom_boxplot()

#Prediction models

# Linear model of points as a function of price

priceLM <-  lm(points ~ price, data=pp)
summary(priceLM)

# The p-values are statistically significant, however the adjusted R-squared is only 20%,
# but that might be considered a good percentage for explaining wine ratings based solely on price.

predict(priceLM, data.frame(price = 100))


# Linear model of price as a function of points

pointsLM <-  lm(price ~ points, data=pp)
summary(pointsLM)

# Looks the same as the reverse (no surprise). The p-values are statistically significant, 
# however the adjusted R-squared is only 20%,
# but that might be considered a good percentage for explaining wine ratings based solely on price.

predict(pointsLM, data.frame(points = 85))


# Create heat map of points / price / country

ppNew <- data.frame(cleanWine$country, cleanWine$points, cleanWine$price)
names(ppNew)[names(ppNew) == "cleanWine.points"] <- "points"
names(ppNew)[names(ppNew) == "cleanWine.price"] <- "price"
names(ppNew)[names(ppNew) == "cleanWine.country"] <- "country"

sum(is.na(ppNew$points))
sum(is.na(ppNew$price))   #8713 blank prices!!!
sum(is.na(ppNew$country))
ppNew <- na.omit(ppNew)
as.numeric(ppNew$points)
as.numeric(ppNew$price)
as.factor(ppNew$country)
ppN2 <- ppNew[!(ppNew$price > 200),]
ppN2 <- ppN2[-97818,]

install.packages("ggpubr")
library(ggpubr)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(knitr)
library(ggplot2)

ppHM <- ggplot(data = ppN2, aes(x=points, y=country)) + geom_point(aes(color=price))
ppHM <- ppHM + scale_colour_gradient(low = "khaki1", high = "orchid4")
ppHM <- ppHM + ggtitle("Price & Points Heat Map by Country")
ppHM <- ppHM + xlab("Points")
ppHM

test1 <- ppNew %>% group_by(country) %>% count()


test2 <- ppNew %>% group_by(country,points,price) %>% count()
test2 <- test2[test2$price < 200,] 
test2

plot1 <- ggplot(data = test2, aes(x=points, y=country)) + geom_point(aes(color=price, size=n))
plot1 <- plot1 + labs(title = "Country Points and Price Distribution and Frequency", x = "Rating", y = "Country", color = "Price", size = "Frequency")
plot1 <- plot1 + scale_colour_gradient(low = "khaki1", high = "orchid4")
plot1
