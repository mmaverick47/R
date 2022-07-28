# Call the libraries
library(gdata)
library(readxl)
library(ggplot2)


# 1 - Read in the data from the URL
 
df <- read_excel("Documents/01-Education/Syracuse/2021/Winter/IST 687/Week 8/mlr01.xls")

# 2 - Option to save file to computer

# 3 - Inspect the data using str() and make sure all cases are read in (n=8 years of observations) w/ 4 variables

str(df)

colnames(df) <- c("fawns", "adults", "precip", "winter")

# 4 - Create bivariate plots of # baby fawns vs adult antelope pop, precipitation, & severity of winter
      # Produce 3 separate plots
      # Make sure axes are labeled

    # Baby fawn vs adult antelope pop

    bbAdult <- ggplot(data = df, aes(x=adults, y=fawns)) + geom_point(color="brown") + geom_smooth(method = "lm", color = "red3")
    bbAdult <- bbAdult + ggtitle("Baby Fawns vs Adult Population")
    bbAdult <- bbAdult+ xlab("Adult Antelope Population")
    bbAdult <- bbAdult+ ylab("Fawn Population")
    bbAdult
    
    bbPrecip <- ggplot(data = df, aes(x=precip, y=fawns)) + geom_point(color="darkblue") + geom_smooth(method = "lm", color = "red3")
    bbPrecip <- bbPrecip + ggtitle("Baby Fawns vs Yearly Precipitation")
    bbPrecip <- bbPrecip + xlab("Yearly Precipitation")
    bbPrecip <- bbPrecip + ylab("Fawn Population")
    bbPrecip
    
    bbWinter <- ggplot(data = df, aes(x=winter, y=fawns)) + geom_point(color="lightblue3") + geom_smooth(method = "lm", color = "red3")
    bbWinter <- bbWinter + ggtitle("Baby Fawns vs Severity of Winter")
    bbWinter <- bbWinter + xlab("Severity of Winter")
    bbWinter <- bbWinter + ylab("Fawn Population")
    bbWinter

# 5 - Create 3 regression models of increasing complexity using lm()

    # 1st model: predict # of fawns from severity of winter

    LM1 <-  lm(fawns ~ winter, data=df)
    summary(LM1)
    
    predict(LM1, data.frame(winter = 3))

    # 2nd model: predict # of fawns from 2 variables (one being severity of winter)

    LM2 <-  lm(fawns~ winter + precip, data=df)
    summary(LM2)
    
    predict(LM2, data.frame(winter = 1, precip = 12.5))
    
    # 3rd model: predict # of fawns from 3 other variables

    LM3 <-  lm(fawns ~ winter + precip + adults, data=df)
    summary(LM3)
    
    predict(LM3, data.frame(winter = 2, precip = 15, adults = 57))

    # Which model works best? 

    # The model using all 3 variables appears to offer the most accuracy, LM3. 

    # Which of the predictors are statistically significant? 
    
    # For LM3, all 3 variables' p-values are statistically significant using an alpha of 0.05
    # when looking at the coefficients. The adjusted R-squared is the highest of the 3 models, 
    # at 0.955, meaning 95.5% of the variability of the number of baby fawns can be explained by 
    # the 3 variables (adult pop, precipitation, and winter severity),  and the p-value for 
    # the overall model is significant using an alpha of 0.01.