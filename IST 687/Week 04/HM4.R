#Step 1: Write a summarizing function to understand the distribution of a vector

#1. The function, call it ‘printVecInfo’ should take a vector as input
#2. The function should print the following information:
  #a. Mean
  #b. Median
  #c. Min & max
  #d. Standard deviation
  #e. Quantiles (at 0.05 and 0.95) 
  #f. Skewness

printVecInfo <- function(X){
  meanX <- mean(X)
  medianX <- median(X)
  minX <- min(X)
  maxX <- max(X)
  sdX <- sd(X)
  quantileX <- quantile(X, probabiliy=c(0.05,0.95))
  skewX <- skewness(X)
  cat("mean:", meanX,
      "median:", medianX,
      "min:", minX,
      "max:", maxX,
      "standard deviation:", sdX,
      "quantiles:", quantileX,
      "skewness:", skewX)
}

#The \ did not work to create a line break. It gave me an error every time.

#3. Test the function with a vector.

testVec <- c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo(testVec)

#Step 2: Creating Samples in a Jar

#4. Create a variable "jar" that has 50 red and 50 blue marbles.

R <- "red"
B <- "blue"
red <- replicate(50, R)
blue <- replicate(50, B)
jar <- c(red, blue)

#5. Confirm there are 50 red by summing the samples that are red.

redCount <- length(jar[jar=="red"])
redCount

#6. Sample 10 'marbles' from the jar. How many are red? What percentage is red?

#Sample jar
sampleJar <- sample(jar,50,replace=TRUE)
sampleJar

#Red count
redCount <- length(sampleJar[sampleJar=="red"])
redCount

#Red percentage
redPerc <- redCount / length(sampleJar) * 100
cat(redCount, "Red equal to", redPerc, "%")

#Sample function
samRed <- function(v,x){
  samp <- sample(v,x,replace=TRUE)
  num <- length(samp[samp=="red"])/length(samp)
  return(num)
}

samRed(jar,10)

#7. Do the sampling 20 times using replicate command. Should generate list of 20 numbers.

samX <- mean(replicate(10,samRed(jar,10)))
samRedMeans <- replicate(20, samRed(jar,10))

#Use printVecInfo to see information of the samples.
printVecInfo(samRedMeans)

#Generate histogram of the samples.
hist(samRedMeans)

#8. Repeat #7, but this time sample the jar 100 times. You should get 20 numbers.

samRedMeans2 <- replicate(20, samRed(jar,100))

#Use printVecInfo to see information of the samples.
printVecInfo(samRedMeans2)

#Generate histogram of the samples.
hist(samRedMeans2)

#9. Repeat #8, but replicate the sampling 100 times. You should get 100 numbers.

samRedMeans3 <- replicate(100, samRed(jar,100))

#Use printVecInfo to see information of the samples.
printVecInfo(samRedMeans3)

#Generate histogram of the samples.
hist(samRedMeans3)

#Step 3: Explore the 'airquality' dataset

#10. Store the 'airquality' dataset into a temporary variable

temp <- airquality

#11. Clean the dataset (i.e. remove the NAs)

temp2 <- na.omit(temp)

#12. Explore the Ozone, Wind, and Temp by doing the 'printVecInfo" on each

printVecInfo(temp$Ozone)
printVecInfo(temp$Wind)
printVecInfo(temp$Temp)

#Generate a histogram for each
hist(temp$Ozone)
hist(temp$Wind)
hist(temp$Temp)
