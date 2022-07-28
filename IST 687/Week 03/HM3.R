#Step 1: Create a function (named readStates) to read a CSV file into R

readStates <- function(){
  statePop <- (read.csv(url("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv")))
  statePop <- statePop[-1:-8,]
  rownames(statePop) <- NULL
  statePop <- statePop[-52:-58,]
  statePop <- statePop[,-6:-10]
  colnames(statePop) <- c("stateName", "base2010", "base2011", "Jul2010", "Jul2011")
  statePop$stateName <- gsub("\\.","",statePop$stateName)
  statePop$base2010 <- gsub("\\,","",statePop$base2010)
  statePop$base2011 <- gsub("\\,","",statePop$base2011)
  statePop$Jul2010 <- gsub("\\,","",statePop$Jul2010)
  statePop$Jul2011 <- gsub("\\,","",statePop$Jul2011)
  statePop$base2010 <- as.numeric(statePop$base2010)
  statePop$base2011 <- as.numeric(statePop$base2011)
  statePop$Jul2010 <- as.numeric(statePop$Jul2010)
  statePop$Jul2011 <- as.numeric(statePop$Jul2011)
  return(statePop)
}

#Step 2: Clean the dataframe

#3. Note the issues that need to be fixed (removing columns, removing rows, changing column names).

#Remove rows 1 - 8

readStates <- readStates[-1:-8,]
rownames(readStates) <- NULL
readStates <- readStates[-52:-58,]

#Remove columns 6 - 10

readStates <- readStates[,-6:-10]

#Change column names

colnames(readStates) <- c("stateName", "base2010", "base2011", "Jul2010", "Jul2011")

#Remove periods in state names

readStates$stateName <- gsub("\\.","",readStates$stateName)

#Remove commas from populations

readStates$base2010 <- gsub("\\,","",readStates$base2010)
readStates$base2011 <- gsub("\\,","",readStates$base2011)
readStates$Jul2010 <- gsub("\\,","",readStates$Jul2010)
readStates$Jul2011 <- gsub("\\,","",readStates$Jul2011)

#Convert populations to numbers

readStates$base2010 <- as.numeric(readStates$base2010)
readStates$base2011 <- as.numeric(readStates$base2011)
readStates$Jul2010 <- as.numeric(readStates$Jul2010)
readStates$Jul2011 <- as.numeric(readStates$Jul2011)

#4. Within your function, make sure there are 51 rows (one per state + the district of Columbia). Make sure there are only 5 columns with the columns having the following names (stateName, base2010, base2011, Jul2010, Jul2011).

str(readStates)

#5. Make sure the last four columns are numbers (i.e. not strings).

str(readStates)

#Step 3: Store and Explore the dataset

#6. Store the dataset into a dataframe, called dfStates.

dfStates <- readStates()

#7. Test your dataframe by calculating the mean for the July2011 data, by doing: mean(dfStates$Jul2011).

mean(dfStates$Jul2011)

#Step 4: Find the state with the Highest Population

#8. Based on the July2011 data, what is the population of the state with the highest population? What is the name of that state?

which.max(dfStates$Jul2011)
dfStates$stateName[5]

#9. Sort the data, in increasing order, based on the July2011 data.

sort(dfStates$Jul2011, decreasing = FALSE)

#Step 5: Explore the distribution of the states

#10. Write a function that takes two parameters. The first is a vector and the second is a number.

#11. The function will return the percentage of the elements within the vector that is less than the same
#(i.e. the cumulative distribution below the value provided).

distStates <- function(myVec, myNum){
  newNum <- myVec[myVec < myNum]
  return(length(newNum) / length(myVec))
}


#12. For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function,
    #the function would return 0.2 (since 20% of the numbers were below 2).
    
#13. Test the function with the vector ‘dfStates$Jul2011Num’, and the mean of dfStates$Jul2011Num’.

distStates(dfStates$Jul2011, mean(dfStates$Jul2011))

#I got help with Step 5 from Chris Vonderach. Kudos to him!

