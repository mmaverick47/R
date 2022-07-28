#Copy mtcars into a new variable called myCars
myCars <- mtcars

#-------------------------
#STEP 1: What is the hp?

#1 What is the highest hp?
which.max(myCars$hp)

#2 Which car has the highest hp?
myCars[31,]

#-------------------------
#STEP 2: Explore mpg

#3 What is the highest mpg?
max(myCars$mpg)

#4 Which car has the highest mpg?
rownames(myCars[which.max(myCars$mpg),])

#5 Create a sorted dataframe, based on mpg
myCarsMPG <- myCars[order(-myCars$mpg),]
myCarsMPG

#-------------------------
#STEP 3: Which car has the "best" combination of mpg and hp?

#6 Which logic did you use?
myCarsMPG[1:10,]
#The car with the highest hp from the top 10 of the highest mpg cars

#7 Which car?
#Lotus Europa
myCarsMPG[myCarsMPG$hp==113,]

#-------------------------
#STEP 4: Which car has the best combo of mpg and hp where they are given equal weight?
mean(myCars$mpg)
mean(myCars$hp)

myCarsMPG$scaledMPG <- scale(myCars$mpg)
myCarsMPG$scaledHP <- scale(myCars$hp)
myCarsMPG[order(-myCarsMPG$scaledMPG),]
myCarsMPG[order(-myCarsMPG$scaledHP),]

#I'm going to say the top results when sorted by scaled mpg, since this is still subjective.
#I'm valuing mpg over hp, so the "best" combo is the Mercedes 280C.
#If you value hp, the "best" combo is the Cadillac Fleetwood, which only has mpg of 10.4, and that's terrible!
#So, I'm going with the Mercedes 280C, based on scaled mpg.
#If I eyeball it based on sorted mpg (not scaled), I'd say the Ferrari Dino.