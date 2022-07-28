#Create vector "height"
height <- c(59,60,61,58,67,72,70)

#Create vector "weight"
weight <- c(150,140,180,220,160,140,130)

#Create variable "a <- 150"
a <- 150

#STEP ONE

#1. Compute average of "height"
mean(height)

#2. Computer average of "weight"
mean(weight)

#3. Calculate the length of the vector "height" and "weight"
length(height)
length(weight)

#4. Calculate the sum of "height"
sum(height)

#5. Computer the average of both "height" and "weight" by dividiing the sum by the length
sum(height)/length(height)
sum(weight)/length(weight)

#How does this compare to the "mean" function?
#The results are the same, 63.86 for height and 160 for weight.

#STEP TWO

#6. Compute the max height, store the result in "maxH"
max(height)
maxH <- max(height)

#7. Compute the min weight, store the result in "minW"
min(weight)
minW <- min(weight)

#STEP 3

#8. Create a new vector, which is the weight + 5 [every person gained 5 lbs]
newW <- weight +5

#9. Computer the weight/height for each person, using the new weight just created
newW/height

#STEP FOUR

#10. Write the R code to test if max height is greater than 60 (output “yes” or “no”)
if(maxH > 60) print("yes") else print("no")

#11. Write the R code to if min weight is greater than the variable ‘a’ (output “yes” or “no”)
if(minW > a) print("yes") else print("no")

