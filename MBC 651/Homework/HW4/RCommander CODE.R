
load("Homework 4 Data Set - Universal Bank.csv")
UniversalBank <- 
  read.table("Homework 4 Data Set - Universal Bank.csv",
   header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GLM.1 <- glm(PersonalLoan ~ Income + Family + Income*Family, family=binomial(logit), 
  data=UniversalBank)
summary(GLM.1)
exp(coef(GLM.1))  # Exponentiated coefficients ("odds ratios")
GLM.2 <- glm(PersonalLoan ~ Income + Mortgage + Income*Mortgage, 
  family=binomial(logit), data=UniversalBank)
summary(GLM.2)
exp(coef(GLM.2))  # Exponentiated coefficients ("odds ratios")
GLM.3 <- glm(PersonalLoan ~ Income +CCAvg + Income*CCAvg, family=binomial(logit), 
  data=UniversalBank)
summary(GLM.3)
exp(coef(GLM.3))  # Exponentiated coefficients ("odds ratios")
GLM.4 <- glm(PersonalLoan ~ Income + CCAvg +Family + Mortgage + CCAvg*Income, 
  family=binomial(logit), data=UniversalBank)
summary(GLM.4)
exp(coef(GLM.4))  # Exponentiated coefficients ("odds ratios")
GLM.5 <- glm(PersonalLoan ~ Income + CCAvg + Family + Income*CCAvg, family=binomial(logit), 
  data=UniversalBank)
summary(GLM.5)
exp(coef(GLM.5))  # Exponentiated coefficients ("odds ratios")
GLM.6 <- glm(PersonalLoan ~ Income + CCAvg + Family +Education + CCAvg*Income, 
  family=binomial(logit), data=UniversalBank)
summary(GLM.6)
exp(coef(GLM.6))  # Exponentiated coefficients ("odds ratios")

UBnet <- neuralnet(PersonalLoan ~ Income + CCAvg, UniversalBank, hidden=2, lifesign="minimal", linear.output=FALSE, threshold=0.01)
plot(UBnet)

UBnet$result.matrix

