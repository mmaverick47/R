
> load("Homework 4 Data Set - Universal Bank.csv")

> UniversalBank <- 
  +   read.table("Homework 4 Data Set - Universal Bank.csv",
                 +    header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

> GLM.1 <- glm(PersonalLoan ~ Income + Family + Income*Family, family=binomial(logit), 
               +   data=UniversalBank)

> summary(GLM.1)

Call:
  glm(formula = PersonalLoan ~ Income + Family + Income * Family, 
      family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-2.28751  -0.28285  -0.15888  -0.07353   2.95928  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -3.6146848  0.4796968  -7.535 4.87e-14 ***
  Income         0.0001998  0.0038434   0.052    0.959    
Family        -1.4895358  0.2119153  -7.029 2.08e-12 ***
  Income:Family  0.0202891  0.0018521  10.955  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162.0  on 4999  degrees of freedom
Residual deviance: 1667.2  on 4996  degrees of freedom
AIC: 1675.2

Number of Fisher Scoring iterations: 7


> exp(coef(GLM.1))  # Exponentiated coefficients ("odds ratios")
(Intercept)        Income        Family Income:Family 
0.02692541    1.00019979    0.22547729    1.02049627 

> GLM.2 <- glm(PersonalLoan ~ Income + Mortgage + Income*Mortgage, 
               +   family=binomial(logit), data=UniversalBank)

> summary(GLM.2)

Call:
  glm(formula = PersonalLoan ~ Income + Mortgage + Income * Mortgage, 
      family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.1330  -0.2980  -0.1727  -0.1091   2.7577  

Coefficients:
  Estimate  Std. Error z value Pr(>|z|)    
(Intercept)     -6.24750755  0.22047396 -28.337   <2e-16 ***
  Income           0.03756196  0.00162808  23.071   <2e-16 ***
  Mortgage         0.00226986  0.00167125   1.358    0.174    
Income:Mortgage -0.00001083  0.00001103  -0.982    0.326    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162.0  on 4999  degrees of freedom
Residual deviance: 2012.6  on 4996  degrees of freedom
AIC: 2020.6

Number of Fisher Scoring iterations: 6


> exp(coef(GLM.2))  # Exponentiated coefficients ("odds ratios")
(Intercept)          Income        Mortgage Income:Mortgage 
0.001935272     1.038276331     1.002272442     0.999989170 

> GLM.3 <- glm(PersonalLoan ~ Income +CCAvg + Income*CCAvg, family=binomial(logit), 
               +   data=UniversalBank)

> summary(GLM.3)

Call:
  glm(formula = PersonalLoan ~ Income + CCAvg + Income * CCAvg, 
      family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.0995  -0.2660  -0.1155  -0.0416   2.6515  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -9.5556476  0.4309632  -22.17   <2e-16 ***
  Income        0.0600142  0.0031175   19.25   <2e-16 ***
  CCAvg         1.3398815  0.1215377   11.02   <2e-16 ***
  Income:CCAvg -0.0086107  0.0008001  -10.76   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162  on 4999  degrees of freedom
Residual deviance: 1880  on 4996  degrees of freedom
AIC: 1888

Number of Fisher Scoring iterations: 7


> exp(coef(GLM.3))  # Exponentiated coefficients ("odds ratios")
(Intercept)        Income         CCAvg  Income:CCAvg 
0.00007080028 1.06185159426 3.81859105026 0.99142625065 

> GLM.4 <- glm(PersonalLoan ~ Income + CCAvg +Family + Mortgage + CCAvg*Income, 
               +   family=binomial(logit), data=UniversalBank)

> summary(GLM.4)

Call:
  glm(formula = PersonalLoan ~ Income + CCAvg + Family + Mortgage + 
        CCAvg * Income, family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.3160  -0.2398  -0.0783  -0.0223   3.1697  

Coefficients:
  Estimate  Std. Error z value Pr(>|z|)    
(Intercept)  -13.7633096   0.6028217 -22.831   <2e-16 ***
  Income         0.0747391   0.0037577  19.890   <2e-16 ***
  CCAvg          1.6458509   0.1361105  12.092   <2e-16 ***
  Family         0.9374746   0.0679472  13.797   <2e-16 ***
  Mortgage       0.0003502   0.0004469   0.784    0.433    
Income:CCAvg  -0.0105962   0.0008972 -11.810   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162.0  on 4999  degrees of freedom
Residual deviance: 1646.6  on 4994  degrees of freedom
AIC: 1658.6

Number of Fisher Scoring iterations: 8


> exp(coef(GLM.4))  # Exponentiated coefficients ("odds ratios")
(Intercept)         Income          CCAvg         Family       Mortgage   Income:CCAvg 
0.000001053587 1.077603004957 5.185420162330 2.553524696088 1.000350226219 0.989459738687 

> GLM.5 <- glm(PersonalLoan ~ Income + CCAvg + Family + Income*CCAvg, family=binomial(logit), 
               +   data=UniversalBank)

> summary(GLM.5)

Call:
  glm(formula = PersonalLoan ~ Income + CCAvg + Family + Income * 
        CCAvg, family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.3362  -0.2391  -0.0780  -0.0221   3.1918  

Coefficients:
  Estimate  Std. Error z value Pr(>|z|)    
(Intercept)  -13.7771831   0.6031461  -22.84   <2e-16 ***
  Income         0.0750491   0.0037406   20.06   <2e-16 ***
  CCAvg          1.6469211   0.1361439   12.10   <2e-16 ***
  Family         0.9395873   0.0679152   13.84   <2e-16 ***
  Income:CCAvg  -0.0106102   0.0008969  -11.83   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162.0  on 4999  degrees of freedom
Residual deviance: 1647.2  on 4995  degrees of freedom
AIC: 1657.2

Number of Fisher Scoring iterations: 8


> exp(coef(GLM.5))  # Exponentiated coefficients ("odds ratios")
(Intercept)         Income          CCAvg         Family   Income:CCAvg 
0.000001039071 1.077937099540 5.190972794620 2.558925062946 0.989445862903 

> GLM.6 <- glm(PersonalLoan ~ Income + CCAvg + Family +Education + CCAvg*Income, 
               +   family=binomial(logit), data=UniversalBank)

> summary(GLM.6)

Call:
  glm(formula = PersonalLoan ~ Income + CCAvg + Family + Education + 
        CCAvg * Income, family = binomial(logit), data = UniversalBank)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.2018  -0.1643  -0.0401  -0.0073   3.8009  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -20.146060   0.883313  -22.81   <2e-16 ***
  Income         0.098543   0.004896   20.13   <2e-16 ***
  CCAvg          2.220100   0.167626   13.24   <2e-16 ***
  Family         0.770788   0.074187   10.39   <2e-16 ***
  Education      1.818407   0.109227   16.65   <2e-16 ***
  Income:CCAvg  -0.014008   0.001091  -12.84   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 3162.0  on 4999  degrees of freedom
Residual deviance: 1255.7  on 4994  degrees of freedom
AIC: 1267.7

Number of Fisher Scoring iterations: 8


> exp(coef(GLM.6))  # Exponentiated coefficients ("odds ratios")
(Intercept)            Income             CCAvg            Family         Education 
0.000000001781054 1.103561413510691 9.208248404917633 2.161468599165782 6.162034145175938 
Income:CCAvg 
0.986089901819070 

> UBnet <- neuralnet(PersonalLoan ~ Income + CCAvg + Family + Education + SecuritiesAccount, UniversalBank, hidden=2, lifesign="minimal", linear.output=FALSE, threshold=0.01)

> plot(UBnet)

> UBnet$result.matrix
[,1]
error                         216.9600000017
reached.threshold               0.0003534488
steps                          39.0000000000
Intercept.to.1layhid1           2.6158871371
Income.to.1layhid1              2.6123620095
CCAvg.to.1layhid1               4.9839920777
Family.to.1layhid1              3.7460275748
Education.to.1layhid1           4.3649864413
SecuritiesAccount.to.1layhid1   3.2637066191
Intercept.to.1layhid2           2.7879790658
Income.to.1layhid2              3.5020707734
CCAvg.to.1layhid2               3.1980808074
Family.to.1layhid2              3.7840969652
Education.to.1layhid2           2.8294262574
SecuritiesAccount.to.1layhid2   2.0378114145
Intercept.to.PersonalLoan       0.9444804493
1layhid1.to.PersonalLoan       -2.1124030270
1layhid2.to.PersonalLoan       -1.0745492054

> UBnet <- neuralnet(PersonalLoan ~ Income + CCAvg, UniversalBank, hidden=2, lifesign="minimal", linear.output=FALSE, threshold=0.01)

> plot(UBnet)

> UBnet$result.matrix
[,1]
error                       144.281107766
reached.threshold             0.008745255
steps                     21291.000000000
Intercept.to.1layhid1        10.302937782
Income.to.1layhid1            0.139318425
CCAvg.to.1layhid1            -4.559067935
Intercept.to.1layhid2        -0.309347740
Income.to.1layhid2           -0.031719444
CCAvg.to.1layhid2            -0.360938344
Intercept.to.PersonalLoan    -1.258527321
1layhid1.to.PersonalLoan      1.517794771
1layhid2.to.PersonalLoan   -159.158431193

