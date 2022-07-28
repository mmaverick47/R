mean(replicate(400,mean(sample(dfStates$Jul2010, size=16, replace=TRUE)), simplify=TRUE))

sample(dfStates$Jul2010, size=16, replace=TRUE)

hist(replicate(4000,mean(sample(dfStates$Jul2010, size=16, replace=TRUE)), simplify=TRUE))

SampleMeans <- replicate(10000,mean(sample(dfStates$Jul2010, size=16, replace=TRUE)), simplify=TRUE)

length(SampleMeans)
mean(SampleMeans)
hist(SampleMeans)

summary(SampleMeans)

quantile(SampleMeans, prob=c(0.25, 0.50, 0.75))

jar <- c(1,0,-1)
jar
numSamples <- 4
sample(jar,numSamples,replace=TRUE)
sum(sample(jar,numSamples,replace=TRUE))
mean(sample(jar,numSamples,replace=TRUE))

replicate(5,sample(jar,numSamples,replace=TRUE), simplify=TRUE)

sampleMeans <- replicate(250,mean(sample(jar,numSamples, replace=TRUE)), simplify=TRUE)

mean(sampleMeans)
sd(sampleMeans)
hist(sampleMeans)


samples <- rnorm(10000, 50, 2)
mean(samples)


library(moments)
mean(dfStates$Jul2010)
v <- replicate(1000, mean(sample(dfStates$Jul2010, size=16, replace=TRUE)), simplify=TRUE)
mean(v)
hist(v)
