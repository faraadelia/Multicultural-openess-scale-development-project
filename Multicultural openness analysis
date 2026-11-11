library(readr)
library(psych)
dataset_6 <- read_csv("data_6.csv")
class(dataset_6)
dim(dataset_6)
head(dataset_6)
describe(dataset_6)
data6<-dataset_6[, c("Q6_1", "Q6_2", "Q6_3","Q6_4","Q6_5","Q6_6", "Q6_7","Q6_8","Q6_9","Q6_10")]
head(data6)
nrow(data6)
head(data6)
describe(data6) #to do univariate data analysis 
out<-alpha(data6)
round(out$response.freq,2) #to see frequency distribution 
multi.hist(data6, density=FALSE, freq=TRUE, ylim=c(0,80)) #to make histogram
round(cor(data6),2) #to find correlation 
round(out$item.stats,2) #to find the r.drop value 
out<-alpha(data6)
out$total$raw_alpha #preliminary reliability estimate 
round(out$alpha.drop,3) #alpha if item drop 

head(data6)
data6final<-data6[, c("Q6_1","Q6_2","Q6_3","Q6_4","Q6_6","Q6_7","Q6_8","Q6_10")]
data6final

#shortened scale 
library(lavaan)
library(semTools)
out1 <- efa(data6final, nfactors = 1)
summary(out1)
out2<-out1[[1]]
omega<-semTools::compRelSEM(out2,obs.var=FALSE)
omega #omega 
alpha<-alpha(data6final)$total$raw_alpha #alpha or reliability decrease as we decrease the item length from 10 to 4
round(alpha,3) #alpha 
round(cor(data6final),3)
lavInspect(out2,"cor.ov") 
lavResiduals(out2)$cov #to find the residual correlation 
lavResiduals(out2)$cov.z 
fitmeasures(out2)["srmr"]
summary(out1) #to find the eigenvalue 
psych::scree(data6final) 
describe(data6final)
data6final$Nsum<-rowSums(data6final)
describe(data6final$Nsum) #Total score distribution
hist(data6final$Nsum, main="Total Scale Score") #histogram of total score distribution
data6final$Nsum <- rowSums(data6final[, c("Q6_1","Q6_2","Q6_3","Q6_4","Q6_6","Q6_7","Q6_8","Q6_10")], na.rm = TRUE)
cor.test(data6final$Nsum, dataset_6$G6V) #validity test with our group validity question 
cor.test(data6final$Nsum, dataset_6$G9V) #validity test with group 9 validity question
