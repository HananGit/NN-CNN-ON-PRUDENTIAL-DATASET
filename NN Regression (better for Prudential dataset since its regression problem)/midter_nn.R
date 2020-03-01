rm(list=ls())
#setwd("~/Documents/OneDrive - Northeastern University/INFO7390/Neural Networks Assignment")

#Load required library
library(neuralnet)
library(caret)

#Define measurements
rmse <- function(error){
  sqrt(mean(error^2))
}

mae <- function(error){
  mean(abs(error))
}

#Read midterm dataset
set.seed(500)
data_from_midterm <- read.csv("data_16_columns.csv")[-1]
dim(data_from_midterm);str(data_from_midterm)

#Scale the dataset
maxs <- apply(data_from_midterm, 2, max) 
mins <- apply(data_from_midterm, 2, min)

scaled <- as.data.frame(scale(data_from_midterm, center = mins, scale = maxs - mins))
dim(scaled);str(scaled)

#Split into train and test data
sample_index = sample(nrow(scaled), nrow(scaled)*0.2)
test_dataset = scaled[sample_index,]
train_dataset = scaled[-sample_index,]
x <- subset(train_dataset, select = -Response) # x is the predict variable 
y <- train_dataset[,'Response'] #y is factor
dim(train_dataset)

##Try the first nn
f <- as.formula(paste("Response ~", 
                      paste(names(data_from_midterm)[!names(data_from_midterm) %in% "Response"], collapse = " + ")))
nn <- neuralnet(f, data = train_dataset, hidden=11, lifesign = "full",
                stepmax = 1e7, rep=1,
                learningrate.factor = list(minus = 0.5, plus = 1.2), threshold = 0.23,
                algorithm = "rprop+")
summary(nn) 
plot(nn)
#Predict NN performance on test dataset
pr.nn <- (compute(nn,test_dataset[,1:15])$net.result)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
test.r <- (test_dataset$Response)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
MSE.nn <- rmse(pr.nn - test.r); MAE.nn <- mae(pr.nn - test.r)
print(MSE.nn) 
#[1] 2.320669455
print(MAE.nn)
#[1] 1.925377265

pr.nn_rep <- pr.nn

pr.nn_rep[round(pr.nn_rep)>=8] <- 8
pr.nn_rep[round(pr.nn_rep)<=1] <- 1

confusionMatrix(test.r, round(pr.nn_rep))

##Try a second nn
nn_2 <- neuralnet(f, data = train_dataset, hidden=c(10,5), lifesign = "full",
                stepmax = 1e7, rep=1,
                learningrate.factor = list(minus = 0.5, plus = 1.2), threshold = 0.15,
                algorithm = "rprop+")
summary(nn_2) 
plot(nn_2)
#Predict NN performance on test dataset
pr.nn_2 <- (compute(nn_2,test_dataset[,1:15])$net.result)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
MSE.nn_2 <- rmse(pr.nn_2 - test.r)
MAE.nn_2 <- mae(pr.nn_2 - test.r)
print(MSE.nn_2) 
#[1] 2.324929278
print(MAE.nn_2)
#[1] 1.927565246

##Try a third nn
nn_3 <- neuralnet(f, data = train_dataset, hidden=1, lifesign = "full",
                  stepmax = 1e7, rep=1,
                  learningrate.factor = list(minus = 0.5, plus = 1.2), threshold = 0.1,
                  algorithm = "rprop+")
summary(nn_3) 

#Predict NN performance on test dataset
pr.nn_3 <- (compute(nn_3,test_dataset[,1:15])$net.result)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
MSE.nn_3 <- rmse(pr.nn_3 - test.r)
MAE.nn_3<- mae(pr.nn_3 - test.r)
print(MSE.nn_3) 
#[1] 2.322958041
print(MAE.nn_3)
#[1] 1.926729142

##Try a forth nn
nn_4 <- neuralnet(f, data = train_dataset, hidden=8, lifesign = "full",
                  stepmax = 1e7, rep=1,
                  learningrate.factor = list(minus = 0.5, plus = 1.2), threshold = 0.1,
                  algorithm = "rprop+")
summary(nn_4) 

#Predict NN performance on test dataset
pr.nn_4 <- (compute(nn_4,test_dataset[,1:15])$net.result)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
MSE.nn_4 <- rmse(pr.nn_4 - test.r)
MAE.nn_4<- mae(pr.nn_4 - test.r)
print(MSE.nn_4) 
#[1] 2.322529671
print(MAE.nn_4)
#[1] 1.927285048

##Try a fifth nn with backprop - this runs pretty fast so I tried multiple hidden layers, rep times
##learningrate and threshold, however all the results were pretty disappointing
nn_5<- neuralnet(f, data = train_dataset, hidden=c(10,3), lifesign = "full",
                  stepmax = 1e7, rep=10,
                  learningrate = 0.005, threshold = 0.003,
                  algorithm = "backprop", linear.output=F)
summary(nn_5) 

#Predict NN performance on test dataset 
pr.nn_5 <- (compute(nn_5,test_dataset[,1:15])$net.result)*(max(data_from_midterm$Response)-min(data_from_midterm$Response))+min(data_from_midterm$Response)
MSE.nn_5 <- rmse(pr.nn_5 - test.r)
MAE.nn_5<- mae(pr.nn_5 - test.r)
print(MSE.nn_5) 
#[1] 3.384031069
print(MAE.nn_5)
#[1] 2.340350218


