#Part 1: Data Preprocessing

setwd("C:/Users/h/Desktop/Final")

#Read the csv data
Prudential_Data <-read.csv("train.csv", header = T)
summary(Prudential_Data)
str(Prudential_Data)

# Finding the the percentage of null values in each column
colMeans(is.na(Prudential_Data)*100)


#Removing the columns that have more than or equal 70% of null values; and removing unnecessary Id column
Prudential_Data <- subset(Prudential_Data, select = -c(Id,Medical_History_15, Medical_History_24 ,Medical_History_32, Medical_History_10,Family_Hist_5) )


# Replacing the null values in each column with the computed mean of that column
Prudential_Data$Family_Hist_2 <- ifelse(is.na(Prudential_Data$Family_Hist_2), ave(Prudential_Data$Family_Hist_2, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_2)

Prudential_Data$Family_Hist_3 <- ifelse(is.na(Prudential_Data$Family_Hist_3), ave(Prudential_Data$Family_Hist_3, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_3)

Prudential_Data$Family_Hist_4 <- ifelse(is.na(Prudential_Data$Family_Hist_4), ave(Prudential_Data$Family_Hist_4, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_4)

Prudential_Data$Insurance_History_5 <- ifelse(is.na(Prudential_Data$Insurance_History_5), ave(Prudential_Data$Insurance_History_5, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Insurance_History_5)

Prudential_Data$Medical_History_1 <- ifelse(is.na(Prudential_Data$Medical_History_1), ave(Prudential_Data$Medical_History_1, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Medical_History_1)

Prudential_Data$Employment_Info_1 <- ifelse(is.na(Prudential_Data$Employment_Info_1), ave(Prudential_Data$Employment_Info_1, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_1)

Prudential_Data$Employment_Info_4 <- ifelse(is.na(Prudential_Data$Employment_Info_4), ave(Prudential_Data$Employment_Info_4, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_4)

Prudential_Data$Employment_Info_6 <- ifelse(is.na(Prudential_Data$Employment_Info_6), ave(Prudential_Data$Employment_Info_6, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_6)

# Checking the percentage of null values again
colMeans(is.na(Prudential_Data)*100)

#The sum of null values for a specific column should be zero after filling null values with the computed mean value
sum(is.na(Prudential_Data$Family_Hist_2)) # sum is zero as no more null values


#Setting categorical columns to as a factor to get factors with n levels
Prudential_Data$Product_Info_1 <- as.factor(Prudential_Data$Product_Info_1)
Prudential_Data$Product_Info_2 <- as.factor(Prudential_Data$Product_Info_2)
Prudential_Data$Product_Info_5 <- as.factor(Prudential_Data$Product_Info_5)
Prudential_Data$Product_Info_6 <- as.factor(Prudential_Data$Product_Info_6)
Prudential_Data$Product_Info_7 <- as.factor(Prudential_Data$Product_Info_7)

Prudential_Data$Employment_Info_3 <- as.factor(Prudential_Data$Employment_Info_3)
Prudential_Data$Employment_Info_5 <- as.factor(Prudential_Data$Employment_Info_5)

Prudential_Data$InsuredInfo_1 <- as.factor(Prudential_Data$InsuredInfo_1)
Prudential_Data$InsuredInfo_2 <- as.factor(Prudential_Data$InsuredInfo_2)
Prudential_Data$InsuredInfo_3 <- as.factor(Prudential_Data$InsuredInfo_3)
Prudential_Data$InsuredInfo_4 <- as.factor(Prudential_Data$InsuredInfo_4)
Prudential_Data$InsuredInfo_5 <- as.factor(Prudential_Data$InsuredInfo_5)
Prudential_Data$InsuredInfo_6 <- as.factor(Prudential_Data$InsuredInfo_6)
Prudential_Data$InsuredInfo_7 <- as.factor(Prudential_Data$InsuredInfo_7)

Prudential_Data$Insurance_History_1 <- as.factor(Prudential_Data$Insurance_History_1)
Prudential_Data$Insurance_History_2 <- as.factor(Prudential_Data$Insurance_History_2)
Prudential_Data$Insurance_History_3 <- as.factor(Prudential_Data$Insurance_History_3)
Prudential_Data$Insurance_History_4 <- as.factor(Prudential_Data$Insurance_History_4)
Prudential_Data$Insurance_History_7 <- as.factor(Prudential_Data$Insurance_History_7)
Prudential_Data$Insurance_History_8 <- as.factor(Prudential_Data$Insurance_History_8)
Prudential_Data$Insurance_History_9 <- as.factor(Prudential_Data$Insurance_History_9)

Prudential_Data$Family_Hist_1 <- as.factor(Prudential_Data$Family_Hist_1)

Prudential_Data$Medical_History_3 <- as.factor(Prudential_Data$Medical_History_3)
Prudential_Data$Medical_History_4 <- as.factor(Prudential_Data$Medical_History_4)
Prudential_Data$Medical_History_5 <- as.factor(Prudential_Data$Medical_History_5)
Prudential_Data$Medical_History_6 <- as.factor(Prudential_Data$Medical_History_6)
Prudential_Data$Medical_History_7 <- as.factor(Prudential_Data$Medical_History_7)
Prudential_Data$Medical_History_8 <- as.factor(Prudential_Data$Medical_History_8)
Prudential_Data$Medical_History_9 <- as.factor(Prudential_Data$Medical_History_9)
Prudential_Data$Medical_History_11 <- as.factor(Prudential_Data$Medical_History_11)
Prudential_Data$Medical_History_12 <- as.factor(Prudential_Data$Medical_History_12)
Prudential_Data$Medical_History_13 <- as.factor(Prudential_Data$Medical_History_13)
Prudential_Data$Medical_History_14 <- as.factor(Prudential_Data$Medical_History_14)
Prudential_Data$Medical_History_16 <- as.factor(Prudential_Data$Medical_History_16)
Prudential_Data$Medical_History_17 <- as.factor(Prudential_Data$Medical_History_17)
Prudential_Data$Medical_History_18 <- as.factor(Prudential_Data$Medical_History_18)
Prudential_Data$Medical_History_19 <- as.factor(Prudential_Data$Medical_History_19)
Prudential_Data$Medical_History_20 <- as.factor(Prudential_Data$Medical_History_20)
Prudential_Data$Medical_History_21 <- as.factor(Prudential_Data$Medical_History_21)
Prudential_Data$Medical_History_22 <- as.factor(Prudential_Data$Medical_History_22)
Prudential_Data$Medical_History_23 <- as.factor(Prudential_Data$Medical_History_23)
Prudential_Data$Medical_History_25 <- as.factor(Prudential_Data$Medical_History_25)
Prudential_Data$Medical_History_26 <- as.factor(Prudential_Data$Medical_History_26)
Prudential_Data$Medical_History_27 <- as.factor(Prudential_Data$Medical_History_27)
Prudential_Data$Medical_History_28 <- as.factor(Prudential_Data$Medical_History_28)
Prudential_Data$Medical_History_29 <- as.factor(Prudential_Data$Medical_History_29)
Prudential_Data$Medical_History_30 <- as.factor(Prudential_Data$Medical_History_30)
Prudential_Data$Medical_History_31 <- as.factor(Prudential_Data$Medical_History_31)
Prudential_Data$Medical_History_33 <- as.factor(Prudential_Data$Medical_History_33)
Prudential_Data$Medical_History_34 <- as.factor(Prudential_Data$Medical_History_34)
Prudential_Data$Medical_History_35 <- as.factor(Prudential_Data$Medical_History_35)
Prudential_Data$Medical_History_36 <- as.factor(Prudential_Data$Medical_History_36)
Prudential_Data$Medical_History_37 <- as.factor(Prudential_Data$Medical_History_37)
Prudential_Data$Medical_History_38 <- as.factor(Prudential_Data$Medical_History_38)
Prudential_Data$Medical_History_39 <- as.factor(Prudential_Data$Medical_History_39)
Prudential_Data$Medical_History_40 <- as.factor(Prudential_Data$Medical_History_40)
Prudential_Data$Medical_History_41 <- as.factor(Prudential_Data$Medical_History_41)

# Making sure the data types of the categorical columns have changed to Factor with n levels
str(Prudential_Data)

# Creating a function to convert each level of every categorical column to a separate column (converting the categorical columns into dummy variables using 1 to C method)
convert.fun <- function(Prudential_Data, Attribute){
  for(level in unique(Prudential_Data[[Attribute]])){
    Prudential_Data[paste(Attribute,seq = "_",level)]<- ifelse(Prudential_Data[[Attribute]] == level,1,0)
  }
  return(subset(Prudential_Data,select = -get(Attribute)))
}

# Calling the function for all categorical columns; If the categorical column has a lot of values, then we deal with it as a discrete column (e.g. Product_Info_3, Employment_Info_2, Medical_History_2)
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_1")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_2")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_5")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_6")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_7")

Prudential_Data <- convert.fun(Prudential_Data, "Employment_Info_3")
Prudential_Data <- convert.fun(Prudential_Data, "Employment_Info_5")

Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_1")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_2")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_3")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_4")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_5")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_6")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_7")

Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_1")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_2")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_3")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_4")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_7")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_8")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_9")
Prudential_Data <- convert.fun(Prudential_Data, "Family_Hist_1")

Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_3")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_4")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_5")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_6")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_7")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_8")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_9")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_11")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_12")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_13")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_14")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_16")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_17")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_18")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_19")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_20")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_21")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_22")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_23")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_25")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_26")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_27")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_28")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_29")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_30")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_31")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_33")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_34")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_35")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_36")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_37")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_38")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_39")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_40")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_41")

# Writing the cleaned data in a new file
Cleaned_Data <- Prudential_Data
write.csv(Cleaned_Data, file = "Cleaned_Data.csv", row.names = FALSE)

#Fitting the linear regression model
fit <-lm(Cleaned_Data$Response ~. ,Cleaned_Data)
summary(fit)

#Assuming that the desired significance is 0.05, we take only variables/columns with p-value less than or equal 0.05 (significance level)
Final_variables <- data.frame(summary(fit)$coef[summary(fit)$coef[,4] <= .05, 4])
# writing the variables/columns names in a new file
write.csv(Final_variables, file = "Final_variables.csv", row.names = TRUE)

#Checking the number of variables/columns left after using p-value
length(summary(fit)$coef[summary(fit)$coef[,4] <= .05, 4])

#Writing the complete final data with all 64 columns plus the response column to a new file
Prudential_final_Data <- subset(Cleaned_Data, select = c(Product_Info_4,Ins_Age,Ht,Wt,BMI,Family_Hist_2,Family_Hist_3,Family_Hist_4,Medical_History_1,Medical_History_2,
                                                         Medical_Keyword_2,Medical_Keyword_3,Medical_Keyword_6,Medical_Keyword_9,Medical_Keyword_11,Medical_Keyword_12,
                                                         Medical_Keyword_15,Medical_Keyword_16,Medical_Keyword_18,Medical_Keyword_19,Medical_Keyword_20,Medical_Keyword_22,Medical_Keyword_25
                                                         ,Medical_Keyword_26,Medical_Keyword_27,Medical_Keyword_29,Medical_Keyword_31,Medical_Keyword_33,Medical_Keyword_34
                                                         ,Medical_Keyword_37,Medical_Keyword_38,Medical_Keyword_39,Medical_Keyword_41,Medical_Keyword_45,Medical_Keyword_46
                                                         ,`Product_Info_2 _ A1`,`Product_Info_2 _ E1`,`Product_Info_2 _ D4`,`Product_Info_2 _ A7`,`Product_Info_2 _ A6`
                                                         ,`Product_Info_2 _ A5`,`Product_Info_2 _ C4`,`Product_Info_2 _ B2`,`Product_Info_2 _ A4`,`Product_Info_6 _ 1`
                                                         ,`Employment_Info_3 _ 1`,`Employment_Info_5 _ 3`,`InsuredInfo_2 _ 2`,`InsuredInfo_5 _ 1`,`InsuredInfo_6 _ 2`,`InsuredInfo_7 _ 1`
                                                         ,`Insurance_History_1 _ 1`,`Insurance_History_3 _ 1`,`Insurance_History_4 _ 1`,`Insurance_History_7 _ 3`
                                                         ,`Medical_History_4 _ 1`,`Medical_History_7 _ 1`,`Medical_History_11 _ 3`,`Medical_History_11 _ 1`
                                                         ,`Medical_History_14 _ 3`,`Medical_History_22 _ 2`,`Medical_History_35 _ 1`,`Medical_History_38 _ 1`,`Medical_History_39 _ 3`, Response ) )
#Making sure all 65 variables are included 
str(Prudential_final_Data)

# Issue: column names from 36 to 64 has spaces in betweens like Product_Info_2 _ A1, Employment_Info_3 _ 1, InsuredInfo_2 _ 2, Insurance_History_1 _ 1, Medical_History_4 _ 1
# This issue will affect the "as.formula" function to paste the fromula Y~x to apply Neural Network 
# "as.formula" does not accept column names has spaces or start with numbers like "25_"
# As a result: I decided to use the "gsub" function to replace all spaces with underscores
names(Prudential_final_Data) <- gsub(" ", "_", names(Prudential_final_Data))

#Making sure column names from 36 to 64 has no spaces
str(Prudential_final_Data)

Prudential_final_Data <- Prudential_final_Data[-(4001:59382),]

# Writing the final data in a new file
write.csv(Prudential_final_Data, file = "Prudential_final_Data.csv", row.names = FALSE)

########################################################################################
# Part2: Neural Network

# encoding/converting each level of "Response" categorical column to a separate column (converting the categorical columns into dummy variables using 1 to C method)
library(nnet)
Prudential_Data <- cbind(Prudential_final_Data[1:64], class.ind(as.factor(Prudential_final_Data$Response)))
# Set labels name
names(Prudential_Data) <- c(names(Prudential_final_Data)[1:64],"Response_1","Response_2", "Response_3", "Response_4", "Response_5", "Response_6", "Response_7", "Response_8")

#OR calling "convert.fun" function above
#Prudential_final_Data <- convert.fun(Prudential_final_Data, "Response")

# Writing the converted data in a new file
write.csv(Prudential_Data, file = "Prudential_Data.csv", row.names = FALSE)


# Scaling the 10 Continuous variables using Min-Max Normalization to be between 0 and 1
scale <- function(x){ (x - min(x))/(max(x) - min(x)) }
Prudential_Data[, 1:10] <- data.frame(lapply(Prudential_Data[, 1:10], scale))

head(Prudential_Data)

# making sure all Continuous variables have been scaled and theire values are between 0 and 1
hist(Prudential_Data$Product_Info_4)
hist(Prudential_Data$Ins_Age)
hist(Prudential_Data$Ht)
hist(Prudential_Data$Wt)
hist(Prudential_Data$BMI)
hist(Prudential_Data$Family_Hist_2)
hist(Prudential_Data$Family_Hist_3)
hist(Prudential_Data$Family_Hist_4)
hist(Prudential_Data$Medical_History_1)
hist(Prudential_Data$Medical_History_2)


#Splitting dataset into train and test datasets
set.seed(2) # we set the seed to make sure that the train and test data will not change every time we divide them by running the sample function

# running sample function to select randomly 80% index numbers of the dataset and use it to divide the dataset into 80% as a train dataset and the remaining 20% as a test dataset
sample_index <- sample(1:nrow(Prudential_Data),round(0.8*nrow(Prudential_Data))) #length(sample_index) should be %80 of the dataset

Prudential_train <- Prudential_Data[sample_index,] #80% of dataset is train data
Prudential_test <- Prudential_Data[-sample_index,] #20% of dataset is test data

#Neural Networks
library(neuralnet)
set.seed(3)

#test
#test <- Prudential_train[c(1:36, 65:72)]
#n <- names(test)
n <- names(Prudential_train)
f <- as.formula(paste("Response_1 + Response_2 + Response_3 + Response_4 + Response_5 + Response_6 + Response_7 + Response_8 ~", paste(n[!n %in% c("Response_1", "Response_2", "Response_3", "Response_4", "Response_5", "Response_6", "Response_7", "Response_8")], collapse = " + ")))
#Fitting Neural Network model on train dataset
nn <- neuralnet(f,data=Prudential_train,hidden=c(27,15),linear.output=FALSE, act.fct = "logistic", err.fct ="sse", lifesign = "minimal", stepmax=1e6) # 'err.fct' automatically set to sum of squared error (sse), because the response is not binary

# plot the neural network 
plot(nn)

# Compute predictions
# For multi-class classification, the typical approach is to have n output neurons in the final layer. They represent the different classes. In the end, the neuron which has the highest prediction 'wins' and that class is predicted.
pr.nn <- compute(nn,Prudential_test[,1:64])
# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)
#test:compare 1st row of actual with the 1st predicted value above
head(Prudential_test[1,])


#Computing accuracy
actual_values <- max.col(Prudential_test[, 65:72]) # Find the maximum position for each row of a matrix.
predicted_values <- max.col(pr.nn_) # Find the maximum position for each row of a matrix.
Accuracy <- mean(predicted_values == actual_values)
Accuracy 
#Computing Misclassification error
Misclass_error <- 1-mean(predicted_values == actual_values)
Misclass_error
