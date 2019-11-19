#Load the data
TrainData <- read.table("C:\\Users\\ivyzh\\Desktop\\Machine Learning\\HW1\\GradedHW1-Train-Data.csv",
                        header=T,sep=",",stringsAsFactors = F,na.strings="")

ValData <- read.table("C:\\Users\\ivyzh\\Desktop\\Machine Learning\\HW1\\GradedHW1-Validation-Data.csv",
                      header=T,sep=",",stringsAsFactors = F,na.strings="")

TestData <- read.table("C:\\Users\\ivyzh\\Desktop\\Machine Learning\\HW1\\GradedHW1-Test-Data.csv",
                       header=T,sep=",",stringsAsFactors = F,na.strings="")

# set the x variables
x <- c('Lot.Area',
       'Total.Bsmt.SF',
       'Gr.Liv.Area',
       'Full.Bath',
       'Bedroom.AbvGr','Year.Built','SalePrice')

# compute the Building Age (in years) as of 2010
TrainData <- TrainData[,x]
TrainData$'Building.Age' <- 2010 -  TrainData$Year.Built
TrainData$Year.Built <- NULL

ValData <- ValData[,x]
ValData$'Building.Age' <- 2010 -  ValData$Year.Built
ValData$Year.Built <- NULL

TestData <- TestData[,x]
TestData$'Building.Age' <- 2010 -  TestData$Year.Built
TestData$Year.Built <- NULL

# examine the individual variables (both the x's and the y)
sapply(TrainData,function(x) any(is.na(x)))
sapply(ValData,function(x) any(is.na(x)))
sapply(TestData,function(x) any(is.na(x)))

# find the variables show strong right skewness with upper tail outliers
par(mfrow = c(3, 3))

for(i in names(TrainData)) {
  boxplot(TrainData[,i], xlab  = i)
}

# remove the NA in validation
ValData <- ValData[-which(is.na(ValData$Total.Bsmt.SF)),]


# find the K without transforming
library(FNN)
ks <- 1:40

rmse <- c()

for(k in ks) {
  fit=knn.reg(train = TrainData[,-6] , test =ValData[,-6]  , y=TrainData[,6] , k = k)$pred
  rmse[k] <- sqrt(mean((fit - ValData[,6])^2))
}

plot(ks,rmse, xlab = "K", ylab = "RMSE", type = "o",main = "Plot of RMSE and K")

match(min(rmse),rmse)
min(rmse)

fit=knn.reg(train = TrainData[,-6] , test =TestData[,-6]  , y=TrainData[,6] , k = 12)$pred
print(sqrt(mean((fit - TestData[,6])^2)))



# Standarlized

for(i in 1:7){
  mu <- mean(TrainData[,i])
  sigma <- sd(TrainData[,i])
  sca_Train[,i] <- (TrainData[,i]-mu)/sigma
  sca_Test[,i] <- (TestData[,i]-mu)/sigma
  sca_Val[,i] <- (ValData[,i]-mu)/sigma
}
sca_Train[,6] = TrainData[,6]
sca_Test[,6] = TestData[,6]
sca_Val[,6] = ValData[,6]

sca_rmse <- c()

for(k in ks) {
  fit=knn.reg(train = sca_Train[,-6] , test =sca_Val[,-6]  , y = sca_Train[,6] , k = k)$pred
  sca_rmse[k] <- sqrt(mean((fit - ValData[,6])^2))
}

plot(ks,sca_rmse, xlab = "K", ylab = "Standarlized RMSE", type = "o",main = "Plot of Standardized RMSE and K")

sca_rmse[1]
sca_rmse[20]

match(min(sca_rmse),sca_rmse)
min(sca_rmse)

fit=knn.reg(train = sca_Train[,-6] , test = sca_Test[,-6]  , y=sca_Train[,6] , k = 12)$pred
print(sqrt(mean((fit - TestData[,6])^2)))


# Transformation
log_Train[,c(1,2,3,6)] = log(TrainData[,c(1,2,3,6)]+1/3)
log_Train[,4:5] = TrainData[,4:5]
log_Train[,7] = sqrt(TrainData[,7])
log_Test[,c(1,2,3,6)] = log(TestData[,c(1,2,3,6)]+1/3)
log_Test[,4:5] = TestData[,4:5]
log_Test[,7] = sqrt(TestData[,7])
log_Val[,c(1,2,3,6)] = log(ValData[,c(1,2,3,6)]+1/3)
log_Val[,4:5] = ValData[,4:5]
log_Val[,7] = sqrt(ValData[,7])

log_rmse = numeric(40)

for(k in ks) {
  fit=knn.reg(train = log_Train[,-6] ,test = log_Val[,-6] , y = log_Train[,6] , k = k)$pred
  log_rmse[k] <- sqrt(mean((fit - log_Val[,6])^2))
}

log_rmse
plot(ks,log_rmse, xlab = "K", ylab = "Transformed RMSE", type = "o",main = "Plot of Transformed RMSE and K")

match(min(log_rmse),log_rmse)
min(log_rmse)


fit=knn.reg(train = log_Train[,-6] , test =log_Test[,-6]  , y= log_Train[,6] , k = 7)$pred
print(sqrt(mean((fit - log_Test[,6])^2)))
print(sqrt(mean((exp(fit) - exp(log_Test[,6]))^2)))

# Standarlized Transformation
sf_Train = TrainData
sf_Test = TestData
sf_Val = ValData

for(i in 1:7){
  mu <- mean(log_Train[,i])
  sigma <- sd(log_Train[,i])
  sf_Train[,i] <- (log_Train[,i]-mu)/sigma
  sf_Test[,i] <- (log_Test[,i]-mu)/sigma
  sf_Val[,i] <- (log_Val[,i]-mu)/sigma
}
sf_Train[,6] = log_Train[,6]
sf_Test[,6] = log_Test[,6]
sf_Val[,6] = log_Val[,6]
sf_rmse <- c()

for(k in ks) {
  fit=knn.reg(train = sf_Train[,-6] , test =sf_Val[,-6]  , y=sf_Train[,6] , k = k)$pred
  sf_rmse[k] <- sqrt(mean((fit - log_Val[,6])^2))
}

plot(ks,sf_rmse, xlab = "K", ylab = "Standarlized Transformated RMSE", type = "o",main = "Plot of Standardized Transformed RMSE and K")

match(min(sf_rmse),sf_rmse)
min(sf_rmse)

fit=knn.reg(train = sf_Train[,-6] , test =sf_Test[,-6]  , y= sf_Train[,6] , k = 10)$pred
print(sqrt(mean((fit - log_Test[,6])^2)))
print(sqrt(mean((exp(fit) - exp(log_Test[,6]))^2)))


