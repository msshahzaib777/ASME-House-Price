library(readr)
library(zoo)
library(tidyverse)
library(reshape2)

train <- read_csv("train.csv")
columns <- read_csv("columns.txt", col_names = FALSE)

train_sub <- train[ , which(names(train) %in% columns$X1)]

summary(train_sub)

train_sub$OverallQual <- as.factor(train_sub$OverallQual)
train_sub$OverallCond <- as.factor(train_sub$OverallCond)

drop = list()
for(i in columns$X1){
  if(class(train_sub[[i]]) == "character"){
    train_sub[i] = as.factor(train_sub[[i]])
    if(sum(is.na(train_sub[[i]])) > dim(train_sub)[1] * 0.4){
      drop = c(drop, i)
    }
  }
  else{
    if((sum(is.na(train_sub[i])) + sum(train_sub[i] == 
                                       0, na.rm=TRUE)) > dim(train_sub)[1] * 0.4){
      drop = c(drop, i)
    }
  }
}
train_sub <- train_sub[, !(names(train_sub) %in% drop)]

summary(train_sub)

train_sub <- train_sub[, !(names(train_sub) %in% c("BsmtCond", "ExterCond", 
                                                   "GarageQual", "GarageCond"))]

train_sub$LotShape[train_sub$LotShape == "IR3"] <- "IR2"

train_sub$HouseStyle[train_sub$HouseStyle %in% c("SFoyer", "SLvl")] <- "SLvl"
train_sub$HouseStyle[train_sub$HouseStyle %in% c("1.5Fin", "1.5Unf")] <- "1.5Fin"
train_sub$HouseStyle[train_sub$HouseStyle %in% c("2.5Fin", "2.5Unf")] <- "2.5Fin"

train_sub$OverallQual[train_sub$OverallQual %in% c("4", "3", "2")] <- "1"
train_sub$OverallQual[train_sub$OverallQual  %in% c("7", "8")] <- "4"
train_sub$OverallQual[train_sub$OverallQual  %in% c("5")] <- "2"
train_sub$OverallQual[train_sub$OverallQual  %in% c("6")] <- "3"
train_sub$OverallQual[train_sub$OverallQual %in% c("10", "9")] <- "5"

train_sub$OverallCond[train_sub$OverallCond %in% c("2", "3", "4")] <- "1"
train_sub$OverallCond[train_sub$OverallCond %in% c("5")] <- "2"
train_sub$OverallCond[train_sub$OverallCond %in% c("6")] <- "3"
train_sub$OverallCond[train_sub$OverallCond %in% c("7")] <- "4"
train_sub$OverallCond[train_sub$OverallCond %in% c("10", "9", "8")] <- "5"

train_sub$ExterQual[train_sub$ExterQual == "Fa"] <- "TA"
train_sub$ExterQual[train_sub$ExterQual == "Ex"] <- "Gd"

train_sub$HeatingQC[train_sub$HeatingQC %in% c("Po", "Fa")] <- "TA"

train_sub$SaleType[train_sub$SaleType %in% c("ConLD", "ConLI", 
                                             "ConLw", "Con", "Oth")] <- "COD"
train_sub$SaleType[train_sub$SaleType %in% c("CWD")] <- "WD"

train_sub = droplevels.data.frame(train_sub)

summary(train_sub)


train_sub$Age <- 2020 - train_sub$YearBuilt
train_sub$GarageAge <- 2020 - train_sub$GarageYrBlt
train_sub$Sold <- ((2020- train_sub$YrSold)*12 + train_sub$MoSold)
train_sub$LastRemodAdd <- 2020 - train_sub$YearRemodAdd 


train_sub <- train_sub[, !(names(train_sub) %in% c("YearBuilt", "GarageYrBlt", 
                                                   "MoSold", "YrSold", "YearRemodAdd"))] 


train_sub$LotFrontage[is.na(train_sub$LotFrontage)] <- mean(train_sub$LotFrontage, 
                                                            na.rm = TRUE)
train_sub$GarageAge[is.na(train_sub$GarageAge)] <- -1
train_sub$GarageArea[is.na(train_sub$GarageArea)] <- 0
train_sub$BsmtExposure[is.na(train_sub$BsmtExposure)] <- "No"
train_sub$HeatingQC[is.na(train_sub$HeatingQC)] <- "TA"



NewLevels <- c(levels(train_sub$GarageFinish), "No")
train_sub$GarageFinish  <- factor(train_sub$GarageFinish, NewLevels)
train_sub$GarageFinish[is.na(train_sub$GarageFinish)] <- "No"


summary(train_sub)


train_subN <- train_sub
for(i in names(train_subN)){
    if(class(train_sub[[i]]) == "factor"){
      train_subN[i] = as.numeric(train_subN[[i]])
    }
}

corelations <- cor(train_subN)
melted_cormat <- melt(corelations)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme_minimal() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "black", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


VarSelected <- c("OverallQual", "OverallCond", "TotalBsmtSF", "1stFlrSF", "GarageArea", 
                    "Age", "LastRemodAdd", "ExterQual", "HeatingQC", "GarageFinish", 
                 "SalePrice")
train_subN <- train_subN[, (names(train_subN) %in% VarSelected)]

corelations2 <- cor(train_subN)
melted_cormat2 <- melt(corelations2)
ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme_minimal() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "WHITE", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

VarDrop <- c("TotalBsmtSF", "ExterQual", "LastRemodAdd", "GarageFinish", "OverallCond")
train_subN <- train_subN[, !(names(train_subN) %in% VarDrop)]

corelations2 <- cor(train_subN)
melted_cormat2 <- melt(corelations2)
ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme_minimal() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "WHITE", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(train_subN, aes(x=SalePrice, y=GarageArea)) + 
  geom_point(aes(color=Age))

ggplot(train_subN, aes(x=SalePrice, y=`1stFlrSF`)) + 
  geom_point(aes(color=Age))

ggplot(train_subN, aes(x=`1stFlrSF`, y=GarageArea)) + 
  geom_point(aes(color=Age))

ggplot(train_subN, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(aes(color=Age))

ggplot(train_subN, aes(x=HeatingQC, y=SalePrice)) + 
  geom_point(aes(color=`1stFlrSF`))

outlier_Detector <- function(data){
  outliers <- list()
  for(i in c("1stFlrSF", "GarageArea", "Age", "SalePrice")){
    outliers[[i]] <-  boxplot(data[[i]], plot = F)$stats
  }
  return(outliers)
}

remove_outlier <- function(data, outliers){
  for(i in names(outliers)){
    data[[i]][data[i] > outliers[[i]][5]] <- as.numeric(outliers[[i]][4])
    data[[i]][data[i] < outliers[[i]][1]] <- as.numeric(outliers[[i]][2])
  }
  return(data)
}

read_train <- function(name){
  data <- read_csv(name)
  columns <- c("HeatingQC", "OverallQual", "1stFlrSF", "GarageArea", 
               "SalePrice", "YearBuilt")
  data <- data[ , which(names(data) %in% columns)]
  
  data$OverallQual <- as.factor(data$OverallQual)
  data$HeatingQC <- as.factor(data$HeatingQC)
  
  data$OverallQual[data$OverallQual %in% c("4", "3", "2")] <- "1"
  data$OverallQual[data$OverallQual  %in% c("7", "8")] <- "4"
  data$OverallQual[data$OverallQual  %in% c("5")] <- "2"
  data$OverallQual[data$OverallQual  %in% c("6")] <- "3"
  data$OverallQual[data$OverallQual %in% c("10", "9")] <- "5"
  
  data$HeatingQC[data$HeatingQC %in% c("Po", "Fa")] <- "TA"
  
  data$Age <- 2020 - data$YearBuilt
  data <- data[, which(!names(data) %in% c("YearBuilt"))]
  
  data$OverallQual <- as.numeric(data$OverallQual)
  data$HeatingQC <- as.numeric(data$HeatingQC)
  
  outliers <- outlier_Detector(data)
  data <- remove_outlier(data, outliers)
  return (data)
}

read_test <- function(name){
  data <- read_csv(name)
  columns <- c("HeatingQC", "OverallQual", "1stFlrSF", "GarageArea", 
               "SalePrice", "YearBuilt")
  data <- data[ , which(names(data) %in% columns)]
  
  data$OverallQual <- as.factor(data$OverallQual)
  data$HeatingQC <- as.factor(data$HeatingQC)
  
  data$OverallQual[data$OverallQual %in% c("4", "3", "2")] <- "1"
  data$OverallQual[data$OverallQual  %in% c("7", "8")] <- "4"
  data$OverallQual[data$OverallQual  %in% c("5")] <- "2"
  data$OverallQual[data$OverallQual  %in% c("6")] <- "3"
  data$OverallQual[data$OverallQual %in% c("10", "9")] <- "5"
  
  data$HeatingQC[data$HeatingQC %in% c("Po", "Fa")] <- "TA"
  
  data$Age <- 2020 - data$YearBuilt
  data <- data[, which(!names(data) %in% c("YearBuilt"))]
  
  data$OverallQual <- as.numeric(data$OverallQual)
  data$HeatingQC <- as.numeric(data$HeatingQC)
  return (data)
}

Train_data <- read_train("train.csv")

boxplot(Train_data$`1stFlrSF`)
boxplot(Train_data$GarageArea)
boxplot(Train_data$Age)
boxplot(Train_data$SalePrice)

ggplot(train_data, aes(x=SalePrice, y=GarageArea)) + 
  geom_point(aes(color=Age))

ggplot(train_data, aes(x=SalePrice, y=`1stFlrSF`)) + 
  geom_point(aes(color=Age))

ggplot(train_data, aes(x=`1stFlrSF`, y=GarageArea)) + 
  geom_point(aes(color=Age))

ggplot(train_data, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(aes(color=Age))

ggplot(train_data, aes(x=HeatingQC, y=SalePrice)) + 
  geom_point(aes(color=`1stFlrSF`))

  
Regression <- function(x, y, var){
  relation <- lm(y~x)
  plot(y,x,col = "blue",main = "Price Vs " + var,
       abline(lm(x~y)),ylab = "Price")
}

predict <- function(model, X){
  y <- as.numeric(coef(model)[1])
  for(i in 2:length(coef(model))){
    y <- y + as.numeric((coef(model)[i]*X[i-1]))
  }
  return(y)
}


Regression(Train_data$SalePrice, Train_data$Age, "Age")
Regression(Train_data$SalePrice, Train_data$`1stFlrSF`, "1stFlrSF")
Regression(Train_data$SalePrice, Train_data$GarageArea, "GarageArea")
Regression(Train_data$SalePrice, Train_data$HeatingQC, "HeatingQC")
Regression(Train_data$SalePrice, Train_data$OverallQual, "OverallQual")


model <- lm(SalePrice~OverallQual+HeatingQC+`1stFlrSF`+GarageArea+Age, data = Train_data)

print(model)

test_data <- read_test("test.csv")

y_hat <- list()
for (i in 1:nrow(test_data)){
  y_hat[i] <- predict(model, as.numeric(test_data[i,]))
}
test_data$SalePrice <- as.numeric(y_hat)

ggplot() + 
  geom_line(data= Train_data, aes(x = Age, y = SalePrice), color = "blue") + 
  geom_line(data= test_data, aes(x = Age, y = SalePrice), color = "red")

ggplot() + 
  geom_line(data= Train_data, aes(x =`1stFlrSF`, y = SalePrice), color = "blue") + 
  geom_line(data= test_data, aes(x = `1stFlrSF`, y = SalePrice), color = "red")

ggplot() + 
  geom_line(data= Train_data, aes(x = GarageArea, y = SalePrice), color = "blue") + 
  geom_line(data= test_data, aes(x = GarageArea, y = SalePrice), color = "red")

ggplot() + 
  geom_line(data= Train_data, aes(x = HeatingQC, y = SalePrice), color = "blue") + 
  geom_line(data= test_data, aes(x = HeatingQC, y = SalePrice), color = "red")

ggplot() + 
  geom_line(data= Train_data, aes(x = OverallQual, y = SalePrice), color = "blue") + 
  geom_line(data= test_data, aes(x = OverallQual, y = SalePrice), color = "red")




