rm(list = ls())
#data input
Train_all=read.csv("C:/Users/Amit Bhalerao/Documents/CareerCenter/Kaggle Projects/Zillow/Data/train.csv/train.csv")

#Structure of data
str(Train_all)

#conversion to factors
Train_all$Id = as.factor(Train_all$Id)
Train_all$MSSubClass = as.factor(Train_all$MSSubClass)
Train_all$OverallQual = as.factor(Train_all$OverallQual)
Train_all$OverallCond = as.factor(Train_all$OverallCond)
Train_all$YearBuilt = as.factor(Train_all$YearBuilt)
Train_all$YearRemodAdd = as.factor(Train_all$YearRemodAdd)
Train_all$GarageYrBlt = as.factor(Train_all$GarageYrBlt)
Train_all$MoSold = as.factor(Train_all$MoSold)
Train_all$YrSold = as.factor(Train_all$YrSold)
Train_all$MasVnrType = as.factor(Train_all$MasVnrType)
Train_all$BsmtQual = as.factor(Train_all$BsmtQual)
Train_all$BsmtCond = as.factor(Train_all$BsmtCond)
Train_all$BsmtExposure = as.factor(Train_all$BsmtExposure)
Train_all$BsmtFinType1 = as.factor(Train_all$BsmtFinType1)
Train_all$BsmtFinType2 = as.factor(Train_all$BsmtFinType2)
Train_all$Electrical = as.factor(Train_all$Electrical)
Train_all$FireplaceQu = as.factor(Train_all$FireplaceQu)
Train_all$GarageType = as.factor(Train_all$GarageType)
Train_all$GarageYrBlt = as.factor(Train_all$GarageYrBlt)
Train_all$GarageFinish = as.factor(Train_all$GarageFinish)
Train_all$GarageQual = as.factor(Train_all$GarageQual)
Train_all$GarageCond = as.factor(Train_all$GarageCond)

#counting missing values
miss.train = as.data.frame(colSums(sapply(Train_all,is.na))/nrow(Train_all)*100)

#replace na with mean
Train_all$LotFrontage[is.na(Train_all$LotFrontage)] = mean(Train_all$LotFrontage, na.rm = T)
sum(is.na(Train_all$LotFrontage))

Train_all$MasVnrArea[is.na(Train_all$MasVnrArea)] = mean(Train_all$MasVnrArea, na.rm = T)
sum(is.na(Train_all$MasVnrArea))

#replacing na with none
Train_all$FireplaceQu = ifelse(is.na(Train_all$FireplaceQu), "None", Train_all$FireplaceQu)
Train_all$GarageType = ifelse(is.na(Train_all$GarageType), "None", Train_all$GarageType)
Train_all$GarageYrBlt = ifelse(is.na(Train_all$GarageYrBlt), "None", Train_all$GarageYrBlt)
Train_all$GarageFinish = ifelse(is.na(Train_all$GarageFinish), "None", Train_all$GarageFinish)
Train_all$GarageQual = ifelse(is.na(Train_all$GarageQual), "None", Train_all$GarageQual)
Train_all$GarageCond = ifelse(is.na(Train_all$GarageCond), "None", Train_all$GarageCond)
Train_all$BsmtQual = ifelse(is.na(Train_all$BsmtQual), "None", Train_all$BsmtQual)
Train_all$BsmtCond = ifelse(is.na(Train_all$BsmtCond), "None", Train_all$BsmtCond)
Train_all$BsmtExposure = ifelse(is.na(Train_all$BsmtExposure), "None", Train_all$BsmtExposure)
Train_all$BsmtFinType1 = ifelse(is.na(Train_all$BsmtFinType1), "None", Train_all$BsmtFinType1)
Train_all$BsmtFinType2 = ifelse(is.na(Train_all$BsmtFinType2), "None", Train_all$BsmtFinType2)
Train_all$MasVnrType = ifelse(is.na(Train_all$MasVnrType), "None", Train_all$MasVnrType)
Train_all$Electrical = ifelse(is.na(Train_all$Electrical), "None", Train_all$Electrical)

#df after removing variables with very high miss%
Train2 = subset(Train_all, select = -c(PoolQC,MiscFeature,Alley,Fence))
sum(is.na(Train2))

#calculating sd values
sd.Train2= as.data.frame(apply(Train2, 2, sd))
sd2 <- sd.Train2
options("scipen"=100, "digits"=4)
sd2 = as.data.frame(sd2)
View(sd2)

nums = sapply(Train2, is.numeric)
Train2_q = Train2[,nums]

cor.Train = as.data.frame(cor(Train2_q))
cor.Train[abs(cor.Train) < 0.6] <- NA

#df after high corr variables and ID removal
Train3 = subset(Train2, select = -c(Id, GarageCars))

#first model
Model1 = lm(SalePrice~.,data = Train3)
summary(Model1)

str(Train3)
#df with only cat variables
fac = sapply(Train3, is.factor)
Train3_c = Train3[fac]

#df with binary variables
Train3_cb = as.data.frame(model.matrix(~ . + 0, data=Train3_c, contrasts.arg = lapply(Train3_c, contrasts, contrasts=FALSE)))

#df with freq% of all cat vars
freq.train3cb = as.data.frame(100*apply(Train3_cb,2,sum)/nrow(Train3_cb))
freq.train3cb = cbind(names(Train3_cb),freq.train3cb)

names(freq.train3cb)[1] = "cols"
names(freq.train3cb)[2] = "freq%"

freq.train3cb2 = subset(freq.train3cb, freq.train3cb$`freq%`>20 & freq.train3cb$`freq%`<80)

#df with good freq binary values
Train3_cb2 = subset(Train3_cb, select = c(freq.train3cb2[,1]))

nums2 = sapply(Train3, is.numeric)
Train3_q = Train3[,nums2]

#df with train5 and binary variables
Train4 = cbind(Train3_q,Train3_cb2)

cor.Train4 = as.data.frame(cor(Train4))
cor.Train4[abs(cor.Train4) < 0.75] <- NA

Train4 = subset(Train4, select = -c(TotalBsmtSF,GrLivArea))

#second model after cleaning data
Model2 = lm(SalePrice~.,data = Train4)
summary(Model2)

#df with standard values
Train4.st = as.data.frame(scale(Train4))

#Model three with standard values
Model2.st = lm(SalePrice ~ .,data = Train4.st)
summary(Model2.st)

#df with selected variables based on business logic
Train_sel = subset(Train4, select = c(BedroomAbvGr
                                      ,BsmtCond
                                      ,Electrical
                                      ,EnclosedPorch
                                      ,FullBath
                                      ,GarageArea
                                      ,GarageCond
                                      ,LotArea
                                      ,TotRmsAbvGrd
                                      ,SalePrice))

Model3 = lm(SalePrice~.,data = Train_sel)
summary(Model3)

mape1 = mean(abs((Train3$SalePrice-Model1$fitted.values)/Train3$SalePrice)*100)
mape2 = mean(abs((Train4$SalePrice-Model2$fitted.values)/Train4$SalePrice)*100)
mape3 = mean(abs((Train_sel$SalePrice-Model3$fitted.values)/Train_sel$SalePrice)*100)

mape1
mape2
mape3

library(olsrr)
ols_step_forward(Model2)
ols_step_forward(Model3)
