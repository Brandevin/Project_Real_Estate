library(plyr)
library(data.table)
library(hash)
library(ggplot2)
library(reshape2)
library(lattice)
library(grid)
library(gridExtra)
library(ggpubr)
library(scales)
library(tidyr)
library(car)
library(mltools)
library(kableExtra)
library(magrittr)
library(faux)
library(randomForest)
library(caret)
library(xgboost)
library(Matrix)
library(glmnet)
library(e1071)

df_train=read.csv('./Inputs/train.csv')
df_validate=read.csv('./Inputs/test.csv')

skew_calc <- function(series){
                return (sum((series - mean(series))^3)/length(series))/((sum((series - mean(series))^2)/length(series)))^(3/2)
}

common_preprocessor <-function(df,garage_yr_blt_mean,quantile_96_dict,columns_96,quantile_4_dict,columns_04,min_values_dict,columns_min,validate=F) {
                #Replacing cells without value with their meaning, when possible
                # If the reason was because there was no room (Ex: garage), with the properties, create categorical variable "No room_name". If the feature is the area, consider the area as being equal to 0
                df$Alley<-df$Alley %>% replace_na("No Alley")
                df$BsmtExposure<-df$BsmtExposure %>% replace_na("NA")
                df$BsmtFinType1<-df$BsmtFinType1 %>% replace_na("No basement")
                df$BsmtCond<-df$BsmtCond %>% replace_na(0)
                df$BsmtQual<-df$BsmtQual %>% replace_na(0)
                df$GarageCars<-df$GarageCars %>% replace_na(0)
                df$BsmtFinSF1<-df$BsmtFinSF1 %>% replace_na(0)
                df$BsmtUnfSF<-df$BsmtUnfSF %>% replace_na(0)
                df$TotalBsmtSF<-df$TotalBsmtSF %>% replace_na(0)
                df$BsmtFullBath<-df$BsmtFullBath %>% replace_na(0)
                df$KitchenQual<-df$KitchenQual %>% replace_na(0)
                df$BsmtHalfBath<-df$BsmtHalfBath %>% replace_na(0)
                df$GarageArea<-df$GarageArea %>% replace_na(0)
                df$Exterior1st<-df$Exterior1st %>% replace_na("Other")
                df$Exterior2nd<-df$Exterior2nd %>% replace_na("Other")
                df$MSZoning<-df$MSZoning %>% replace_na("Other")
                df$Functional<-df$Functional %>% replace_na("Other")
                df$SaleType<-df$SaleType %>% replace_na("Other")
                
                df$Fence<-df$Fence %>% replace_na("No fence")
                df$FireplaceQu<-df$FireplaceQu %>% replace_na(0)
                df$GarageCond<-df$GarageCond %>% replace_na(0)
                df$GarageFinish<-df$GarageFinish %>% replace_na("No garage")
                df$GarageQual<-df$GarageQual %>% replace_na(0)
                df$GarageType<-df$GarageType %>% replace_na("No garage")
                df$LotFrontage<-df$LotFrontage %>% replace_na(0)
                df$MasVnrArea<-df$MasVnrArea %>% replace_na(0)
                df$MasVnrType<-df$MasVnrType %>% replace_na('None')
                df$Electrical<-df$Electrical %>% replace_na('Other')
                #Dropping features that dont have enough samples distributed in different categories (Some features had only 5 samples that had a different value, for example)
                drop<-c("RoofMatl","BsmtFinType2","BsmtFinSF2","Heating","KitchenAbvGr","EnclosedPorch","ScreenPorch","PoolArea","MiscFeature","PoolQC","MiscVal","Street","Utilities")
                df<-df[,!(names(df) %in% drop)]
                #Substituting Categories by categories that, although less descriptive, are able to generalize more, as there are more samples per category. Either group similar categorizations together, or lump some of them together as "Other".
                df$BedroomAbvGr<-ifelse(df$BedroomAbvGr<1,1,ifelse(df$BedroomAbvGr>5,5,df$BedroomAbvGr))
                df$TotRmsAbvGrd<-ifelse(df$TotRmsAbvGrd<4,4,ifelse(df$TotRmsAbvGrd>10,10,df$TotRmsAbvGrd))
                df$Fireplaces<-ifelse(df$Fireplaces>2,2,df$Fireplaces)
                df$LotShape <- mapvalues(df$LotShape, from=c("IR1", "IR2", "IR3","Reg"), to=c("IR1", "IR2&IR3","IR2&IR3", "Reg"),warn_missing=FALSE)
                df$MSSubClass <- mapvalues(df$MSSubClass, from=c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190), to=c("20","30","Other","Other","50","60","70","70","80","80","90","120","Other","160","Other","190"),warn_missing=FALSE)
                #df$LotConfig <- mapvalues(df$LotConfig, from=c("Corner","CulDSac","FR2","FR3","Inside"), to=c("Corner","CulDSac","FR2&FR3","FR2&FR3","Inside"),warn_missing=FALSE)
                df$Neighborhood <- mapvalues(df$Neighborhood, from=c("Blueste","NPkVill","Veenker"), to=c("Other","Other","Other"),warn_missing=FALSE)
                df$BsmtExposure <- ifelse(df$BsmtExposure=="Gd","Good",df$BsmtExposure)
                df$RoofStyle <- ifelse(df$RoofStyle=="Gable","Gable",ifelse(df$RoofStyle=="Hip","Hip","Other"))
                #df$LandSlope <- ifelse(df$LandSlope=="Gtl","Gtl","Other")
                #df$Foundation <- ifelse(df$Foundation=="Stone","Other",ifelse(df$Foundation=="Wood","Other",ifelse(df$Foundation=="Slab","Other",df$Foundation)))
                df$MSZoning <- ifelse(df$MSZoning=="FV","FV",ifelse(df$MSZoning=="RM","RM",ifelse(df$MSZoning=="RL","RL","Other")))
                df$Electrical <- ifelse(df$Electrical=="SBrkr","SBrkr",ifelse(df$Electrical=="FuseA","FuseA","Other"))
                df$Functional <- ifelse(df$Functional=="Typ","Typ","Other")
                df$GarageType <- ifelse(df$GarageType=="Attchd","Attchd",ifelse(df$GarageType=="BuiltIn","BuiltIn",ifelse(df$GarageType=="Detchd","Detchd","Other")))
                df$SaleType <- ifelse(df$SaleType=="WD","WD",ifelse(df$SaleType=="New","New",ifelse(df$SaleType=="COD","COD","Other")))
                df$SaleCondition <- ifelse(df$SaleCondition=="Normal","Normal",ifelse(df$SaleCondition=="Partial","Abnormal","Other"))
                df$HouseStyle <- ifelse(df$HouseStyle=="1Story","1Story",ifelse(df$HouseStyle=="1.5Fin","1.5Fin",ifelse(df$HouseStyle=="2Story","2Story",ifelse(df$HouseStyle=="SFoyer","SFoyer",ifelse(df$HouseStyle=="SLvl","SLvl","Other")))))
                df$Exterior1st <- ifelse(df$Exterior1st=="HdBoard","HdBoard",ifelse(df$Exterior1st=="MetalSd","MetalSd",ifelse(df$Exterior1st=="VinylSd","VinylSd",ifelse(df$Exterior1st=="Wd Sdng","Wd Sdng",ifelse(df$Exterior1st=="CemntBd","CemntBd",ifelse(df$Exterior1st=="BrkFace","BrkFace",ifelse(df$Exterior1st=="WdShing","WdShing","Other")))))))
                df$Exterior2nd <- ifelse(df$Exterior2nd=="HdBoard","HdBoard",ifelse(df$Exterior2nd=="MetalSd","MetalSd",ifelse(df$Exterior2nd=="VinylSd","VinylSd",ifelse(df$Exterior2nd=="Wd Sdng","Wd Sdng",ifelse(df$Exterior2nd=="CemntBd","CemntBd",ifelse(df$Exterior2nd=="BrkFace","BrkFace",ifelse(df$Exterior1st=="WdShing","WdShing","Other")))))))
                
                df$BsmtFullBath[df$BsmtFullBath > 0 ] <- 1
                df$BsmtHalfBath[df$BsmtHalfBath > 0 ] <- 1
                df$FullBath[df$FullBath == 0 ] <- 1
                #df$HalfBath[df$HalfBath > 1 ] <- 1
                #df$GarageCars[df$GarageCars > 3 ] <- 3
                
                #Create new variables from some of the features, such as condition 1 & condition 2, Exterior1st &Exterior2nd
                
                df$Condition<-ifelse(df$Condition1=="Artery" | df$Condition2=="Artery","Artery",ifelse(df$Condition1=="Feedr"|df$Condition2=="Feedr","Feedr","Other"))
                
                df$Ext_matl_cementbd<-ifelse(df$Exterior1st=="CemntBd" | df$Exterior2nd=="CemntBd",1,0)
                df$Ext_matl_brkface<-ifelse(df$Exterior1st=="BrkFace" | df$Exterior2nd=="BrkFace",1,0)
                df$Ext_matl_hdboard<-ifelse(df$Exterior1st=="HdBoard" | df$Exterior2nd=="HdBoard",1,0)
                df$Ext_matl_metalsd<-ifelse(df$Exterior1st=="MetalSd" | df$Exterior2nd=="MetalSd",1,0)
                df$Ext_matl_vinylsd<-ifelse(df$Exterior1st=="VinylSd" | df$Exterior2nd=="VinylSd",1,0)
                df$Ext_matl_wdsdng<-ifelse(df$Exterior1st=="Wd Sdng" | df$Exterior2nd=="Wd Sdng",1,0)
                df$Ext_matl_wdshing<-ifelse(df$Exterior1st=="WdShing" | df$Exterior2nd=="WdShing",1,0)
                
                #Replace ratings by rating number
                
                df[df=="Ex"]<-5
                df[df=="Gd"]<-4
                df[df=="TA"]<-3
                df[df=="Fa"]<-2
                df[df=="Po"]<-1
                
                #Creating new variables when area is equal to 0
                
                df$Has2ndFlr<-df$X2ndFlrSF>0
                df$HasPorch<-df$X3SsnPorch>0
                df$HaslowQuality_finished<-df$LowQualFinSF>0
                df$HasOpenPorch<-df$OpenPorchSF>0
                
                # Drop columns that were not valid after the creation of the new variables
                
                drop<-c("Condition1","Condition2","Exterior1st","Exterior2nd","X3SsnPorch","LowQualFinSF")
                df<-df[,!(names(df) %in% drop)]
                
                # Trim Outliers. Assign the values above the threshold as being equal to the threshold
                for (col in columns_96){
                                #df[[col]][df[[col]] > quantile_96_dict[[col]] ] <- quantile_96_dict[[col]]
                }
                
                for (col in columns_04){
                                #df[[col]][df[[col]] < quantile_4_dict[[col]] ] <- quantile_4_dict[[col]]
                }
                
                #Substituting values equal to 0 to the minimum value. This way, the leverage of these points is decreased
                for (col in columns_min){
                                min_values_dict[col]<-min(df[df[[col]]>0,col])
                                df[[col]][df[[col]]==0] <-min_values_dict[[col]]
                                
                }
                
                #Replacing nan by mean value
                df$GarageYrBlt<-df$GarageYrBlt %>% replace_na(garage_yr_blt_mean)
                
                df$LotFrontage<-df$LotFrontage %>% replace_na(0)
                if (validate){
                                numberic_feats<-c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","YrSold","ExterQual", "ExterCond","BsmtQual", "BsmtCond","HeatingQC","KitchenQual","FireplaceQu","GarageQual","GarageCond")  
                }
                else{
                                numberic_feats<-c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","YrSold","SalePrice","ExterQual", "ExterCond","BsmtQual", "BsmtCond","HeatingQC","KitchenQual","FireplaceQu","GarageQual","GarageCond")  
                                
                }
                binary_feats<-c("Ext_matl_cementbd","Ext_matl_brkface",  "Ext_matl_hdboard",  "Ext_matl_metalsd", "Ext_matl_vinylsd",  "Ext_matl_wdsdng"  ,"Ext_matl_wdshing","Has2ndFlr", "HasPorch", "HaslowQuality_finished", "HasOpenPorch")
                cat_feats<-c("LandContour","LotConfig","LandSlope", "Neighborhood",  "HouseStyle","RoofStyle", "MasVnrType", "Foundation",  "BsmtExposure","BsmtFinType1", "CentralAir","Electrical", "Functional", "GarageType","GarageFinish","PavedDrive","Fence","SaleType", "SaleCondition","MoSold","Condition")
                df[, numberic_feats] <- sapply(df[, numberic_feats], as.numeric)
                df[, cat_feats] <- sapply(df[, cat_feats], as.factor)
                
                return (df)
}

columns_96<-c("LotFrontage","LotArea","MasVnrArea","BsmtFinSF1","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","GarageArea","WoodDeckSF","OpenPorchSF")
quantile_96_dict<-hash()
for (col in columns_96){
                quantile_96_dict[col]<-quantile(df_train[[col]],0.96,na.rm=TRUE)
}

# Getting 4th percentile of some columns to trim outliers. Values above the 96th percentile for these columns are going to be assigned as equal to the 4th percentile

columns_04<-c("GrLivArea","LotArea","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","YearBuilt","WoodDeckSF")
quantile_4_dict<-hash()
for (col in columns_04){
                quantile_4_dict[col]<-quantile(df_train[df_train[[col]]>0,col],0.04,na.rm=TRUE)
}
#Substituting 0s by the minimum value

min_values_dict<-hash()
columns_min<-c("GarageArea","LotFrontage","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","WoodDeckSF")
for (col in columns_min){
                min_values_dict[col]<-min(df_train[df_train[[col]]>0,col])
}

#Getting mean garage year built value
garage_yr_blt_mean<-mean(df_train$GarageYrBlt,na.rm=TRUE)

df_train<-common_preprocessor(df_train,garage_yr_blt_mean,quantile_96_dict,columns_96,quantile_4_dict,columns_04,min_values_dict,columns_min)

df_validate<-common_preprocessor(df_validate,garage_yr_blt_mean,quantile_96_dict,columns_96,quantile_4_dict,columns_04,min_values_dict,columns_min,validate=T)

numeric_feats<-c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","YrSold","SalePrice","ExterQual", "ExterCond","BsmtQual", "BsmtCond","HeatingQC","KitchenQual","FireplaceQu","GarageQual","GarageCond")  

high_skew<-numeric_feats[sapply(df_train[,numeric_feats], function(x) abs(skew_calc(x)))>2]

for (column in high_skew){
                df_train[,column]<-log1p(df_train[,column])
                if (column!="SalePrice"){
                                df_validate[,column]<-log1p(df_validate[,column])
                }
                }

drop<-c("TotalBsmtSF","GarageArea","TotalRmsAbvGrd","X1stFlrSF")
df_train<-df_train[,!(names(df_train) %in% drop)]
df_validate<-df_validate[,!(names(df_validate) %in% drop)]

x_train<-df_train[,!(names(df_train) %in% c("SalePrice","Id"))]
y_train<-df_train$SalePrice

x_validate<-df_validate[,!(names(df_validate) %in% c("SalePrice","Id"))]
y_validate<-df_validate$SalePrice

parsed_matrix_train<-model.matrix(SalePrice ~ ., data = df_train[,!names(df_train) %in% c("Id")])
parsed_matrix_train<-parsed_matrix_train[,!colnames(parsed_matrix_train) %in% c("BldgTypeDuplex")]
parsed_matrix_validate<-model.matrix(~.,data = df_validate[,!names(df_validate) %in% c("Id")])
parsed_matrix_validate<-parsed_matrix_validate[,!colnames(parsed_matrix_validate) %in% c("BldgTypeDuplex")]


common_cols <- intersect(colnames(parsed_matrix_train), colnames(parsed_matrix_validate))

parsed_matrix_train_common=parsed_matrix_train[,common_cols]
parsed_matrix_validate_common=parsed_matrix_validate[,common_cols]


control <- rfeControl(functions = rfFuncs, # random forest
                         method = "repeatedcv", # repeated cv
                         repeats = 5, # number of repeats
                         number = 10,
                         verbose=F) # number of folds

# Selecting the top 60 variables

result_rfe <- rfe(x = parsed_matrix_train_common, 
                   y = y_train,
                   sizes = c(65),
                   rfeControl = control)
#varimp_data <- data.frame(feature = row.names(varImp(result_rfe)),importance = varImp(result_rfe))

feat_imp_order<-predictors(result_rfe)[1:65]

rfe_selected_vars <- result_rfe$variables %>% 
                dplyr::filter(Variables == 65) %>% 
                dplyr::group_by(var) %>% 
                dplyr::summarise(Overall = mean(Overall)) %>% 
                dplyr::arrange(desc(Overall)) %>% 
                dplyr::pull(var) 
feat_imp_order<-rfe_selected_vars[1:65]

numeric_feats<-c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtUnfSF","X2ndFlrSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","WoodDeckSF","OpenPorchSF","YrSold","SalePrice","ExterQual", "ExterCond","BsmtQual", "BsmtCond","HeatingQC","KitchenQual","FireplaceQu","GarageQual","GarageCond")  
numeric_feats_without_sale_price<-numeric_feats[!numeric_feats %in% c("SalePrice")]

# Centering and scaling numeric features
preProcValues <- preProcess(df_train[,numeric_feats_without_sale_price], method = c("center", "scale"))

df_train[,numeric_feats_without_sale_price]<-predict(preProcValues,df_train[,numeric_feats_without_sale_price])
df_validate[,numeric_feats_without_sale_price]<-predict(preProcValues,df_validate[,numeric_feats_without_sale_price])

#Separating into input and output
x_train<-df_train[,!(names(df_train) %in% c("SalePrice","Id"))]
y_train<-df_train$SalePrice

x_validate<-df_validate[,!(names(df_validate) %in% c("SalePrice","Id"))]

# One of the features is linearly dependent of the others. It was removed
parsed_matrix_train<-model.matrix(SalePrice ~ ., data = df_train[,!names(df_train) %in% c("Id")])
parsed_matrix_validate<-model.matrix( ~ ., data = df_validate[,!names(df_validate) %in% c("Id")])

parsed_matrix_train<-parsed_matrix_train[,!colnames(parsed_matrix_train) %in% c("BldgTypeDuplex")]
parsed_matrix_validate<-parsed_matrix_validate[,!colnames(parsed_matrix_validate) %in% c("BldgTypeDuplex")]

#Keep only common features in both train and test

common_cols <- intersect(colnames(parsed_matrix_train), colnames(parsed_matrix_validate))
parsed_matrix_train_common=parsed_matrix_train[,common_cols]
parsed_matrix_validate_common=parsed_matrix_validate[,common_cols]


#Select top features selected by RFE


parsed_matrix_train_common_top_65=parsed_matrix_train_common[,intersect(feat_imp_order,colnames(parsed_matrix_validate_common))]
parsed_matrix_validate_common_top_65=parsed_matrix_validate_common[,intersect(feat_imp_order,colnames(parsed_matrix_validate_common))]

#################
#Multi Linear Regression

fit_lm<-lm(SalePrice~. -Id-BldgType-MoSold-HasPorch-Alley-FireplaceQu-HeatingQC-BsmtFullBath-BsmtHalfBath-GarageType-GarageFinish-PavedDrive,df_train)
prediction_lm<-predict(fit_lm,df_validate)

#################
# Extreme Gradient Boosting

#Creating hyperparameter grid

hyper_grid_xgb <- expand.grid(max_depth = seq(3, 12, 1),  eta = seq(.0, .5, .025))
xgb_test_rmse <- NULL

#Search grid for best hyperparameters

for (j in 1:nrow(hyper_grid_xgb)) {
                set.seed(124)
                m_xgb_untuned <- xgb.cv(
                                data = parsed_matrix_train_common_top_65,
                                label = y_train,
                                nrounds = 1000,
                                objective = "reg:squarederror",
                                early_stopping_rounds = 3,
                                nfold = 5,
                                max_depth = hyper_grid_xgb$max_depth[j],
                                eta = hyper_grid_xgb$eta[j],
                                verbose=F
                )
                
                xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
                
}

#Best scoring hyperparamters

optimal_par<-hyper_grid_xgb[which.min(xgb_test_rmse), ]

#Using the model with the best performing parameters

m_xgb_tuned <- xgboost(
                data = parsed_matrix_train_common_top_65,
                #data = parsed_matrix_train_common[,feat_imp_order[1:optimal_par$dimension]],
                label = y_train,
                nrounds = 1000,
                objective = "reg:squarederror",
                early_stopping_rounds = 3,
                max_depth =optimal_par$max_depth,
                eta = optimal_par$eta,
                verbose=F
)

prediction_xgb=predict(m_xgb_tuned,parsed_matrix_validate_common_top_65)

#################
# Support vector machines

#Hyperparameter grid

tuneResult <- tune(svm,  train.x=data.frame(parsed_matrix_train_common_top_65),train.y=y_train,
                   ranges = list(epsilon = seq(0.01,0.2,0.01), cost = 2^(-3:4),gamma=2^(-8:-4))
)

#Retraining with best feature

tunedModel <- tuneResult$best.model
prediction_svm <- predict(tunedModel, data.frame(parsed_matrix_validate_common_top_65)) 

prediction_avg<-(prediction_lm+prediction_svm+prediction_xgb)/3

prediction_svm<-exp(prediction_svm)-1
pr_svm<-data.frame(prediction_svm)
pr_svm$Id<-df_validate$Id
pr_svm<-pr_svm %>% dplyr::rename(Id=Id,SalePrice=prediction_svm) %>% dplyr::relocate(Id)
pr_svm$Id<-as.character(pr_svm$Id)
pr_svm$SalePrice<-as.character(pr_svm$SalePrice)

write.csv(pr_svm,"predictions/pred_svm.csv", row.names = FALSE)

prediction_xgb<-exp(prediction_xgb)-1
pr_xgb<-data.frame(prediction_xgb)
pr_xgb$Id<-df_validate$Id
pr_xgb<-pr_xgb %>% dplyr::rename(Id=Id,SalePrice=prediction_xgb) %>% dplyr::relocate(Id)
pr_xgb$Id<-as.character(pr_xgb$Id)
pr_xgb$SalePrice<-as.character(pr_xgb$SalePrice)
write.csv(pr_xgb,"predictions/pred_xgb.csv", row.names = FALSE)

prediction_lm<-exp(prediction_lm)-1
pr_lm<-data.frame(prediction_lm)
pr_lm$Id<-df_validate$Id
pr_lm<-pr_lm %>% dplyr::rename(Id=Id,SalePrice=prediction_lm) %>% dplyr::relocate(Id)
pr_lm$Id<-as.character(pr_lm$Id)
pr_lm$SalePrice<-as.character(pr_lm$SalePrice)
write.csv(pr_lm,"predictions/pred_lm.csv", row.names = FALSE)

prediction_avg<-exp(prediction_avg)-1
pr_avg<-data.frame(prediction_avg)
pr_avg$Id<-df_validate$Id
pr_avg<-pr_avg %>% dplyr::rename(Id=Id,SalePrice=prediction_avg) %>% dplyr::relocate(Id)
pr_avg$Id<-as.character(pr_avg$Id)
pr_avg$SalePrice<-as.character(pr_avg$SalePrice)
write.csv(pr_avg,"predictions/pred_avg.csv", row.names = FALSE)
