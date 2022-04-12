library(haven)
library(dplyr)
library(xtable)
library(tidyverse)
library(sjlabelled)
library(fastDummies)
library(matrixStats)
library(ivreg)
library(DoubleML)
rm(list=ls())
#df2 <- read_dta("") # directory. 
df <- df2

iv1 <- ivreg(fertility2015 ~ as.factor(percentPeersGang1997) +  as.factor(percentPeersDrugs1997) + as.factor(hardTimes1997)+
               age2015 + ageMBorn1997 + famNetWorth1997 + dadHighestGrade +
               momHighestGrade + as.factor(race) + indexFamRoutines1997 + indexFamRisk1997 + as.factor(divorcedParents) + 
               numSiblings + grossFamInc2015 + as.factor(marStat2015) + as.factor(religion) |college2015| bullied1997, data=df)
iv1Coefs <- summary(iv1, robust=TRUE)$coefficients["college2015", ]


#First stage check 
summary(lm(college2015 ~ bullied1997+ as.factor(percentPeersGang1997) +  as.factor(percentPeersDrugs1997) + as.factor(hardTimes1997)+
               age2015 + ageMBorn1997 + famNetWorth1997 + dadHighestGrade +
               momHighestGrade + as.factor(race) + indexFamRoutines1997 + indexFamRisk1997 + as.factor(divorcedParents) + 
               numSiblings + grossFamInc2015 + as.factor(marStat2015) + as.factor(religion), data=df))
summary(lm(college2015 ~ bullied1997, data=df))




ols1 <- lm(fertility2015 ~ college2015 + as.factor(percentPeersGang1997) +  as.factor(percentPeersDrugs1997) + as.factor(hardTimes1997)+
               age2015 + ageMBorn1997 + famNetWorth1997 + dadHighestGrade +
               momHighestGrade + as.factor(race) + indexFamRoutines1997 + indexFamRisk1997 + as.factor(divorcedParents) + 
               numSiblings + grossFamInc2015 + as.factor(marStat2015) + as.factor(religion), data=df)
ols1Coefs <- summary(ols1, robust=TRUE)$coefficients["college2015", ]



#DDML PLR:
#Change categs to factor dummies to get rid of haven type vars.
#bullied1997
df$bullied1997 <- as.integer(df$bullied1997)
df$bullied1997

#Percent gang
temp <- dummy_cols(df$percentPeersGang1997, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("Percent Gang", get_labels(df$percentPeersGang1997))

df <- df[, -2]
df <- cbind(df, temp)

#Percent Drugs
temp <- dummy_cols(df$percentPeersDrugs1997, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("Percent Drugs", get_labels(df$percentPeersDrugs1997))
df <- df[, -2]
df <- cbind(df, temp)

#Race
temp <- dummy_cols(df$race, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("Race", get_labels(df$race))
df <- df[, -7]
df <- cbind(df, temp)

#Religion
temp <- dummy_cols(df$religion, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("religion", get_labels(df$religion))
df <- df[, -16]
df <- cbind(df, temp)

df<- as.data.frame(df)
df<- remove_all_labels(df)
df <- df[,-1]
#LASSO: 
dfDDML <- double_ml_data_from_data_frame(df= df, y_col="fertility2015", d_cols="college2015")


pliv1 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.cv_glmnet" , ml_m= "regr.cv_glmnet",
                           score="partialling out" , apply_cross_fitting = TRUE )
pliv1$fit()
pliv1$summary()
pliv1coeffs <- pliv1$summary()

#RANDOM FOREST: 
pliv2 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.ranger" , ml_m= "regr.ranger",
                           score="partialling out" , apply_cross_fitting = TRUE )
pliv2$fit()
pliv2$summary()
pliv2coeffs <- pliv2$summary()

#XGBOOST: 
pliv3 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.xgboost" , ml_m= "regr.xgboost",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv3$fit()
pliv3coeffs <- pliv3$summary()


rownames=c("OLS", "2SLS", "DDML: Lasso", "DDML: Random Forest", "DDML: XgBoost")
colnames= c("Theta", "SE", "t stat", "p-value")

regOutput <- data.frame(matrix(ncol=4, nrow=5, dimnames=list(rownames,colnames)))

regOutput[1,] <- ols1Coefs
regOutput[2,] <- iv1Coefs
regOutput[3,] <- pliv1coeffs
regOutput[4,] <- pliv2coeffs
regOutput[5,] <- pliv3coeffs
regOutput
xtable(regOutput)




#--------------------------------------------------------------------------------
df2 <- df2[, -c(1:6)]
colnames(df2)

ols1 <- lm(fertility2015 ~ college2015 + age2015 + dadHighestGrade +
             momHighestGrade + as.factor(race) + indexFamRoutines1997 + indexFamRisk1997 + as.factor(divorcedParents) + 
             numSiblings + grossFamInc2015 + as.factor(marStat2015) + as.factor(religion), data=df2)
ols1Coefs <- summary(ols1, robust=TRUE)$coefficients["college2015", ]

#Race
temp <- dummy_cols(df2$race, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("Race", get_labels(df2$race))
df2 <- df2[, -3]
df2 <- cbind(df2, temp)

#Religion
temp <- dummy_cols(df2$religion, remove_first_dummy = FALSE)[, -1]
colnames(temp) <- paste("religion", get_labels(df2$religion))
df2 <- df2[, -12]
df2 <- cbind(df2, temp)

df2<- as.data.frame(df2)
df2<- remove_all_labels(df2)

#LASSO: 
dfDDML <- double_ml_data_from_data_frame(df= df2, y_col="fertility2015", d_cols="college2015")


pliv1 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.cv_glmnet" , ml_m= "regr.cv_glmnet",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv1$fit()
pliv1$summary()
pliv1coeffs <- pliv1$summary()

#RANDOM FOREST: 
pliv2 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.ranger" , ml_m= "regr.ranger",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv2$fit()
pliv2$summary()
pliv2coeffs <- pliv2$summary()

#XGBOOST: 
pliv3 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.xgboost" , ml_m= "regr.xgboost",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv3$fit()
pliv3coeffs <- pliv3$summary()


rownames=c("OLS", "2SLS", "DDML: Lasso", "DDML: Random Forest", "DDML: XgBoost")
colnames= c("Theta", "SE", "t stat", "p-value")

regOutput <- data.frame(matrix(ncol=4, nrow=5, dimnames=list(rownames,colnames)))

regOutput[1,] <- ols1Coefs
regOutput[3,] <- pliv1coeffs
regOutput[4,] <- pliv2coeffs
regOutput[5,] <- pliv3coeffs
regOutput
xtable(regOutput)



#------------------------------------------- 
#robustness on df2

#first we remove one of indicators to regain nonsingular mat
colnames(df2)
df2<- df2[, -c(19, 15)]

#calc distance
center <- colMeans(df2)
cov <- cov(df2)

df2$mahalScore <- mahalanobis(df2, center, cov)

df2$pvalue <- pchisq(df2$mahalScore, df=(ncol(df2)-1), lower.tail=FALSE)

df2Filtered <- filter(df2, pvalue>=0.01)
cat(dim(df2)[1]-dim(df2Filtered)[1] , "observations were filtered out as outliers")







colnames(df2Filtered)
df2Filtered<- df2Filtered[, -c(18,19)]

#now run everything on df2Filtered: 

ols1 <- lm(fertility2015 ~ college2015 + age2015 + dadHighestGrade +
             momHighestGrade + Race.Black + Race.Hispanic + Race.Mixed.Race..Non.Hispanic. + indexFamRoutines1997 + indexFamRisk1997 + as.factor(divorcedParents) + 
             numSiblings + grossFamInc2015 + as.factor(marStat2015) + religion.Catholic + religion.Baptist + religion.Other.Christian, data=df2Filtered)
ols1Coefs <- summary(ols1, robust=TRUE)$coefficients["college2015", ]


df2Filtered<- as.data.frame(df2Filtered)
df2Filtered<- remove_all_labels(df2Filtered)

#LASSO: 
dfDDML <- double_ml_data_from_data_frame(df= df2Filtered, y_col="fertility2015", d_cols="college2015")


pliv1 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.cv_glmnet" , ml_m= "regr.cv_glmnet",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv1$fit()
pliv1$summary()
pliv1coeffs <- pliv1$summary()

#RANDOM FOREST: 
pliv2 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.ranger" , ml_m= "regr.ranger",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv2$fit()
pliv2$summary()
pliv2coeffs <- pliv2$summary()

#XGBOOST: 
pliv3 <- DoubleMLPLR$new(data=dfDDML, ml_g="regr.xgboost" , ml_m= "regr.xgboost",
                         score="partialling out" , apply_cross_fitting = TRUE )
pliv3$fit()
pliv3coeffs <- pliv3$summary()


rownames=c("OLS", "2SLS", "DDML: Lasso", "DDML: Random Forest", "DDML: XgBoost")
colnames= c("Theta", "SE", "t stat", "p-value")

regOutput <- data.frame(matrix(ncol=4, nrow=5, dimnames=list(rownames,colnames)))

regOutput[1,] <- ols1Coefs
regOutput[3,] <- pliv1coeffs
regOutput[4,] <- pliv2coeffs
regOutput[5,] <- pliv3coeffs
regOutput
xtable(regOutput)


summary(ols1)
nrow(df2)-nrow(df2Filtered)







