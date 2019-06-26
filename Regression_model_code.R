### Pesonalisation L0 CoE 
# 1. Regression 

# Loading the required libraries
library(dummies)

#loading the data 
reg_data <- read.csv("Regression_data.csv",stringsAsFactors = T,strip.white = T,na.strings = c("","NA","N/A","?"),
                     header = T)


#Q1.	Before you start modeling, do you want to remove any variables? If so, why?

## Looking at the structure of the data
str(reg_data) 

## Removing categorical variables which denotes ID's & name & multiple level of sub-granularity
## Checking level frequencies for categorical variables & removing variables with very high single level frequency & no variablility

cat_data<-reg_data[sapply(reg_data,class)%in%c("character","factor")]
for(j in 1:ncol(cat_data))
{
  print(names(cat_data[j]))
  print(table(cat_data[,j]))
}

colnames(reg_data)
reg_data_v1 <- reg_data[,which(!colnames(reg_data)%in% c("Sales_Country","Partner_Name","PRODUCT_ID",
                                                         "SO_NUMBER","POS_TRANSACTION_ID","PF",
                                                         "Volume_Biz","FISCAL_YEAR_QUARTER_NUMBER_INT",
                                                         "L3_TECHNOLOGY_MKT_SEGMENT_NAME"))]


# observations:
# 1. Sales_Country                  : No variation in the data.Only 1 level i.e "AU"
# 4. Partner_Name                   : 1336 levels
# 5. PRODUCT_ID                     : 1552 different product ID's
# 6. SO_NUMBER                      : its the sales order number,will work as an ID
# 7. POS_TRANSACTION_ID             : it is also an ID.
# 8. FISCAL_YEAR_QUARTER_NUMBER_INT : it is a date
# 11.PF                             : 194 levels. Too granular
# 14.L3_TECHNOLOGY_MKT_SEGMENT_NAME : 77 levels of data.
# 18.Volume_Biz                     : No variation in the data.Only 1 level i.e "Y"

## Removing the above columns due to lack of variations and subgranularity.

## checking whether there is any missing values in the data

colSums(is.na(reg_data_v1)) ## ListPrice and PROMOTION_1_CD has missing values. 

## percentage of missing values in each column

## missing value interpretation
check<-list(list())
for(i in 1:ncol(reg_data_v1))
{
  check[[i]]<-ceiling((sum(is.na(reg_data_v1[,i])|reg_data_v1[,i]=="NaN")/length(reg_data_v1[,i]))*100)
}
check_set<-data.frame(Variable_Name=names(reg_data_v1),Missing_Value_Percentage=unlist(check))

# PROMOTION_1_CD has 33% missing values.
# One might remove this variable from the model but I am imputing this with the mode value.

## Imputing missing values
MODE <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  return(xmode)
}

reg_data_v1[is.na(reg_data_v1$PROMOTION_1_CD)==TRUE,
            which(colnames(reg_data_v1)=="PROMOTION_1_CD")] = MODE(reg_data_v1[,which(colnames(reg_data_v1)=="PROMOTION_1_CD")],
                                                                   na.rm = TRUE)

reg_data_v1[is.na(reg_data_v1$ListPrice)==TRUE,
            which(colnames(reg_data_v1)=="ListPrice")] = mean(reg_data_v1[,which(colnames(reg_data_v1)=="ListPrice")],
                                                              na.rm = TRUE)

colSums(is.na(reg_data_v1))

## checking the correlation between numeric variables
num_col = sapply(reg_data_v1[,-20], is.numeric)
data_numeric = reg_data_v1[,num_col]
correlations <- as.data.frame(cor(na.omit(data_numeric, with = FALSE)))


## New set of categorica variables
cat_data<-reg_data_v1[sapply(reg_data_v1,class)%in%c("character","factor")]

## Dummy var for cat variables

dum_data<-dummy.data.frame(cat_data)
num_data<-reg_data_v1[sapply(reg_data_v1,class)%in%c("numeric","integer")]
model_data<-cbind(num_data,dum_data)

#### Fitting Linear Regression

fit<-lm(EXTENDED_QUANTITY ~.,data=model_data)
summary(fit)

#Rerunning regression omitting levels with NA coeff

model_data1<-model_data[names(model_data)%in%names(fit$coefficients[is.na(fit$coefficients)==F])]
model_data1<-cbind(model_data1,EXTENDED_QUANTITY=model_data$EXTENDED_QUANTITY)

### Splitting the data into train and test

id<-sample(nrow(model_data1), 0.8*nrow(model_data1))
training<-model_data1[id,]
test <- model_data1[-id,]
fit1<-lm(EXTENDED_QUANTITY~.,data=training)
summary(fit1)

### Checking assumptions of linear regression

# Assumptions of Linear Regression
# 1.Normality - For fixed values of the independent variables, the dependent variable is normally distributed.
 
# 2.Independence -The Yi values are independent of each other.
 
# 3.Linearity - The dependent variable is linearly related to the independent variables.
 
# 4.Homoscedasticity - The variance of the dependent variable doesn't vary with the levels of the 
#                      independent variables. We could call this constant variance


### Normailty check ###
#There are several methods for normality test such as 
#Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk's test.
#Null hypothesis "sample distribution is normal". 
#If the test is significant, the distribution is non-normal.

### Check for Multicollinearity ###
library(car)
vif(fit1)  # There is multicollinearity in the data

### Plotting graph###
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(fit1)


### QQ plot ###

par(mfrow=c(1,1))
qqPlot(fit1, labels=row.names(model_data1), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
# The Normality condition is not satisfied as Q-Q Plot doesn't seems to be a straight line along 45degree.
# Heavy tailed distribution.


### Autocorrelation test ###
durbinWatsonTest(fit1)
# Null hypothesis is there is no correlation among the residuals
# The insignificant p-value (p=0.548) suggests there is no correlation among residuals


### Homoscedasticity - Breusch-Pegan Test ###
library(lmtest)
bptest(fit1)
#studentized Breusch-Pagan test
#data:  fit1
#BP = 1140.7, df = 52, p-value < 2.2e-16      Hence we reject the null hypothesis.
# There is heteroscedasticity in the data

plot(fit1)  # This gives 4 plots.

######### Rectification/ fine tuning the model #######
# Box-cox transformation is a mathematical transformation of the variable to make it approximate to a normal distribution. 
# Often, doing a box-cox transformation of the Y variable solves the issue,
# which is exactly what I am going to do now.

library(caret)
y_BC <- BoxCoxTrans(model_data1$EXTENDED_QUANTITY)
print(y_BC)

## BoxCox transformation could not estimate the lambda as the variable is highly skewed. No transformation has been applied.

#### Outlier test ####
outlierTest(fit1)

#### Leverage points #####
cutoff <- 4/(nrow(model_data1)-length(fit1$coefficients)-2)
plot(fit1, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
# There are 3 leverage points that we got from the graph. viz.2122,9250,12980

## Final model data : Removing Outliers and leverage points
model_data_final <- model_data1[-c(2122,9250,5988,11232,12980,19133,5881,10832,7108,17217),]

# Removing the columns which has VIF > 4 : removing multicollinearity
model_data_final <- model_data_final[,-c(4,5,6,8,9,10,11,12,43,44,46)]

## Again splitting into train and test
id<-sample(nrow(model_data1), 0.8*nrow(model_data_final))
training<-model_data_final[id,]
test <- model_data_final[-id,]

final_fit <- lm(EXTENDED_QUANTITY~.,data=training)
summary(final_fit)

par(mfrow=c(2,2))
plot(final_fit)
## Top left plot shows it is a straight line with some slope but there is no curvature. 
# Homoscedasticity is removed.

# Adjusted R-sq : 0.993

