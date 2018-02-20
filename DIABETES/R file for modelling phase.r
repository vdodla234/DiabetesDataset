#reading the diabetes csv file.
rm(list=ls())
diabetes <- read.csv(file="C:/Users/Vineeth Adithya/Desktop/diabetes.csv",stringsAsFactors = FALSE)
diabetes
#finding mean of the differnt columns in the dataset i.e. pregnancies, glucose, BP, skinThickness, Insulin, BMI, age
mean_pregnancies <- mean(diabetes$Pregnancies)
mean_pregnancies
mean_glucose <- mean(diabetes$Glucose)
mean_glucose
mean_bp <- mean(diabetes$BloodPressure)
mean_bp
mean_skinThickness <- mean(diabetes$SkinThickness)
mean_skinThickness
mean_Insulin <- mean(diabetes$Insulin)
mean_Insulin
mean_BMI <- mean(diabetes$BMI)
mean_BMI
mean_age <- mean(diabetes$Age)
mean_age
summary(diabetes)
plot(diabetes$Pregnancies,diabetes$Pregnancies)
#dit(diabetes,factor.mode =c("numeric"),edit.row.names = any(row.names(diabetes$Glucose)))
#replacing the missing values in glucose by the mean, as in the dataset there are some values that are 0 which is 
#medically not possible.
for(i in 1:length(diabetes$Glucose)) {
  glucose_value <- diabetes[i,2]
  glucose_value
  if(glucose_value == 0){
    diabetes[i,2]<-121
  }
}
#diabetes$Glucose[1,:]
#iabetes[76,2]
diabetes$Glucose
#ength(diabetes)
#replacing the missing value in blood pressure with the mean
for(i in 1:length(diabetes$BloodPressure)){
  BP_value <- diabetes[i,3]
  BP_value
  if(BP_value == 0) {
    diabetes[i,3] <- 69
  }
}
diabetes$BloodPressure
summary(diabetes)
#knn imputation
#library(VIM)
cases_diabetes<-0
cases_nodiabetes<-0
for(i in 1:length(diabetes$Outcome)){
  outcome_value <- diabetes[i,9]
  if(outcome_value == 0)
    cases_nodiabetes=cases_nodiabetes+1
  else
    cases_diabetes=cases_diabetes+1
}
cases_nodiabetes
cases_diabetes
#setting the values in skin thickness and insulin to NA
diabetes$SkinThickness[diabetes$SkinThickness==0] <- NA
diabetes$Insulin[diabetes$Insulin==0] <- NA
diabetes$BMI[diabetes$BMI==0] <- NA

#min-max normalization
mmnorm.pregnancies <-(diabetes$Pregnancies-
                        min(diabetes$Pregnancies))/(max(diabetes$Pregnancies)-
                                                      min(diabetes$Pregnancies))
mmnorm.pregnancies
diabetes$mmnorm_pregnancies <- mmnorm.pregnancies

mmnorm.diabetespedigreefunction<-(diabetes$DiabetesPedigreeFunction-
                                    min(diabetes$DiabetesPedigreeFunction))/(max(diabetes$DiabetesPedigreeFunction)-
                                                                               min(diabetes$DiabetesPedigreeFunction))
mmnorm.diabetespedigreefunction
diabetes$mmnorm_diabetespedigreefunction<-mmnorm.diabetespedigreefunction

mmnorm.age<-(diabetes$Age-
               min(diabetes$Age))/(max(diabetes$Age)-
                                     min(diabetes$Age))
mmnorm.age
diabetes$mmnorm_age<-mmnorm.age
diabetes$V10<-NULL

install.packages("mice",lib="/downloads/mice")
library(mice)
md.pattern(diabetes)

diabetesData <- mice(diabetes,m=5,maxit=50,meth='pmm',seed=500)
summary(diabetesData) 
# summary(diabetes$Pregnancies)
#diabetes$V10<-NULL
diabetesData$imp$SkinThickness
completedData<-complete(diabetesData,1)
completedData

mmnorm.BloodPressure<-(diabetes$BloodPressure-min(diabetes$BloodPressure))/(max(diabetes$BloodPressure)-
                                                                              min(diabetes$BloodPressure))
mmnorm.BloodPressure
completedData$mmnorm.BloodPressure <- mmnorm.BloodPressure

mmnorm.SkinThickness<-(completedData$SkinThickness-min(completedData$SkinThickness))/(max(completedData$SkinThickness)-
                                                                                        min(completedData$SkinThickness))
mmnorm.SkinThickness
completedData$mmnorm.SkinThickness<-mmnorm.SkinThickness

mmnorm.Insulin<-(completedData$Insulin-min(completedData$Insulin))/(max(completedData$Insulin)-
                                                                      min(completedData$Insulin))
mmnorm.Insulin
completedData$mmnorm.Insulin<-mmnorm.Insulin

mmnorm.BMI<-(completedData$BMI-min(completedData$BMI))/(max(completedData$BMI)-
                                                          min(completedData$BMI))
mmnorm.BMI
completedData$mmnorm.BMI<-mmnorm.BMI

mmnorm.Glucose<-(completedData$Glucose-min(completedData$Glucose))/(max(completedData$Glucose)-
                                                                      min(completedData$Glucose))
mmnorm.Glucose
completedData$mmnorm.Glucose <- mmnorm.Glucose

zscore.BloodPressure<-(completedData$BloodPressure- mean(completedData$BloodPressure))/ sd(completedData$BloodPressure)
zscore.BloodPressure
completedData$zscore.BloodPressure<-zscore.BloodPressure

zscore.Pregnancies<-(completedData$Pregnancies- mean(completedData$Pregnancies))/ sd(completedData$Pregnancies)
zscore.Pregnancies
completedData$zscore.Pregnancies<-zscore.Pregnancies

zscore.Glucose<-(completedData$Glucose- mean(completedData$Glucose))/ sd(completedData$Glucose)
zscore.Glucose
completedData$zscore.Glucose<-zscore.Glucose

zscore.SkinThickness<-(completedData$SkinThickness- mean(completedData$SkinThickness))/ sd(completedData$SkinThickness)
zscore.SkinThickness
completedData$zscore.SkinThickness<-zscore.SkinThickness

zscore.Insulin<-(completedData$Insulin- mean(completedData$Insulin))/ sd(completedData$Insulin)
zscore.Insulin
completedData$zscore.Insulin<-zscore.Insulin

zscore.BMI<-(completedData$BMI- mean(completedData$BMI))/ sd(completedData$BMI)
zscore.BMI
completedData$zscore.BMI<-zscore.BMI

zscore.Age<-(completedData$Age- mean(completedData$Age))/ sd(completedData$Age)
zscore.Age
completedData$zscore.Age<-zscore.Age


install.packages("corrplot",lib="/downloads/corrplot")
library(corrplot)
correlat <- cor(completedData[, setdiff(names(completedData), 'Outcome')])
corrplot(correlat)


completedData$part <- runif(length(completedData$Pregnancies),min = 0,max = 1)
completedData_training <- completedData[completedData$part<=0.75,]
completedData_test <- completedData[completedData$part>0.75,]
View(diabetes)
install.packages("plyr")
library(plyr)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
install.packages("C50")
library(C50)
install.packages("pROC")
library(pROC)
install.packages("ROSE")
library(ROSE)
diabetes$Outcome <- factor(ifelse(diabetes$Outcome == 0, 'No', 'Yes'))
df_balanced <- ovun.sample(Outcome ~ ., data = diabetes, method = "over", 
                           seed = 1, N = 990)$data
table(df_balanced$Outcome)
indices <- sample(2, nrow(df_balanced), replace = TRUE, 
                  prob = c(0.8, 0.2))
train <- df_balanced[indices == 1,]
test <- df_balanced[indices == 2,]
install.packages("caret")
library(caret)
cvCont <- trainControl(method = "repeatedcv", number = 5,
                       summaryFunction = twoClassSummary, 
                       classProbs = TRUE)

grid <- expand.grid(model = "tree",
                    trials = 1:10,
                    winnow = FALSE)
set.seed(1) 

c50_2nd <- train(x = train[, -9],
                 y = train[, 9],
                 method = "C5.0",
                 tuneLength = 3,
                 preProc = c("center", "scale"),
                 tuneGrid = grid,
                 metric = "ROC",
                 trControl = cvCont) 
c50_2nd
summary(c50_2nd)
plot(c50_2nd, transform.x = log10, 
     xlab = expression(log[10](gamma)), 
     ylab = "cost")
t_pred <- predict(c50_2nd, newdata = test[,-9], type = 'raw')
confusionMatrix(test$Outcome, t_pred)
auc_2nd <- predict(c50_2nd, newdata = test, type = 'prob')[,2]
plot.roc(test$Outcome, auc_2nd, percent=TRUE, 
         print.auc=TRUE, main = 'decision tree')
precision <- posPredValue(t_pred, test$Outcome)
recall <- sensitivity(t_pred, test$Outcome)
F1 <- (2 * precision * recall) / (precision + recall)
F1
accuracy <- table(Actual = test$Outcome, Pred = t_pred)
accuracy
addmargins(round(prop.table(accuracy, 1), 3) * 100)
varImpact <- varImp(c50_2nd, scale = FALSE)
varImpact
plot(varImpact, 8, main = "Variable impact")
install.packages("neuralnet",lib="/downloads/neuralnet_1.33/neuralnet")
library(neuralnet)

model <- glm(Outcome ~.,family=binomial(link='logit'),data=completedData_training[1:14])
summary(model)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(completedData_test,select=c(1,2,3,4,5,6,7,8,10,11,12,13,14
)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != completedData_test$Outcome)
print(paste('Accuracy',1-misClasificError))

#neural network model
install.packages("neuralnet",lib="/downloads/neuralnet_1.33/neuralnet")
library(neuralnet)

n <- names(completedData_normalized)
f <- as.formula(paste("Outcome ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=completedData_training,hidden=c(5,3),linear.output=T)

plot(nn)

pr.nn <- compute(nn,completedData_test[,10:16])
completedData_test[,10:17]

pr.nn_ <- pr.nn$net.result*(max(completedData_training$Outcome)-min(completedData_training$Outcome))+
  min(completedData_training$Outcome)
test.r <- (completedData_test$Outcome)*(max(completedData_training$Outcome)-min(completedData_training$Outcome))+
  min(completedData_training$Outcome)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(completedData_test)
MSE.nn

