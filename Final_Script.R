library(dplyr)
adult <- read.table("C:/Users/Sprit/Desktop/adult.data", sep = ",", header = FALSE);

colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "educ
                     ation_num", "marital_status", "occupation", "relationship", "race",
                     "sex","capital_gain", "capital_loss", "hours_per_week", "native_country", "income");
dim(adult)
adult[ adult == " ?" ] <- NA;
adult <- na.omit(adult);
adult$workclass <- droplevels(adult$workclass);
adult$fnlwgt <- NULL
adult$relationship <- NULL
government <- c(" Federal-gov", " Local-gov", " State-gov")
self <- c(" Self-emp-inc", " Self-emp-not-inc"," Without-pay")
adult$MaritalStatus <- ifelse(adult$marital_status %in% 
                                c(" Married-AF-spouse", " Married-civ-spouse"), "married", "alone")
adult$work_class <- ifelse(adult$workclass %in% government, "government", 
                           ifelse(adult$workclass %in% self, "self", 
                                  ifelse(adult$workclass == " Private", "private", "other")))
School <- c(" 10th", " 11th", " 12th", " 1st-4th", " 5th-6th", " 7th-8th", " 9th", " Preschool")
HS_Grad <- c(" HS-grad", " Some-college")
Graduate <- c(" Masters", " Doctorate")
Assoc <- c(" Prof-school", " Assoc-acdm", " Assoc-voc")
adult$Education_Level <- ifelse(adult$education %in% HS_Grad, "HS-grad", 
                                ifelse(adult$education %in% School, "Below high school", 
                                       ifelse(adult$education %in% Assoc, "Assoc",
                                              ifelse(adult$education %in% Graduate, "graduate", "college"))))
developed_countries <- c(" Canada", " England", " France", " Germany", " Italy", " Ireland", 
                         " Japan", " Portugal", " Taiwan", " India", " Holand-Netherlands", " China", " United-States")
adult$Country_Native <- ifelse(adult$`native_countr` %in% developed_countries, "developed", "not_developed")
DeskJob <- c(" Exec-managerial", " Prof-specialty", " Protective-serv", " Sales", " Tech-support")
adult$Profession <- ifelse(adult$occupation %in% DeskJob, "Desk job", "Not desk job")

adult$capital_income <- adult$capital_gain - adult$capital_loss
adult$capital_income <- ifelse(adult$capital_income == 0, "Zero",
                               ifelse(adult$capital_income < 0 , "negative", "positive"))
adult$race <- ifelse(adult$race == " White", "White", "other")
adult$hours_w[adult$hours_per_week < 40] <- " less_than_40"
adult$hours_w[adult$hours_per_week >= 40 & 
                adult$hours_per_week <= 45] <- " between_40_and_45"
adult$hours_w[adult$hours_per_week > 45 &
                adult$hours_per_week <= 60  ] <- " between_45_and_60"
adult$hours_w[adult$hours_per_week > 60 &
                adult$hours_per_week <= 80  ] <- " between_60_and_80"
adult$hours_w[adult$hours_per_week > 80] <- " more_than_80"
adult$Profession <- as.factor(adult$Profession)
adult$capital_income <- as.factor(adult$capital_income)
adult$hours_w <- as.factor(adult$hours_w)
adult$Country_Native <- as.factor(adult$Country_Native)
adult$Education_Level <- as.factor(adult$Education_Level)
adult$work_class <- as.factor(adult$work_class)
adult$MaritalStatus <- as.factor(adult$MaritalStatus)
adult$sex <- as.factor(adult$sex)
adult$race <- as.factor(adult$race)

adult <- adult %>% 
  select(-occupation,-hours_per_week, -native_country, -marital_status, -workclass, 
         -education, -capital_gain, -capital_loss)

#To keep the income column as the last one
income <- adult$income
adult <- adult %>% select(-income)
adult<- cbind(adult, income)
dim(adult)
summary(adult)



library(ggplot2)
tab <- with(adult, table(age, income))
ageratio <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(data = ageratio, aes(x = age, y = Freq, group = income, colour = income)) + geom_line() + geom_point() + scale_x_discrete(name = "Age",breaks = seq(10,100,by=10)) +  scale_y_continuous(name="Percentage") + ggtitle("Distribution of Income Level by Age")
tab <- with(adult, table(Country_Native, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = Country_Native,y = Freq, fill = income)) + geom_bar(stat = "identity")  +     labs(x = "Native Country",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Native Country") + theme(plot.title = element_text(hjust = 0.5))
tab <- with(adult, table(Education_Level, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = Education_Level,y = Freq, fill = income)) + geom_bar(stat = "identity") +     labs(x = "Education Level",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Education Level") + theme(plot.title = element_text(hjust = 0.5))
tab <- with(adult, table(MaritalStatus, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = MaritalStatus,y = Freq, fill = income)) + geom_bar(stat = "identity") +     labs(x = "Marital Status",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Marital Status") + theme(plot.title = element_text(hjust = 0.5))
tab <- with(adult, table(Profession, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = Profession,y = Freq, fill = income)) + geom_bar(stat = "identity") +     labs(x = "Profession",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Profession") + theme(plot.title = element_text(hjust = 0.5))
tab <- with(adult, table(sex, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = sex,y = Freq, fill = income)) + geom_bar(stat = "identity") +     labs(x = "Gender",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Gender") + theme(plot.title = element_text(hjust = 0.5))
tab <- with(adult, table(hours_w, income))
temp <- data.frame(prop.table(tab, margin = 1)*100)
ggplot(temp, aes(x = hours_w,y = Freq, fill = income)) + geom_bar(stat = "identity") +     labs(x = "Hours Worked per Week",y = "Percentage", fill = "Income") +     ggtitle("Income grouped by Hours Worked") + scale_x_discrete(labels = c("40-45", "45-60", "60-80", "<40", ">80")) + theme(plot.title = element_text(hjust = 0.5))

library(e1071)
library(rpart)
library(rpart.plot)
library(partykit)
library(caret)

# Partitioning of Data
ind <- sample(2, nrow(adult),replace = TRUE,prob = c(0.8,0.2))
tdata <- adult[ind==1,]
vdata <- adult[ind==2,]

#Naive bayes model
NB_model <- naiveBayes(income~age+race+sex+MaritalStatus+work_class+Education_Level+
                         Country_Native+Profession+hours_w+capital_income,data = tdata)
#visualise the conditional probabilities
NB_model
predict(NB_model,vdata,type="class") -> result2
confMat_NB <- table(vdata$income,result2)
accuracy_NB <- sum(diag(confMat_NB))/sum(confMat_NB)
accuracy_NB
#Naive bayes model checking for overfitting
predict(NB_model,tdata,type="class") -> resultNBof
confMat_NBof <- table(tdata$income,resultNBof)
accuracy_NBof <- sum(diag(confMat_NBof))/sum(confMat_NBof)
accuracy_NBof

#SVM model
svm_model <- svm(income~age+race+sex+MaritalStatus+work_class+Education_Level+
                   Country_Native+Profession+hours_w+capital_income, data=tdata)
predict(svm_model,vdata,type="class") -> result3
confMat_SVM <- table(vdata$income,result3)
accuracy_svm <- sum(diag(confMat_SVM))/sum(confMat_SVM)
accuracy_svm
#SVM model checking for overfitting
predict(svm_model,tdata,type="class") -> resultOF
confMat_SVMof <- table(tdata$income,resultOF)
accuracy_svmof <- sum(diag(confMat_SVMof))/sum(confMat_SVMof)
accuracy_svmof


# Decision_tree for training data
decision_tree <- ctree(income~age+race+sex+MaritalStatus+work_class+Education_Level+
                         Country_Native+Profession+hours_w+capital_income,data = tdata)
decision_tree


# Prediction on Validate data
predict(decision_tree,vdata,type="response") -> result1

# Generation of confusion Matrix
confMat <- table(vdata$income,result1)

# Calculation of accuracy
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)

# Prediction on Training data to check overfitting
predict(decision_tree,tdata,type="response") -> result1


# Generation of confusion Matrix
confMat <- table(tdata$income,result1)

# Calculation of accuracy
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)


#Decision_tree using rpart
decision_treeRpart <- rpart(income~age+race+sex+MaritalStatus+work_class+Education_Level+
                              Country_Native+Profession+hours_w+capital_income,data = tdata)
rpart.plot(decision_treeRpart)

# Prediction on Validate data
predict(decision_treeRpart,vdata,type="class") -> resultrpart

# Generation of confusion Matrix
confMat_rpart <- table(vdata$income,resultrpart)
# Calculation of accuracy
accuracy_rpart <- sum(diag(confMat_rpart))/sum(confMat_rpart)
print(accuracy_rpart)

# Prediction on Training data to check overfitting
predict(decision_treeRpart,tdata,type="class") -> resultrpart

# Generation of confusion Matrix
confMat_rpart <- table(tdata$income,resultrpart)
# Calculation of accuracy
accuracy_rpart <- sum(diag(confMat_rpart))/sum(confMat_rpart)
print(accuracy_rpart)



# KNN Model
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

set.seed(222)
fit <- train(income~.,
             data = tdata,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center","scale"))

#Model Performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = vdata)
confusionMatrix(pred, vdata$income)
pred <- predict(fit, newdata = tdata)
confusionMatrix(pred, tdata$income)