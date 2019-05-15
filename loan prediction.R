train<-read.csv(file.choose(),na.strings = "")
summary(train)
table(complete.cases(test))
str(train)
test<-read.csv(file.choose(),na.strings = "")
summary(test)


#convert credit history to factor
train$Credit_History<-as.factor(train$Credit_History)

#dealing with NA values in train

train[!complete.cases(train),]
train[is.na(train$Gender),]


train[is.na(train$Gender),"Gender"] <-"Male"

train[is.na(train$Married),"Married"] <-"Yes"

train[is.na(train$Dependents),"Dependents"] <-"0"

train[is.na(train$Self_Employed),"Self_Employed"] <-"No"

train[is.na(train$Credit_History),"Credit_History"] <-"1"

#dealing with missing numerical values
boxplot(train$LoanAmount,horizontal = T)

by(train,train$Property_Area,FUN = summary)
#property area has been used since these are home loans
train[is.na(train$LoanAmount) & train$Property_Area == "Rural","LoanAmount"] <- median(train[train$Property_Area == "Rural","LoanAmount"],na.rm = T)

train[is.na(train$LoanAmount) & train$Property_Area == "Semiurban","LoanAmount"] <- median(train[train$Property_Area == "Semiurban","LoanAmount"],na.rm = T)

train[is.na(train$LoanAmount) & train$Property_Area == "Urban","LoanAmount"] <- median(train[train$Property_Area == "Urban","LoanAmount"],na.rm = T)

summary(train$Loan_Amount_Term)

train[is.na(train$Loan_Amount_Term),"Loan_Amount_Term"] <- median(train[train$Loan_Amount_Term,"Loan_Amount_Term"],na.rm = T)

#response rate
table(train$Loan_Status)

#building model
library(randomForest)

table(train$Loan_Status)
RF <- randomForest(Loan_Status ~ ., data = train[,-c(1,2,5,6)], 
                   ntree=401, mtry = 3, nodesize = 5,
                   importance=TRUE)
print(RF)

plot(RF, main="random forest")

RF$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
#tRF holds best random forest output
tRF <- tuneRF(x = train[,-c(1,2,6,13)], 
              y=train$Loan_Status,
              mtryStart = 3, 
              ntreeTry=301, 
              stepFactor = 2, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 5, 
              importance=TRUE
)

tRF
tRF$importance



#loading test data
test<-read.csv(file.choose(),na.strings = "")
summary(test)

#convert credit history to factor
test$Credit_History<-as.factor(test$Credit_History)

#dealing with NA values in test


test[is.na(test$Gender),"Gender"] <-"Male"


test[is.na(test$Dependents),"Dependents"] <-"0"

test[is.na(test$Self_Employed),"Self_Employed"] <-"No"

test[is.na(test$Credit_History),"Credit_History"] <-"1"

#dealing with missing numerical values

#median criteria is property area since these are home loans
test[is.na(test$LoanAmount) & test$Property_Area == "Rural","LoanAmount"] <- median(test[test$Property_Area == "Rural","LoanAmount"],na.rm = T)

test[is.na(test$LoanAmount) & test$Property_Area == "Semiurban","LoanAmount"] <- median(test[test$Property_Area == "Semiurban","LoanAmount"],na.rm = T)

test[is.na(test$LoanAmount) & test$Property_Area == "Urban","LoanAmount"] <- median(test[test$Property_Area == "Urban","LoanAmount"],na.rm = T)


test[is.na(test$Loan_Amount_Term),"Loan_Amount_Term"] <- median(test[test$Loan_Amount_Term,"Loan_Amount_Term"],na.rm = T)

summary(test)

## Prediction
test$predict.class <- predict(tRF, test, type="class")
test$predict.score <- predict(tRF, test, type="prob")

table(test$predict.class)

write.csv(test,"C:/Users/Kesav Duvvuri/Desktop/R-3.5.1/working directory/Competitions/loan_prediction_submission.csv")
