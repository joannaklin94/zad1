library(kernlab)
library(gmodels)

#----------------------exploring and preparing the data----------------------
setwd("C:/Users/joann/Desktop/4GL/zad1")
mydata = read.csv("dataset_cancer_diagnosis.csv")
mydata <- mydata[-1]

mydata_train <- mydata[1:469, ]
mydata_test <- mydata[470:569, ]

#----------------------training a model on the data----------------------
svm_model_linear <- ksvm(diagnosis ~ ., data=mydata_train, kernel="vanilladot")
pred <- predict(svm_model_linear, mydata_test, type="response")
CrossTable(x = mydata_test$diagnosis, y = pred, prop.chisq=FALSE)

#----------------------improving model performance----------------------
svm_model_polynomial <- ksvm(diagnosis ~ ., data=mydata_train, kernel="polydot")
pred <- predict(svm_model_polynomial, mydata_test, type="response")
table(pred,mydata_test$diagnosis)  

svm_model_RBF <- ksvm(diagnosis ~ ., data=mydata_train, kernel="rbfdot")
pred <- predict(svm_model_RBF, mydata_test, type="response")
table(pred,mydata_test$diagnosis)  

