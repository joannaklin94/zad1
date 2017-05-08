library(e1071)
library(gmodels)

#----------------------exploring and preparing the data----------------------
setwd("C:/Users/joann/Desktop/4GL/zad1")
mydata = read.csv("dataset_cancer_diagnosis.csv")
mydata <- mydata[-1] 

# data is split into two sets: 469 records for learning and 100 records for testing 
mydata_train <- mydata[1:469, ]
mydata_test <- mydata[470:569, ]

#----------------------training a model on the data----------------------
classifier <- naiveBayes(mydata_train[ ,-1], mydata_train[ ,1])
predictor <- predict(classifier, mydata_test[ ,-1], type="class")
CrossTable(x = mydata_test$diagnosis, y = predictor, prop.chisq=FALSE)


classifier$apriori  # shows class distribution
classifier$tables$texture_mean

plot(function(x) dnorm(x, 17.55, 3.64), -5, 40, col="red", ylab="density", xlab="texture mean", main="Mean texture distribution for benign and malignant tumor")
par(new = TRUE)
plot(function(x) dnorm(x, 21.5, 3.8), -5, 40, col="green", ylab="density", xlab="texture mean")
legend("topright", c("benign tumor", "malignant tumor"), col = c("red", "green"), lwd=1)


print(confusionMatrix(predictor, mydata_test$diagnosis, positive="M", dnn=list('prediction','M')))
