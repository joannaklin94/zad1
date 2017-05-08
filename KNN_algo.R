library(class)
library(gmodels)

#----------------------exploring and preparing the data----------------------
setwd("C:/Users/joann/Desktop/4GL/zad1")
# first column (id) is obsolete
mydata = read.csv("dataset_cancer_diagnosis.csv")
mydata <- mydata[-1] 
mydata$diagnosis <- factor(mydata$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"));

# normalization needs to be applied to rescale the attributes
normalize <- function(x) {	return ((x - min(x)) / (max(x) - min(x)))	}
mydata_normalized <- as.data.frame(lapply(mydata[2:31], normalize)) 

# data is split into two sets: 469 records for learning and 100 records for testing 
mydata_train <- mydata_normalized[1:469, ]
mydata_test <- mydata_normalized[470:569, ]

# labels which store the diagnosis factor
mydata_train_labels <- mydata[1:469, 1]
mydata_test_labels <- mydata[470:569, 1]

#----------------------training a model on the data----------------------
#k=21
mydata_test_pred_21 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=21)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_21, prop.chisq=FALSE)$t 
list_false_negatives <- cross[2]
list_false_positives <- cross[3]

#----------------------improving model performance----------------------
#k=1
mydata_test_pred_1 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=1)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_1, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=3
mydata_test_pred_3 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=3)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_3, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=5
mydata_test_pred_5 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=5)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_5, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=7
mydata_test_pred_7 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=7)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_7, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=9
mydata_test_pred_9 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=9)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_9, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=11
mydata_test_pred_11 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=11)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_11, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=13
mydata_test_pred_13 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=13)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_13, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=15
mydata_test_pred_15 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=15)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_15, prop.chisq=FALSE)$t
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

#k=27
mydata_test_pred_27 <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=27)
cross = CrossTable(x = mydata_test_labels, y = mydata_test_pred_27, prop.chisq=FALSE)$t 
list_false_negatives <- append(list_false_negatives, cross[2])
list_false_positives <- append(list_false_positives, cross[3])

# drawing two plots to find the best k
x <- c(21, 1, 3, 5, 7, 9, 11, 13, 15, 27)
y1 <- list_false_negatives
y2 <- list_false_positives

plot(x,y1, type = "p",col = "blue", xlab = "k", ylab = "number of false negatives", main = "False negative results vs. k")
plot(x,y2, type = "p",col = "blue", xlab = "k", ylab = "number of false positives", main = "False positive results vs. k")







