mydata <- read.csv(file.choose())
View(mydata)
library(e1071)   # For Naive Bayes
library(class)   # For KNN
library(dplyr)   # For data manipulation
library(scales)  # Work with ggplot
library(party)   # For Decision Tree
library(ggplot2) # For plotting

mydata<- mydata %>% select(-AQI) 

mydata <- mydata %>% select(-X) 




# Create the 'AQI' column by categorizing based on 'PM 2.5' values
mydata$AQI <- ifelse(mydata[["PM.2.5"]] >= 0 & mydata[["PM.2.5"]] <= 30, "Good",
                     ifelse(mydata[["PM.2.5"]] > 30 & mydata[["PM.2.5"]] <= 60, "Satisfactory",
                            ifelse(mydata[["PM.2.5"]] > 60 & mydata[["PM.2.5"]] <= 90, "Moderate",
                                   ifelse(mydata[["PM.2.5"]] > 90 & mydata[["PM.2.5"]] <= 120, "Poor",
                                          ifelse(mydata[["PM.2.5"]] > 120 & mydata[["PM.2.5"]] <= 250, "Very Poor",
                                                 ifelse(mydata[["PM.2.5"]] > 250, "Severe", NA))))))
nrow(mydata)
ncol(mydata)
str(mydata)
mydata$AQI <- as.factor(mydata$AQI)
str(mydata$AQI)
table(mydata$AQI)
str(mydata[10])



# KNN Algorithm
ran1 <- sample(1:nrow(mydata),0.85*nrow(mydata))
nor1 <- function(x1) {(x1-min(x1))/(max(x1)-min(x1))} 
data_norm1 <- as.data.frame(lapply(mydata[,-10],nor1))
summary(mydata)
summary((data_norm1))

sum(is.na(mydata))      # Check missing values in training data
colSums(is.na(mydata))  # to check NA values column vise
which(rowSums(is.na(mydata)) > 0)  # will show row number that contains NA values
mydata[!complete.cases(mydata), ]  # will show row number with entries

mydata <- na.omit(mydata)
View(mydata)
sum(is.na(mydata))

data_train1 <- data_norm1[ran1,]
data_test1 <-data_norm1[-ran1,]

data_train_target1 <- mydata[ran1,10]
data_test_target1 <- mydata[-ran1,10]

k=sqrt(nrow(mydata))

model1 <- knn(data_train1,data_test1,cl=data_train_target1,k=9)   
model1

tab1 <- table(model1,data_test_target1)
tab1

accuracy <- function(x){sum(diag(x)/sum(rowSums((x))))}
KNN_Accuracy <- percent(accuracy(tab1))
KNN_Accuracy

# Naive Bayes Algo

#Train and Test Data set
ran <- sample(1:nrow(mydata),0.80* nrow(mydata))   # will generate 80% random rows
train_data <- mydata[ran,]
test_data <- mydata[-ran,]
str(mydata)

#Target dataset
data_train_target <- mydata[ran,10]    # column no 10 is my train target
data_test_target <- mydata[-ran,10]    # column no 10 is my test target

model <- naiveBayes(AQI~.,data=train_data)
model

prediction <- predict(model,test_data)
prediction

(tab <- table(test_data$AQI,prediction))

(NB_Accuracy <- percent(sum(diag(tab))/sum(tab)))


# Decision Tree

head(mydata)
summary(mydata)
class(mydata)
str(mydata)
colnames(mydata)

set.seed(1234)

ind = sample(2,nrow(mydata),replace = T,prob=c(0.8,0.2))  # 2 means passing two values i.e 1 & 2, replace = T means allowing system to replace the values 
mydata_train = mydata[ind==1,]
mydata_test = mydata[ind==2,]
dim(mydata_test)
dim(mydata_train)

tree = ctree(AQI~PM.2.5,data = mydata_train)
plot(tree)
tree=ctree(AQI~PM.2.5,data = mydata_train,controls = ctree_control(mincriterion =0.99,minsplit = 500))
mydata_predict =  predict(tree,mydata_test)
(mydata_predict_table = table(mydata_predict,mydata_test$AQI))
  (DT_Accuracy = percent(sum(diag(mydata_predict_table)/sum(mydata_predict_table))))


cat(KNN_Accuracy)
cat(NB_Accuracy)
cat(DT_Accuracy)


Overall_Accuracy<-data.frame(Algorithms=c("KNN","Decision Tree","Naive bayes"),Accuracy=c(KNN_Accuracy,NB_Accuracy,DT_Accuracy))
Overall_Accuracy


names(mydata)
mydata %>% ggplot(aes(x=T,y=`PM.2.5`))+ geom_point()

#Overall_Accuracy %>% ggplot(aes(x=Algorithms,y=Overall_Accuracy))+geom_col()

ggplot(data = Overall_Accuracy, aes(x = Algorithms, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "purple2", color = "black") +
  labs(title = "Algorithms Accuracy", x = "Algorithms", y = "Accuracy") 
  





