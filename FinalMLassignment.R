#coursera project
#
#install required libraries
#  install.packages("caret")
#  install.packages("rpart")
#  install.packages("RColorBrewer")
#  install.packages("RGtk2")
#  install.packages("randomForest")
#  install.packages("rmarkdown")
#  install.packages("knitr")
#  library(caret)
# library(rpart)
#  library(rpart.plot)
#  library(RColorBrewer)
#  library(randomForest)
#  library(RGtk2)
#  library(rmarkdown)
#  library(knitr)
set.seed(123456) #set seed for replicability purposes

#download training and testing datasets

url_train<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#make datasets readable as csv files
training <- read.csv(url(url_train), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(url_test), na.strings=c("NA","#DIV/0!",""))
#get rid of NAs
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]

#create a training (60% of the original data) and testing dataset (40%)
inTrain = createDataPartition(y=training$classe, p = 0.6, list=FALSE)
myTraining = training[inTrain, ]
mytesting = training[-inTrain, ]
mytraining<-myTraining
#check structure of the datasets for exploration purposes
dim(mytraining); dim(mytesting); dim(testing)
str(mytraining)
colnames(mytraining)
colnames(testing)
#creates a list of variables with 0 observations or not useful predictors (NZV)
myDataNZV <- nearZeroVar(mytraining, saveMetrics=TRUE)



#take the list of variables with NZV and concatenate them as a list for readability purposes
myNZVvars <- names(mytraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")


mytraining <- mytraining[!myNZVvars]#updates the training dataset without NZV variables


#To check that the new N of observations is the same as in the old training dataset
dim(mytraining)

#remove ID from dataset so that it does not interfere with machine learning analyses
mytraining <- mytraining[c(-1)]
dim(mytraining)

#get rid of NA; I decided to remove those with more than 60% NA
trainingV3 <- mytraining #creating another clean data subset (training V3) with less than 60% NA
for(i in 1:length(mytraining)) { #for every column in the training dataset, if NAs>60%  get rid of it
if( sum( is.na( mytraining[, i] ) ) /nrow(mytraining) >= .6 ) {
for(j in 1:length(trainingV3)) {
if( length( grep(names(mytraining[i]), names(trainingV3)[j]) ) ==1)  { # compare training and training V3
        #and if a column is found to have too many NAs remove it and save it all in training V3
trainingV3 <- trainingV3[ , -j]
}
}
}
}
dim(trainingV3)
#To check  new N of observations in training V3
dim(mytraining)
plot(mytraining$classe)#visualize the data
summary(mytraining$classe)#plot data

myTraining <- trainingV3

rm(trainingV3)

clean1 <- colnames(mytraining)#create a table with the list of variables that are included in the training dataset
mytesting <- mytesting[clean1]#mytesting has all the variables included in my training
clean2<-colnames(mytraining[,-58])#make the mytesting and initial
#testing dataset compatible with same variables
testing<-testing[clean2]
#To check the new N of observations, which should be 57
dim(mytesting)
dim(testing)
#coerce data into the same data type
for (i in 1:length(testing) ) {
  for(j in 1:length(mytraining)) {
    if( length( grep(names(mytraining[i]), names(testing)[j]) ) ==1)  {
      class(mytesting[j]) <- class(mytraining[i])
    }
  }
}

testing<-rbind(mytraining[2, -58], testing)
testing<-testing[-1,]

#test hardness by using a 3-fold cross-validation to estimate accuracy. This is set in subsequent
#code using the "fitControl" object

fitControl <- trainControl(method='cv', number = 3)
metric <-"fitControl"


#apply a first ML algorithm for prediction: generalized boosted regression
#(gbm)
set.seed(7)
modFitA1 <- train(classe ~ ., data=mytraining, method="gbm", trControl=fitControl)
par(mar = rep(2, 4))
summary(modFitA1)


install.packages("e1071")
library(e1071)

#use predictions on testing dataset using  the training dataset
predsA1 <- predict(modFitA1, mytesting)
#compare predictions and real data using the confusion matrix
CMA1<-confusionMatrix(predictionsA1, mytesting$classe)
par(mar = rep(2, 4))
plot(predsA1)

#use a second ML algorithm for prediction: random forest
set.seed(7)
modFitB1 <- randomForest(classe ~. , data=mytraining, trControl=fitControl)
#use predictions on training set on testing dataset
predsB1 <- predict(modFitB1, mytesting, type = "class")
#calculate accuracy of model use confusion matrix
CMB1<-confusionMatrix(predictionsB1, mytesting$classe)
plot(predsB1)



plot(modFitA1)
plot(modFitB1)

AccuracyResults<-data.frame(Model=c('GBR', 'RF'), Accuracy=rbind(CMA1$overall[1], CMB1$overall[1]))
print(AccuracyResults)

#out-of-sample error is equal to 1 - accuracy against cross-validation
#dataset. RF is the best model with 99.89% accuracy e.g. OSE=.11%

#now use RF prediction in the "initial" testing dataset that
#we downloaded at the beginning -independent sample)
predictB2<-predict(modFitB1, testing, type="class")
plot(predictB2)



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictB2)

library(knitr)
library(markdown)
knit("FinalMLassignment.Rmd")


#transform the .md to HTML format
markdownToHTML("FinalMLassignment.md", "FinalMLassignment.html",fragment.only = TRUE)
