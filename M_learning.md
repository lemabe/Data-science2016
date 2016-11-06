Executive Introduction
----------------------

In this project i will predict the manner in which they did the exercise. This is the "classe" variable in the training set. i will use any of the other variables to predict the exercise. I wil create a report describing how i built my model, how i used cross validation, what i think the expected out of sample error is, and why you made the choices you did. And finaly i will also use my prediction model to predict 20 different test cases. now let's load the package we will used for this project but before that let's state the background of this project.

Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

### Load packages

In this assignment we will explore the data using the package `caret`, `rpart`, and `randomForest` for our prediction. load the different package:

``` r
library(knitr)
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(rpart)
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(3223)
```

### Data loading

data was downloaded directly to my home directory via the url and will be read directly from the preceding directory.

``` r
training <- read.csv("pml-training.csv",row.names=1,na.strings = "")
testing <- read.csv("pml-testing.csv",row.names=1,na.strings = "NA")
```

let's create a partition for our usable dataset.

``` r
inTrain  <- createDataPartition(training$classe, p=0.7, list=FALSE)
TrainSet <- training[inTrain, ]
TestSet  <- training[-inTrain, ]
dim(TrainSet)
```

    ## [1] 13737   159

``` r
dim(TestSet)
```

    ## [1] 5885  159

let's remove variables with Nearly Zero Variance and all the NA's on th data.

``` r
N_Z_V <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[, -N_Z_V]
TestSet  <- TestSet[, -N_Z_V]
dim(TrainSet)
```

    ## [1] 13737    75

Removing the NA's

``` r
AllNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, AllNA==FALSE]
TestSet  <- TestSet[, AllNA==FALSE]
dim(TestSet)
```

    ## [1] 5885   58

Removing unuseful variable from column(1:5)

``` r
TrainSet <- TrainSet[, -(1:5)]
TestSet  <- TestSet[, -(1:5)]
dim(TrainSet)
```

    ## [1] 13737    53

Prediction method: Random Forest
--------------------------------

one method will be applied to model the regressions (in the Train dataset) and the best one (with higher accuracy when applied to the Test dataset) will be used for the quiz predictions. The method is: Random Forests.

``` r
set.seed(3223)
RF_control <- trainControl(method="cv", number=3, verboseIter=FALSE)
```

``` r
Fit_RandForest <- train(classe ~ ., data=TrainSet, method="rf",
                          trControl=RF_control)
Fit_RandForest$finalModel
```

    ## 
    ## Call:
    ##  randomForest(x = x, y = y, mtry = param$mtry) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 27
    ## 
    ##         OOB estimate of  error rate: 0.63%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 3902    3    1    0    0 0.001024066
    ## B   20 2632    5    1    0 0.009781791
    ## C    0   11 2375   10    0 0.008764608
    ## D    0    1   23 2227    1 0.011101243
    ## E    0    2    3    5 2515 0.003960396

let's do some prediction now on the test data.

``` r
predict_RandForest <- predict(Fit_RandForest, newdata=TestSet)
confMat_RandForest <- confusionMatrix(predict_RandForest, TestSet$classe)
confMat_RandForest
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1671    9    0    0    0
    ##          B    1 1125    7    0    0
    ##          C    1    5 1016    3    1
    ##          D    0    0    3  960    3
    ##          E    1    0    0    1 1078
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9941          
    ##                  95% CI : (0.9917, 0.9959)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9925          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9982   0.9877   0.9903   0.9959   0.9963
    ## Specificity            0.9979   0.9983   0.9979   0.9988   0.9996
    ## Pos Pred Value         0.9946   0.9929   0.9903   0.9938   0.9981
    ## Neg Pred Value         0.9993   0.9971   0.9979   0.9992   0.9992
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2839   0.1912   0.1726   0.1631   0.1832
    ## Detection Prevalence   0.2855   0.1925   0.1743   0.1641   0.1835
    ## Balanced Accuracy      0.9980   0.9930   0.9941   0.9973   0.9979

we can see that the confusion matrix give us an accuracy of .9941 we can conclude that the random forest method is the best method for our prediction. but now, let's have a look on the matrix result.

``` r
plot(confMat_RandForest$table, col = confMat_RandForest$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confMat_RandForest$overall['Accuracy'], 4)))
```

![](M_learning_files/figure-markdown_github/unnamed-chunk-11-1.png)

let's use the Random Forest model to predict our data.

Prediction for the 20 question quiz.
------------------------------------

``` r
data_predict <- predict(Fit_RandForest, newdata=testing)
data_predict
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E
