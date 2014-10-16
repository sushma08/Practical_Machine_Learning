pml_write_files <- function(x){
    ## Prints predictions to separate files
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


project <- function(){
    ## Predicts the quality of weight lifting exercise based on accelerometer
    ## data.
    ## Training data of form [id {various accelerometer information} classe] was
    ## used, where classe denotes quality of exercise.
    ## Training data was acquired and preprocessed before using random
    ## forest model. Cross-validation showed Accuracy = 0.995. 

    set.seed(33833)                     

    ## Data acquisition
    if(!file.exists("data")) dir.create("data")              # create a folder for data

    if(!file.exists("data/train.csv")){
        download.file(
            "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
            destfile="./data/train.csv", method="curl")      # download training data
    }

    if(!file.exists("data/test.csv")){
        download.file(
            "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
            destfile="./data/test.csv", method="curl")       # download final test data
    }

    data = read.csv("data/train.csv", header=TRUE)           # load the training data
    test_data = read.csv("data/test.csv", header=TRUE)       # and the final test data

    clean_data = data.frame(matrix(1, nrow = nrow(data), ncol = 0)) 
    
    ## Data cleaning
    for(name in names(data)[-1]){                            # the first column (indices) was removed
        if (any(is.na(data[, name]))) next                   # all columns containing NA removed
        if (any(data[, name] == "" )) next                   # all columns containing empty entries removed
        clean_data[, name] = data[, name]                    # all remaining columns added to clean_data 
    }


    ## Filter test data by the same columns filtered in clean_data
    cleanNames <- names(clean_data)
    cleanNames <- cleanNames[cleanNames != "classe"]
    test_data <- test_data[, cleanNames]

    if(any(is.na(test_data)) || any(test_data == ""))        # double check that test_data valid
        return("Program terminated. test_data has unexpected empty/NA entries")
    
    ## Cross-validation partitioning
    inTrain <- createDataPartition(y=clean_data$classe,
                                   p=0.2, list=FALSE)
    training <- clean_data[inTrain,]                         # partition the training data into
    testing <- clean_data[-inTrain,]                         # training and testing data for cross-validation

    nsv <- nearZeroVar(training,saveMetrics=TRUE)            # check for zero variance
    if(any(nsv$zeroVar)){                                    # remove zero variance columns if exist
        print("At least one covariate as zero variance. Commencing removal.")
        training <- training[!nsv$zeroVar,]
        testing <- testing[!nsv$zeroVar,]
    }else{
        print("All selected columns have nonzero variance.")
    }


    print("Initiating Random Forest Algorithm")
    ## Model creation
    modFit <- train(classe ~ ., method="rf", data=training)  # rf model
    print(modFit)                                            # print fitness of model

    ## Cross Validation/Out-of-Sample Error
    pred <- predict(modFit, newdata=testing)      # run prediction on previously partitioned testing data
    ## Compute out-of-sample error
    print(confusionMatrix(pred, testing$classe))  # visualize via confusion matrix

    ## Predictions for test_data
    pred <- predict(modFit, newdata=test_data)    # actual predictions 
    pml_write_files(pred)                         # written to file
}

library(caret)
project()
