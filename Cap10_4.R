
set.seed(12345678)


ctrl  <- trainControl(method = "cv")

smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
test <- data[-train_ind,  ]
data <- data[train_ind, ]

filter_evaluator <- filterEvaluator("chiSquared")
skb_direct_search <- selectKBest(k=9)
fselection<-skb_direct_search(data, 'Class', filter_evaluator)
Rank<-data.frame('Rank'=1:length(fselection$featuresSelected),'imporatance'= fselection$valuePerFeature)
row.names(Rank)<-c(fselection$featuresSelected)

feat<-list()
best<-data.frame('acc'=1)

for (i in 1:(dim(Rank)[1]-1)) {
  
  feat[i]=row.names(Rank[dim(Rank)[1]+1-i,])
  subdata=data[,-which(names(data) %in% unlist(feat))]
  acc=list()
  folds <- partition(subdata$Class, p = c(fold_1 = 0.33, fold_2 = 0.33, fold_3 = 0.33))
  
  
  for(f in length(folds)){
    fold <- data[unlist(folds[f]),]
    validation_index <- createDataPartition(fold$Class, p=0.80, list=FALSE)
    analysis <- na.omit(fold[validation_index, ])
    assessment <- fold[-validation_index, ]
    knn<- train(Class ~ ., 
                data = analysis, method = 'knn', tuneGrid = data.frame(k = 5:9),
                trControl = ctrl)
    predict <- predict(knn, assessment)
    cm <- as.matrix(table(Actual = assessment$Class, Predicted = predict))
    accuracy <- sum(diag(cm))/sum(cm)
    acc[f]<-accuracy
  }
  
  best[paste('k_remove_',i,sep = '')]=c(max(unlist(acc)))
  
}
best=t(best)[-1,]
best


# performance
training <- data[,-which(names(data) %in% unlist(feat[1:5]))]
test <- test[,-which(names(test) %in% unlist(feat[1:5]))]

knn<- train(Class ~ ., 
            data = training, method = 'knn', tuneGrid = data.frame(k = 5:9),
            trControl = ctrl)

predict_train <- predict(knn, training)
predict_test <- predict(knn, test)
cm_train <- as.matrix(table(Actual = training$Class, Predicted = predict_train))
cm_test <- as.matrix(table(Actual = test$Class, Predicted = predict_test))
accuracy_train <- sum(diag(cm_train))/sum(cm_train)
accuracy_test <- sum(diag(cm_test))/sum(cm_test)
print(names(test))
paste('Accuracy nel training set', accuracy_train, sep=' ')
paste('Accuracy nel test set', accuracy_test, sep=' ')

set.seed(1234)

data("BreastCancer")
data<-na.omit(BreastCancer[,-1])

smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
test <- data[-train_ind,  ]
data <- data[train_ind, ]

#filtro utilizzato
filter_evaluator <- filterEvaluator("chiSquared")
skb_direct_search <- selectKBest(k=9)
best<-data.frame('acc'=0)
type_of_subset <- list()
acc=list()
folds <- partition(data$Class, p = c(fold_1 = 0.33, fold_2 = 0.33, fold_3 = 0.33))

for(f in length(folds)){
  fold <- data[unlist(folds[f]),]
  validation_index <- createDataPartition(fold$Class, p=0.80, list=FALSE)
  analysis <- fold[validation_index, ]
  assessment <- fold[-validation_index, ]
  fselection<-skb_direct_search(analysis, 'Class', filter_evaluator)
  Rank<-data.frame('Rank'=1:length(fselection$featuresSelected),'imporatance'= fselection$valuePerFeature)
  row.names(Rank)<-c(fselection$featuresSelected)
  
  feat<-list()
  
  
  for (i in 1:(dim(Rank)[1]-1)) {
    
    feat[i]=row.names(Rank[dim(Rank)[1]+1-i,])
    subanalysis=analysis[,-which(names(analysis) %in% unlist(feat))]
    subassessment=assessment[,-which(names(assessment) %in% unlist(feat))]
    #print(subassessment)
    knn<- train(Class ~ ., 
                data = subanalysis, method = 'knn', tuneGrid = data.frame(k = 5:9),
                trControl = ctrl)
    predict <- predict(knn, subassessment)
    cm <- as.matrix(table(Actual = subassessment$Class, Predicted = predict))
    accuracy <- sum(diag(cm))/sum(cm)
    #print(names(subassessment))
    #print(accuracy)
    if(length(type_of_subset)==0){
      type_of_subset <- append(type_of_subset,list(names(subassessment)))
      acc[1] <- accuracy 
    } 
    else{
      temp <- 1
      for (k in 1:length(type_of_subset)) {
        if(!(type_of_subset[k] %in% list(names(subassessment)))) {
          temp <- temp * 1
        }
        else{#features uguali
          temp <- temp * 0
          acc[k] <- (unlist(acc[k])+accuracy)/2
        }
      }
      if(temp==1){#sono tutti diversi
        type_of_subset <- append(type_of_subset,list(names(subassessment)))
        acc[(length(acc)+1)]<-accuracy
      }
    }
  }
}
best_subset<- type_of_subset[which.max(unlist(acc))]
best_acc <- max(unlist(acc))


# performance
training <- data[,which(names(data) %in% unlist(best_subset))]
test <- test[,which(names(test) %in% unlist(best_subset))]

knn<- train(Class ~ ., 
            data = training, method = 'knn', tuneGrid = data.frame(k = 5:9),
            trControl = ctrl)

predict_train <- predict(knn, training)
predict_test <- predict(knn, test)
cm_train <- as.matrix(table(Actual = training$Class, Predicted = predict_train))
cm_test <- as.matrix(table(Actual = test$Class, Predicted = predict_test))
accuracy_train <- sum(diag(cm_train))/sum(cm_train)
accuracy_test <- sum(diag(cm_test))/sum(cm_test)
print(names(test))
paste('Accuracy nel training set', accuracy_train, sep=' ')
paste('Accuracy nel test set', accuracy_test, sep=' ')

