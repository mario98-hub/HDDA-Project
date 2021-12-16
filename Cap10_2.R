fselection_filter<- function(data, method, k, Target){
  
  filter_evaluator <- filterEvaluator(method)
  
  skb_direct_search <- selectKBest(k=k)
  
  fselection<-skb_direct_search(data, Target, filter_evaluator)
  
  newdata<-data[,which(names(data) %in% c(fselection$featuresSelected,Target))]
  
  Vimp<-data.frame('Feature'=fselection$featuresSelected,'imporatance'= fselection$valuePerFeature)
  
  p<-ggplot(Vimp, aes(x=reorder(Feature,imporatance), y=imporatance, fill=Feature)) +
    geom_bar(stat = "identity", width=0.2)+
    labs(x = "Features", y = "Variable Importance")+
    scale_fill_brewer(palette = "Set1") +
    coord_flip()
  
  returnValue(list(p,newdata ))
}

datanew<-fselection_filter(data, "chiSquared", k=5 ,Target = 'Class') 
datanew[[1]] #Grafico
datanew[[2]] #dataset filtrato

x <- data %>%
  select(-Class) %>%
  as.data.frame()

# Target variable
y <- data$Class

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

#...Relevent codes...


# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:9),
                   rfeControl = control)
result_rfe1




# Print the selected features
predictors(result_rfe1)


# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()



varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:8],
                          importance = varImp(result_rfe1)[1:8, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")



# Post prediction
postResample(predict(result_rfe1, x_test), y_test)

