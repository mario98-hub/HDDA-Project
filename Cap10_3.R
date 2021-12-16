rm(list=ls())
#Set up ambiente
library(caret)
library(tidyverse)
library(Rcpp)
## parametri per la simulazione ##
noise_max <- 100
step <- 25
nrow_dataset_list <- c(1000, 500)
n_iterations <- 5

#modelli utilizzati
modelli<-c('lm','ridge','lasso','knn','rf','treebag')


ctrl  <- trainControl(method = "cv")
rctrl <- trainControl(method = "cv", search = "random")

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#funzione che esegue un ciclo sui modelli e ne calcola le performace
mod_noise<- function (mod, n, noise_max, step) {
  seq_noise <- seq(0, noise_max, step)
  risultati<-data.frame('Models'=modelli)
  for (n_noise in seq_noise){
    train<-SLC14_1(n = n, noiseVars = n_noise, corrValue = 0)
    test<-SLC14_1(n = 10000, noiseVars = n_noise, corrValue = 0)
    print(n_noise)
    Rmse_vec <- c()
    for (i in 1:length(mod)){
      m<-mod[i]
      print(m)
      if (m=='treebag'){
        fit<-  train(y ~ ., data = train, method = mod[i],
                     trControl = ctrl)
      }
      if (m=='knn'){
        tuneGrid = data.frame(k = 5:9)
        fit<- train(y ~ ., 
                    data = train, method = mod[i], tuneGrid = data.frame(k = 5:9),
                    trControl = ctrl)
      }
      if (m=='rf') {
        mtryGrid = data.frame(mtry = (ncol(train)-1)/3)
        fit <- train(y ~ ., data = train, ntree = 50, method = "rf",
                     tuneGrid = mtryGrid,trControl = ctrl)
      }
      if (m=='lasso'){
        glmn_grid <- expand.grid(alpha = 1,
                                 lambda = 2^(-2:2))
        fit <- train(y ~ .+ Var05:Var06 + Var19:Var20, 
                     data = train, method = "glmnet",
                     preProc = c("center", "scale"),
                     tuneGrid = glmn_grid,
                     trControl = ctrl)
      }
      if (m=='ridge'){
        glmn_grid <- expand.grid(alpha = 0,
                                 lambda = 2^(-2:2))
        fit <- train(y ~ .+ Var05:Var06 + Var19:Var20, 
                     data = train, method = "glmnet",
                     preProc = c("center", "scale"),
                     tuneGrid = glmn_grid,
                     trControl = ctrl)
      }
      if (m=='lm') { 
        fit<- train(y ~ .+ Var05:Var06 + Var19:Var20, 
                    data = train, method = 'lm',
                    trControl = ctrl)
      }
      pred = predict(fit, newdata = test)
      Rmse = round(RMSE(pred, test$y),2)
      print(Rmse)
      Rmse_vec<-c(Rmse_vec,Rmse)
    }
    risultati[paste0(n_noise,"")]<-c(Rmse_vec)
  }
  
  
  risultati<- t(risultati)
  risultati<- risultati[-1,]
  colnames(risultati) <- modelli
  returnValue(risultati)
}


seq_noise <- seq(0, noise_max, step)

results_average_list <- c(list())

#stimiamo i modelli un numero di volte pari a n_iterations
for(i in 1:n_iterations){
  print("iterazione:")
  print(i)
  set.seed(555+i)
  
  results_list <-c(list())
  for(nrow_dataset in nrow_dataset_list){
    result <- as.tibble(mod_noise(modelli,nrow_dataset, noise_max, step))
    result<-result%>%mutate(Rmse=seq_noise)%>%
      gather('lm','ridge','lasso','knn','rf','treebag', key = Model, value = RMSE)
    results_list <- append(results_list, list(as.data.frame(result)))
  }
  
  
  if (i == 1) results_average_list <- results_list
  else{
    for (j in 1:length(results_list)){
      results_average_list[[j]]$RMSE <- as.numeric(results_average_list[[j]]$RMSE) + as.numeric(results_list[[j]]$RMSE)
    }
  }
}


#mediamo i risultati
for (j in 1:length(results_list)){
  results_list[[j]]$RMSE <-  results_average_list[[j]]$RMSE/n_iterations
}

### results_list è quello mediato da questo punto ###

#grafici
plot_list <-c(list())

for (i in seq_along(results_list)){
  title <- paste0("N = ", nrow_dataset_list[i])
  data=as.data.frame(results_list[i])
  p<-ggplot(data=data, aes(x=Rmse, y=RMSE, group=Model)) +
    geom_line(aes(color=Model))+
    geom_point(aes(shape=Model))+
    labs(title = title, x = "Additional Noise Varaibilie") #subtitle = sub)
  plot_list <- append(plot_list, list(p))
}

plot_list[[1]]
plot_list[[2]]
