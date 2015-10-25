setwd("/media/data/data_project/citybike")

library(dplyr)
library(gbm)
library(caret)
#library(DMwR)
library(pROC)
load('train/train_bin')
load('test/test_bin')
cmat <- list()
monthFit <- list()
for(i in 1:12){
  mth <- gmstbin %>% filter(month == i)
  mth <- mth[,-2]
  mtht <- gmstbin_test %>% filter(month == i)
  mtht <- mtht[,-2]
  
  set.seed(97)    
  gbmFit <- gbm(formula = value ~ .,            #def best for hjuly
                 distribution = "bernoulli", 
                 data = mth,
                 n.trees = 500,              
                 interaction.depth = 9,  
                 shrinkage = 0.1,          
                 verbose = FALSE,
                 cv.folds = 10,
                 n.cores = 4)   
  

  gbmPred <- predict(gbmFit, newdata = mtht, n.trees = 500,type = "response")
  pred <- ifelse(gbmPred > .5, 1, 0)
  
  cmat[[i]] <- confusionMatrix(pred, mtht$value)
  monthFit[[i]] <- gbmFit
  print(i)
}
save(monthFit, file = 'monthFit')
save(cmat, file = 'month_confmat')

################################################################# try with weight to compensate class inbalance
## does not work well
cmat <- list()
monthFit <- list()
for(i in 1:12){
  mth <- gmstbin %>% filter(month == i)
  mth <- mth[,-2]
  mtht <- gmstbin_test %>% filter(month == i)
  mtht <- mtht[,-2]
  
  w <- mth$value
  w <- ifelse(w == 1, 6, 1)
  set.seed(97)    
  gbmFit <- gbm(formula = value ~ .,            #def best for hjuly
                distribution = "bernoulli", 
                data = mth,
                n.trees = 500,              
                interaction.depth = 9,  
                shrinkage = 0.1, 
                weights = w,
                verbose = FALSE,
                #cv.folds = 10,
                n.cores = 4)   
  

  gbmPred <- predict(gbmFit, newdata = mtht, n.trees = 500,type = "response")
  pred <- ifelse(gbmPred > .5, 1, 0)
  
  cmat[[i]] <- confusionMatrix(pred, mtht$value)
  
  monthFit[[i]] <- gbmFit
  print(i)
}
save(monthFit, file = 'monthFit')
save(cmat, file = 'month_confmat')


################################################################### try resample
## and do a small grid of value
gbmGrid <- expand.grid(interaction.depth = seq(5, 7, by = 2), n.trees = seq(100, 500, by = 200), shrinkage = c(0.1, 1))

library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
cmat <- list()
for(i in 7:12){
  mth <- gmstbin %>% filter(month == i)
  mth <- mth[,-2]
  mtht <- gmstbin_test %>% filter(month == i)
  mtht <- mtht[,-2]
  
  mthup <- upSample(mth[,1:7], as.factor(mth$value), yname='value')
  mthup$value <- as.numeric(as.character(mthup$value))
  
  AUC <- foreach(j = 1:dim(gbmGrid)[1]) %dopar%{
    library(gbm)
    library(pROC)
    library(caret)
    set.seed(97)    
    gbmFit <- gbm(formula = value ~ .,            
                  distribution = "bernoulli", 
                  data = mthup,
                  n.trees = gbmGrid$n.trees[j],              
                  interaction.depth = gbmGrid$interaction.depth[j],  
                  shrinkage = gbmGrid$shrinkage[j], 
                  verbose = FALSE,
                  cv.folds = 10,
                  n.cores = 3
                  )  
    
    gbm_perf <- gbm.perf(gbmFit, method = "cv", plot.it = FALSE)
    gbmPred <- predict(gbmFit, newdata = mtht, n.trees = gbm_perf,type = "response")   
    pred <- ifelse(gbmPred > .5, 1, 0)
    list(grid = gbmGrid[j,], auc = auc(gbmPred, mtht$value), cmat = confusionMatrix(pred, mtht$value))
  }
  cmat[[i]] <- AUC
}

save(cmat, file = 'cmat_grid')

val <- list()
choice <- list()
for(i in 1:12){
  cm <- cmat[[i]]
  cm <- t(sapply(cm, function(x) t(data.frame(s= unlist(x)[c(1:4, 10,17,18)]))))
  cm <- apply(cm, 2, as.numeric)
  if(which.max(cm[,6]) == which.max(cm[,7])){
    choice[[i]] <- cm[which.max(cm[,6]),]
    }else if((cm[which.max(cm[,6]),6] - cm[which.max(cm[,7]),6]) > (cm[which.max(cm[,7]),7] - cm[which.max(cm[,6]),7])){
    choice[[i]] <- cm[which.max(cm[,6]),]
  }else{
    choice[[i]] <- cm[which.max(cm[,7]),]
  }
  val[[i]] <- cm
}

choice <- do.call(rbind, choice)
colnames(choice) <- c('interaction', 'ntrees', 'shrinkage','auc','accuracy', 'sensitivity', 'specificity')
save(choice, file = 'gbmChoice')


## now get the def choice!
cmat <- list()
monthFit <- list()
pdf(file = 'gbmfit.pdf')
for(i in 1:12){
  mth <- gmstbin %>% filter(month == i)
  mth <- mth[,-2]
  mtht <- gmstbin_test %>% filter(month == i)
  mtht <- mtht[,-2]
  mthup <- upSample(mth[,1:7], as.factor(mth$value), yname='value')
  mthup$value <- as.numeric(as.character(mthup$value))
  
  param <- choice[i, ]

  set.seed(97)    
  gbmFit <- gbm(formula = value ~ .,            #def best for hjuly
                distribution = "bernoulli", 
                data = mth,
                n.trees = param[2],              
                interaction.depth = param[1],  
                shrinkage = param[3], 
                verbose = FALSE,
                cv.folds = 10,
                n.cores = 4)   
  
  gbm_perf <- gbm.perf(gbmFit, method = "cv")
  summary(gbmFit)
  gbmPred <- predict(gbmFit, newdata = mtht, n.trees = gbm_perf,type = "response")
  pred <- ifelse(gbmPred > .5, 1, 0)
  
  cmat[[i]] <- confusionMatrix(pred, mtht$value)
  
  monthFit[[i]] <- gbmFit
  print(i)
}
dev.off()
save(monthFit, file = 'monthFit')
save(cmat, file = 'month_confmat')

