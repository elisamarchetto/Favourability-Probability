list.of.packages <- c("raster", "tidyverse", "sp", "sf", "virtualspecies", "ggplot2", "rgdal", "fuzzySim", "rasterVis", "viridis", "RStoolbox", "rnaturalearth", "scico","ranger","mgcv", "dismo", "gbm", "patchwork","FSA","ROCR", "groupdata2")
lapply(list.of.packages, library, character.only = TRUE)




####################### AUCs #########################
######################################################


AUCFunzGLM02 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}

AUCFunzGLM04 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}


AUCFunzGLM05 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
 Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}
  
AUCFunzGLM06 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
  
Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}

AUCFunzGLM08 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <- as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
  
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}
 
  
  
########### RF ########

AUCFunzRF02 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
Model<-ranger(training$pres ~., data= training, importance='impurity') 
    Pred<- predict(
      Model,
      data = validation[2:ncol(validation)],
      predict.all = FALSE,
      num.trees = Model$num.trees)
      
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16

    Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }
  
AUCFunzRF04 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
  
Model<-ranger(training$pres ~., data= training, importance='impurity') 
    Pred<- predict(
      Model,
      data = validation[2:ncol(validation)],
      predict.all = FALSE,
      num.trees = Model$num.trees)
      
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16

    Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

AUCFunzRF05 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

Model<-ranger(training$pres ~., data= training, importance='impurity') 
    Pred<- predict(
      Model,
      data = validation[2:ncol(validation)],
      predict.all = FALSE,
      num.trees = Model$num.trees)
      
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16

    Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }
  
  
AUCFunzRF06 <- function(x){

  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

Model<-ranger(training$pres ~., data= training, importance='impurity') 
    Pred<- predict(
      Model,
      data = validation[2:ncol(validation)],
      predict.all = FALSE,
      num.trees = Model$num.trees)
      
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16

    Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }
  
AUCFunzRF08 <- function(x){

df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <- as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

  Model<-ranger(training$pres ~., data= training, importance='impurity') 
    Pred<- predict(
      Model,
      data = validation[2:ncol(validation)],
      predict.all = FALSE,
      num.trees = Model$num.trees)
      
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16

    Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

  
  ########## GAM ###########
  
  AUCFunzGAM02 <- function(x){

  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

  sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
  
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

AUCFunzGAM04 <- function(x){

  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
  
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }


AUCFunzGAM05 <- function(x){
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
  
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

AUCFunzGAM06 <- function(x){

  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train
 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
  
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

AUCFunzGAM08 <- function(x){
 
  
df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <- as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
  
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }
  
  
  
########### GBM ###########


AUCFunzGBM02 <- function(x){
 

df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

  sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    Model <- gbm.step(data = training, 
                      gbm.x = names(training)[pred_cols],
                      gbm.y = names(training)[sp_cols], 
                      family = 'bernoulli',
                      tree.complexity = 5,
                      bag.fraction = 0.75,
                      learning.rate = 0.005,
                      verbose=F)
    prediction <- predict.gbm(Model, newdata = validation, n.trees=Model$gbm.call$best.trees, type="response")
    df.prediction <- data.frame(Pred=prediction)
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }

AUCFunzGBM04 <- function(x){


df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- data.frame(rbind(dfP_inds[[1]], dfP_inds[[3]]))
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  data.frame(rbind(dfA_inds[[2]], dfA_inds[[3]]))
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

  sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    Model <- gbm.step(data = training, 
                      gbm.x = names(training)[pred_cols],
                      gbm.y = names(training)[sp_cols], 
                      family = 'bernoulli',
                      tree.complexity = 5,
                      bag.fraction = 0.75,
                      learning.rate = 0.005,
                      verbose=F)
    prediction <- predict.gbm(Model, newdata = validation, n.trees=Model$gbm.call$best.trees, type="response")
    df.prediction <- data.frame(Pred=prediction)
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }



AUCFunzGBM05 <- function(x){

df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

  sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    Model <- gbm.step(data = training, 
                      gbm.x = names(training)[pred_cols],
                      gbm.y = names(training)[sp_cols], 
                      family = 'bernoulli',
                      tree.complexity = 5,
                      bag.fraction = 0.75,
                      learning.rate = 0.005,
                      verbose=F)
    prediction <- predict.gbm(Model, newdata = validation, n.trees=Model$gbm.call$best.trees, type="response")
    df.prediction <- data.frame(Pred=prediction)
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }


AUCFunzGBM06 <- function(x){


df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- as.data.frame(dfP_inds[2])
trainA0.8 <- as.data.frame(dfA_inds[1])
testA0.2 <-  as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    Model <- gbm.step(data = training, 
                      gbm.x = names(training)[pred_cols],
                      gbm.y = names(training)[sp_cols], 
                      family = 'bernoulli',
                      tree.complexity = 5,
                      bag.fraction = 0.75,
                      learning.rate = 0.005,
                      verbose=F)
    prediction <- predict.gbm(Model, newdata = validation, n.trees=Model$gbm.call$best.trees, type="response")
    df.prediction <- data.frame(Pred=prediction)
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }


AUCFunzGBM08 <- function(x){


df <- x$ModelDatabase
dfP <- df %>% filter(pres==1)
dfA <- df %>% filter(pres==0)
set.seed(999)
dfP_inds <- partition(dfP, p = c(train = 0.8, test = 0.2))
set.seed(999)
dfA_inds <- partition(dfA, p = c(train = 0.8, test = 0.2))
trainP0.8 <- as.data.frame(dfP_inds[1])
testP0.2 <- data.frame(rbind(dfP_inds[[2]], dfP_inds[[3]]))
trainA0.8 <- data.frame(rbind(dfA_inds[[1]], dfA_inds[[3]]))
testA0.2 <- as.data.frame(dfA_inds[2])
train <- rbind(trainP0.8, trainA0.8)
test <- rbind(testP0.2, testA0.2)
validation<-test
training<-train

 sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    Model <- gbm.step(data = training, 
                      gbm.x = names(training)[pred_cols],
                      gbm.y = names(training)[sp_cols], 
                      family = 'bernoulli',
                      tree.complexity = 5,
                      bag.fraction = 0.75,
                      learning.rate = 0.005,
                      verbose=F)
    prediction <- predict.gbm(Model, newdata = validation, n.trees=Model$gbm.call$best.trees, type="response")
    df.prediction <- data.frame(Pred=prediction)
    Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
    
    #ROC
    ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
    
    #AUC
    AUCperfProb <-performance(Prob, measure="auc")
    AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
    AUCperfProb <-round(AUCperfProb,4)
    
    return(AUCperfProb)
  }


#### Random ####

AUCFunz02GLM.R <- lapply(Rsdm02GLM, AUCFunzGLM02)
AUCFunz04GLM.R <- lapply(Rsdm04GLM, AUCFunzGLM04)
AUCFunz05GLM.R <- lapply(Rsdm05GLM, AUCFunzGLM05)
AUCFunz06GLM.R <- lapply(Rsdm06GLM, AUCFunzGLM06)
AUCFunz08GLM.R <- lapply(Rsdm08GLM, AUCFunzGLM08)
            

AUC02GLM.R <- data.frame(AUC02=unlist(AUCFunz02GLM.R))
AUC04GLM.R <- data.frame(AUC04=unlist(AUCFunz04GLM.R))
AUC05GLM.R <- data.frame(AUC05=unlist(AUCFunz05GLM.R))
AUC06GLM.R <- data.frame(AUC06=unlist(AUCFunz06GLM.R))
AUC08GLM.R <- data.frame(AUC08=unlist(AUCFunz08GLM.R))

AUCGLM.R <- cbind(AUC02GLM.R, AUC04GLM.R, AUC05GLM.R, AUC06GLM.R, AUC08GLM.R)
colnames(AUCGLM.R) <- c("0.2","0.4","0.5","0.6","0.8")
#write.csv(AUC, "AUC.csv")

AUCFunz02RF.R <- lapply(Rsdm02RF, AUCFunzRF02)
AUCFunz04RF.R <- lapply(Rsdm04RF, AUCFunzRF04)
AUCFunz05RF.R <- lapply(Rsdm05RF, AUCFunzRF05)
AUCFunz06RF.R <- lapply(Rsdm06RF, AUCFunzRF06)
AUCFunz08RF.R <- lapply(Rsdm08RF, AUCFunzRF08)
            

AUC02RF.R <- data.frame(AUC02=unlist(AUCFunz02RF.R))
AUC04RF.R <- data.frame(AUC04=unlist(AUCFunz04RF.R))
AUC05RF.R <- data.frame(AUC05=unlist(AUCFunz05RF.R))
AUC06RF.R <- data.frame(AUC06=unlist(AUCFunz06RF.R))
AUC08RF.R <- data.frame(AUC08=unlist(AUCFunz08RF.R))

AUCRF.R <- cbind(AUC02RF.R, AUC04RF.R, AUC05RF.R, AUC06RF.R, AUC08RF.R)
colnames(AUCRF.R) <- c("0.2","0.4","0.5","0.6","0.8")


AUCFunz02GAM.R <- lapply(Rsdm02GAM, AUCFunzGAM02)
AUCFunz04GAM.R <- lapply(Rsdm04GAM, AUCFunzGAM04)
AUCFunz05GAM.R <- lapply(Rsdm05GAM, AUCFunzGAM05)
AUCFunz06GAM.R <- lapply(Rsdm06GAM, AUCFunzGAM06)
AUCFunz08GAM.R <- lapply(Rsdm08GAM, AUCFunzGAM08)
            

AUC02GAM.R <- data.frame(AUC02=unlist(AUCFunz02GAM.R))
AUC04GAM.R <- data.frame(AUC04=unlist(AUCFunz04GAM.R))
AUC05GAM.R <- data.frame(AUC05=unlist(AUCFunz05GAM.R))
AUC06GAM.R <- data.frame(AUC06=unlist(AUCFunz06GAM.R))
AUC08GAM.R <- data.frame(AUC08=unlist(AUCFunz08GAM.R))

AUCGAM.R <- cbind(AUC02GAM.R, AUC04GAM.R, AUC05GAM.R, AUC06GAM.R, AUC08GAM.R)
colnames(AUCGAM.R) <- c("0.2","0.4","0.5","0.6","0.8")


AUCFunz02GBM.R <- lapply(Rsdm02GBM, AUCFunzGBM02)
AUCFunz04GBM.R <- lapply(Rsdm04GBM, AUCFunzGBM04)
AUCFunz05GBM.R <- lapply(Rsdm05GBM, AUCFunzGBM05)
AUCFunz06GBM.R <- lapply(Rsdm06GBM, AUCFunzGBM06)
AUCFunz08GBM.R <- lapply(Rsdm08GBM, AUCFunzGBM08)
            

AUC02GBM.R <- data.frame(AUC02=unlist(AUCFunz02GBM.R))
AUC04GBM.R <- data.frame(AUC04=unlist(AUCFunz04GBM.R))
AUC05GBM.R <- data.frame(AUC05=unlist(AUCFunz05GBM.R))
AUC06GBM.R <- data.frame(AUC06=unlist(AUCFunz06GBM.R))
AUC08GBM.R <- data.frame(AUC08=unlist(AUCFunz08GBM.R))

AUCGBM.R <- cbind(AUC02GBM.R, AUC04GBM.R, AUC05GBM.R, AUC06GBM.R, AUC08GBM.R)
colnames(AUCGBM.R) <- c("0.2","0.4","0.5","0.6","0.8")



Theme1<-theme(panel.background= element_rect(color="black", fill="white"),
              panel.grid.major = element_blank(),
              plot.title = element_text(size=8.5,face = 'bold',hjust = 0.15),
              legend.title=element_text(size=10),
              legend.text = element_text(size=10),
              legend.key.size = unit(0.5, 'cm'),
              axis.title.x = element_text(size = 10,face = 'bold'),
              axis.text.x = element_text(size = 10),
              axis.title.y = element_text(size = 10,face = 'bold'),
              axis.text.y = element_text(size = 10),
              plot.title.position ='plot')


aucGLMR_boxplot <- AUCGLM.R %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")


boxplotAUCGLM.R <- ggplot(aucGLMR_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="A") + Theme1


aucRFR_boxplot <- AUCRF.R %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCRF.R <- ggplot(aucRFR_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="C") + Theme1

aucGAMR_boxplot <- AUCGAM.R %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCGAM.R <- ggplot(aucGAMR_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="E") + Theme1


aucGBMR_boxplot <- AUCGBM.R %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCGBM.R <- ggplot(aucGBMR_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="G") + Theme1



#########################
#### Stratified ######
#########################

Ssdm.l.prev <- function(x){
  
  
  Ssdm02.l <- list()
  Ssdm04.l <- list()
  Ssdm05.l <- list()
  Ssdm06.l <- list()
  Ssdm08.l <- list()
  for(i in 1:length(x)) {
    
    
    
    for( j in 1:5) {
      if(j==1){
        
        Ssdm02 <- x[[i]][[j]]
        
        
        print(Ssdm02)
        
      }else if(j==2){
        
        Ssdm04 <- x[[i]][[j]]
        
        
        print(Ssdm04)
        
      }else if(j==3){
        
        
        Ssdm05 <- x[[i]][[j]]
        
        
        print(Ssdm05)
        
      }else if(j==4){
        
        
        Ssdm06 <- x[[i]][[j]]
        
        
        print(Ssdm06)
        
      }else if(j==5){
        
        
        Ssdm08 <- x[[i]][[j]]
        
        
        print( Ssdm08)
        
      }else{  
      }
      
      
    }
    
    Ssdm02.l[[i]] <- Ssdm02
    Ssdm04.l[[i]] <- Ssdm04
    Ssdm05.l[[i]] <- Ssdm05
    Ssdm06.l[[i]] <- Ssdm06
    Ssdm08.l[[i]] <- Ssdm08
    
  }
  
  Outputs.l = list( Ssdm02.l, Ssdm04.l, Ssdm05.l, Ssdm06.l, Ssdm08.l)
  
  
  return(Outputs.l)
}

Ssdm.pGLM <- Ssdm.l.prev(SsdmGLM)

Ssdm02GLM <- Ssdm.pGLM[[1]]
Ssdm04GLM <- Ssdm.pGLM[[2]]
Ssdm05GLM <- Ssdm.pGLM[[3]]
Ssdm06GLM <- Ssdm.pGLM[[4]]
Ssdm08GLM <- Ssdm.pGLM[[5]]
  
Ssdm.pRF <- Ssdm.l.prev(SsdmRF)

Ssdm02RF <- Ssdm.pRF[[1]]
Ssdm04RF <- Ssdm.pRF[[2]]
Ssdm05RF <- Ssdm.pRF[[3]]
Ssdm06RF <- Ssdm.pRF[[4]]
Ssdm08RF <- Ssdm.pRF[[5]]  
  
Ssdm.pGAM <- Ssdm.l.prev(SsdmGAM)

Ssdm02GAM <- Ssdm.pGAM[[1]]
Ssdm04GAM <- Ssdm.pGAM[[2]]
Ssdm05GAM <- Ssdm.pGAM[[3]]
Ssdm06GAM <- Ssdm.pGAM[[4]]
Ssdm08GAM <- Ssdm.pGAM[[5]]
  
Ssdm.pGBM <- Ssdm.l.prev(SsdmGBM)

Ssdm02GBM <- Ssdm.pGBM[[1]]
Ssdm04GBM <- Ssdm.pGBM[[2]]
Ssdm05GBM <- Ssdm.pGBM[[3]]
Ssdm06GBM <- Ssdm.pGBM[[4]]
Ssdm08GBM <- Ssdm.pGBM[[5]]

 

AUCFunz02GLM.S <- lapply(Ssdm02GLM, AUCFunzGLM02)
AUCFunz04GLM.S <- lapply(Ssdm04GLM, AUCFunzGLM04)
AUCFunz05GLM.S <- lapply(Ssdm05GLM, AUCFunzGLM05)
AUCFunz06GLM.S <- lapply(Ssdm06GLM, AUCFunzGLM06)
AUCFunz08GLM.S <- lapply(Ssdm08GLM, AUCFunzGLM08)
            

AUC02GLM.S <- data.frame(AUC02=unlist(AUCFunz02GLM.S))
AUC04GLM.S <- data.frame(AUC04=unlist(AUCFunz04GLM.S))
AUC05GLM.S <- data.frame(AUC05=unlist(AUCFunz05GLM.S))
AUC06GLM.S <- data.frame(AUC06=unlist(AUCFunz06GLM.S))
AUC08GLM.S <- data.frame(AUC08=unlist(AUCFunz08GLM.S))

AUCGLM.S <- cbind(AUC02GLM.S, AUC04GLM.S, AUC05GLM.S, AUC06GLM.S, AUC08GLM.S)
colnames(AUCGLM.S) <- c("0.2","0.4","0.5","0.6","0.8")
#write.csv(AUC, "AUC.csv")


AUCFunz02RF.S <- lapply(Ssdm02RF, AUCFunzRF02)
AUCFunz04RF.S <- lapply(Ssdm04RF, AUCFunzRF04)
AUCFunz05RF.S <- lapply(Ssdm05RF, AUCFunzRF05)
AUCFunz06RF.S <- lapply(Ssdm06RF, AUCFunzRF06)
AUCFunz08RF.S <- lapply(Ssdm08RF, AUCFunzRF08)
            

AUC02RF.S <- data.frame(AUC02=unlist(AUCFunz02RF.S))
AUC04RF.S <- data.frame(AUC04=unlist(AUCFunz04RF.S))
AUC05RF.S <- data.frame(AUC05=unlist(AUCFunz05RF.S))
AUC06RF.S <- data.frame(AUC06=unlist(AUCFunz06RF.S))
AUC08RF.S <- data.frame(AUC08=unlist(AUCFunz08RF.S))

AUCRF.S <- cbind(AUC02RF.S, AUC04RF.S, AUC05RF.S, AUC06RF.S, AUC08RF.S)
colnames(AUCRF.S) <- c("0.2","0.4","0.5","0.6","0.8")


AUCFunz02GAM.S <- lapply(Ssdm02GAM, AUCFunzGAM02)
AUCFunz04GAM.S <- lapply(Ssdm04GAM, AUCFunzGAM04)
AUCFunz05GAM.S <- lapply(Ssdm05GAM, AUCFunzGAM05)
AUCFunz06GAM.S <- lapply(Ssdm06GAM, AUCFunzGAM06)
AUCFunz08GAM.S <- lapply(Ssdm08GAM, AUCFunzGAM08)
            

AUC02GAM.S <- data.frame(AUC02=unlist(AUCFunz02GAM.S))
AUC04GAM.S <- data.frame(AUC04=unlist(AUCFunz04GAM.S))
AUC05GAM.S <- data.frame(AUC05=unlist(AUCFunz05GAM.S))
AUC06GAM.S <- data.frame(AUC06=unlist(AUCFunz06GAM.S))
AUC08GAM.S <- data.frame(AUC08=unlist(AUCFunz08GAM.S))

AUCGAM.S <- cbind(AUC02GAM.S, AUC04GAM.S, AUC05GAM.S, AUC06GAM.S, AUC08GAM.S)
colnames(AUCGAM.S) <- c("0.2","0.4","0.5","0.6","0.8")

AUCFunz02GBM.S <- lapply(Ssdm02GBM, AUCFunzGBM02)
AUCFunz04GBM.S <- lapply(Ssdm04GBM, AUCFunzGBM04)
AUCFunz05GBM.S <- lapply(Ssdm05GBM, AUCFunzGBM05)
AUCFunz06GBM.S <- lapply(Ssdm06GBM, AUCFunzGBM06)
AUCFunz08GBM.S <- lapply(Ssdm08GBM, AUCFunzGBM08)
            

AUC02GBM.S <- data.frame(AUC02=unlist(AUCFunz02GBM.S))
AUC04GBM.S <- data.frame(AUC04=unlist(AUCFunz04GBM.S))
AUC05GBM.S <- data.frame(AUC05=unlist(AUCFunz05GBM.S))
AUC06GBM.S <- data.frame(AUC06=unlist(AUCFunz06GBM.S))
AUC08GBM.S <- data.frame(AUC08=unlist(AUCFunz08GBM.S))

AUCGBM.S <- cbind(AUC02GBM.S, AUC04GBM.S, AUC05GBM.S, AUC06GBM.S, AUC08GBM.S)
colnames(AUCGBM.S) <- c("0.2","0.4","0.5","0.6","0.8")


aucGLMS_boxplot <- AUCGLM.S %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCGLM.S <- ggplot(aucGLMS_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="B") + Theme1


aucRFS_boxplot <- AUCRF.S %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCRF.S <- ggplot(aucRFS_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="D") + Theme1

aucGAMS_boxplot <- AUCGAM.S %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCGAM.S <- ggplot(aucGAMS_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="F") + Theme1


aucGBMS_boxplot <- AUCGBM.S %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")

boxplotAUCGBM.S <- ggplot(aucGBMS_boxplot, aes(x=prevalence, y=values))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")+
  labs(x = "Sample prevalence", y="AUC", title ="H") + Theme1


################ Distribution values of 50 virtual species/ Favourability vs Probability ####################
#############################################################################################################
  
############# GLM #############
  
  listProbGLM <- function(x) {
  stackProb <- stack(x$Raster$pres_P)
  
  return(stackProb)
}

listProb02GLM <- lapply(Rsdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.R <- data.frame(values = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM )%>%drop_na() 
df_fav02GLM.R <- data.frame(values = unlist(df_fav02GLM))


listProb04GLM <- lapply(Rsdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.R <- data.frame(values = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na() 
df_fav04GLM.R <- data.frame(values = unlist(df_fav04GLM))

listProb05GLM <- lapply(Rsdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.R <- data.frame(values = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na() 
df_fav05GLM.R <- data.frame(values = unlist(df_fav05GLM))


listProb06GLM <- lapply(Rsdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.R <- data.frame(values = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na() 
df_fav06GLM.R <- data.frame(values = unlist(df_fav06GLM))


listProb08GLM <- lapply(Rsdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.R <- data.frame(values = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na() 
df_fav08GLM.R <- data.frame(values = unlist(df_fav08GLM))

# Stratified
listProb02GLM <- lapply(Ssdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.S <- data.frame(values = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM)%>%drop_na() 
df_fav02GLM.S <- data.frame(values = unlist(df_fav02GLM))


listProb04GLM <- lapply(Ssdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.S <- data.frame(values = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na() 
df_fav04GLM.S <- data.frame(values = unlist(df_fav04GLM))

listProb05GLM <- lapply(Ssdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.S <- data.frame(values = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na() 
df_fav05GLM.S <- data.frame(values = unlist(df_fav05GLM))


listProb06GLM <- lapply(Ssdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.S <- data.frame(values = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na() 
df_fav06GLM.S <- data.frame(values = unlist(df_fav06GLM))


listProb08GLM <- lapply(Ssdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.S <- data.frame(values = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na() 
df_fav08GLM.S <- data.frame(values = unlist(df_fav08GLM))

###### #######

df_fav08GLM.R$prevalence <- "F0.8"
df_fav06GLM.R$prevalence <- "F0.6"
df_fav05GLM.R$prevalence <- "F0.5"
df_fav04GLM.R$prevalence <- "F0.4"
df_fav02GLM.R$prevalence <- "F0.2"

df_favGLM.R <- as.data.frame(rbind(df_fav02GLM.R, df_fav04GLM.R, df_fav05GLM.R, df_fav06GLM.R, df_fav08GLM.R))



df_prob08GLM.R$prevalence <- "P0.8"
df_prob06GLM.R$prevalence <- "P0.6"
df_prob05GLM.R$prevalence <- "P0.5"
df_prob04GLM.R$prevalence <- "P0.4"
df_prob02GLM.R$prevalence <- "P0.2"

df_probGLM.R <- as.data.frame(rbind(df_prob02GLM.R, df_prob04GLM.R, df_prob05GLM.R, df_prob06GLM.R, df_prob08GLM.R))

df_fav08GLM.S$prevalence <- "F0.8"
df_fav06GLM.S$prevalence <- "F0.6"
df_fav05GLM.S$prevalence <- "F0.5"
df_fav04GLM.S$prevalence <- "F0.4"
df_fav02GLM.S$prevalence <- "F0.2"

df_favGLM.S <- as.data.frame(rbind(df_fav02GLM.S, df_fav04GLM.S, df_fav05GLM.S, df_fav06GLM.S, df_fav08GLM.S))



df_prob08GLM.S$prevalence <- "P0.8"
df_prob06GLM.S$prevalence <- "P0.6"
df_prob05GLM.S$prevalence <- "P0.5"
df_prob04GLM.S$prevalence <- "P0.4"
df_prob02GLM.S$prevalence <- "P0.2"

df_probGLM.S <- as.data.frame(rbind(df_prob02GLM.S, df_prob04GLM.S, df_prob05GLM.S, df_prob06GLM.S, df_prob08GLM.S)) 

df_rFGLM.R <- rbind(df_probGLM.R, df_favGLM.R)
                  
df_rFGLM.S <- rbind(df_probGLM.S, df_favGLM.S)                  

df_boxplotGLM.R <- mutate(df_rFGLM.R, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))

df_boxplotGLM.S <- mutate(df_rFGLM.S, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))
                  
Theme<-theme(panel.background= element_rect(color="black", fill="white"),
             panel.grid.major = element_blank(),
             plot.title = element_text(size=15,face = 'bold', hjust = 0.12),
             legend.title=element_text(size=15, face = 'bold'),
             legend.text = element_text(size=15, face = 'bold'),
             legend.key.size = unit(1.0, 'cm'),
             axis.title.x =  element_text(size = 18, face = 'bold'),
             axis.text.x = element_text(size = 15, face = 'bold'),
             axis.title.y = element_text(size = 18, face = 'bold'),
             axis.text.y = element_text(size = 15, face = 'bold'),
             plot.title.position ='plot',
             legend.position = 'bottom',
             )

ggplot(df_boxplotGLM.R, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="A")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> glm1

 ggplot(df_boxplotGLM.S, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="B")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> glm2

 glm <- glm1 + glm2 + plot_annotation(title = "GLM") + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = glm,
filename = "glm.jpg",
width = 10,
 height = 8,
dpi = 600) 
                  
############ RF ############
                  
 listProbRF <- function(x) {
  stackProb <- stack(x$Raster$Pred.predictions)
  
  return(stackProb)
}


# Random 

listProb02RF <- lapply(Rsdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.R <- data.frame(values = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)%>%drop_na()
df_fav02RF.R <- data.frame(values = unlist(df_fav02RF))


listProb04RF <- lapply(Rsdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.R <- data.frame(values = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)%>%drop_na()
df_fav04RF.R <- data.frame(values = unlist(df_fav04RF))

listProb05RF <- lapply(Rsdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.R <- data.frame(values = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)%>%drop_na()
df_fav05RF.R <- data.frame(values = unlist(df_fav05RF))


listProb06RF <- lapply(Rsdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.R <- data.frame(values = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)%>%drop_na()
df_fav06RF.R <- data.frame(values = unlist(df_fav06RF))


listProb08RF <- lapply(Rsdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.R <- data.frame(values = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)%>%drop_na()
df_fav08RF.R <- data.frame(values = unlist(df_fav08RF))

# Stratified

listProb02RF <- lapply(Ssdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.S <- data.frame(values = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)%>%drop_na()
df_fav02RF.S <- data.frame(values = unlist(df_fav02RF))


listProb04RF <- lapply(Ssdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.S <- data.frame(values = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)%>%drop_na()
df_fav04RF.S <- data.frame(values = unlist(df_fav04RF))

listProb05RF <- lapply(Ssdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.S <- data.frame(values = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)%>%drop_na()
df_fav05RF.S <- data.frame(values = unlist(df_fav05RF))


listProb06RF <- lapply(Ssdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.S <- data.frame(values = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)%>%drop_na()
df_fav06RF.S <- data.frame(values = unlist(df_fav06RF))


listProb08RF <- lapply(Ssdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.S <- data.frame(values = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)%>%drop_na()
df_fav08RF.S <- data.frame(values = unlist(df_fav08RF))

###### #######

df_fav08RF.R$prevalence <- "F0.8"
df_fav06RF.R$prevalence <- "F0.6"
df_fav05RF.R$prevalence <- "F0.5"
df_fav04RF.R$prevalence <- "F0.4"
df_fav02RF.R$prevalence <- "F0.2"

df_favRF.R <- as.data.frame(rbind(df_fav02RF.R, df_fav04RF.R, df_fav05RF.R, df_fav06RF.R, df_fav08RF.R))



df_prob08RF.R$prevalence <- "P0.8"
df_prob06RF.R$prevalence <- "P0.6"
df_prob05RF.R$prevalence <- "P0.5"
df_prob04RF.R$prevalence <- "P0.4"
df_prob02RF.R$prevalence <- "P0.2"

df_probRF.R <- as.data.frame(rbind(df_prob02RF.R, df_prob04RF.R, df_prob05RF.R, df_prob06RF.R, df_prob08RF.R))



df_fav08RF.S$prevalence <- "F0.8"
df_fav06RF.S$prevalence <- "F0.6"
df_fav05RF.S$prevalence <- "F0.5"
df_fav04RF.S$prevalence <- "F0.4"
df_fav02RF.S$prevalence <- "F0.2"

df_favRF.S <- as.data.frame(rbind(df_fav02RF.S, df_fav04RF.S, df_fav05RF.S, df_fav06RF.S, df_fav08RF.S))



df_prob08RF.S$prevalence <- "P0.8"
df_prob06RF.S$prevalence <- "P0.6"
df_prob05RF.S$prevalence <- "P0.5"
df_prob04RF.S$prevalence <- "P0.4"
df_prob02RF.S$prevalence <- "P0.2"

df_probRF.S <- as.data.frame(rbind(df_prob02RF.S, df_prob04RF.S, df_prob05RF.S, df_prob06RF.S, df_prob08RF.S))
  

df_rFRF.R <- rbind(df_probRF.R, df_favRF.R)
                  
df_rFRF.S <- rbind(df_probRF.S, df_favRF.S)                  

df_boxplotRF.R <- mutate(df_rFRF.R, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))

df_boxplotRF.S <- mutate(df_rFRF.S, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))
                  
Theme<-theme(panel.background= element_rect(color="black", fill="white"),
             panel.grid.major = element_blank(),
             plot.title = element_text(size=15,face = 'bold', hjust = 0.12),
             legend.title=element_text(size=15, face = 'bold'),
             legend.text = element_text(size=15, face = 'bold'),
             legend.key.size = unit(1.0, 'cm'),
             axis.title.x =  element_text(size = 18, face = 'bold'),
             axis.text.x = element_text(size = 15, face = 'bold'),
             axis.title.y = element_text(size = 18, face = 'bold'),
             axis.text.y = element_text(size = 15, face = 'bold'),
             plot.title.position ='plot',
             legend.position = 'bottom',
             )

ggplot(df_boxplotRF.R, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="C")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> rf1

 ggplot(df_boxplotRF.S, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="D")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> rf2

 rf<- rf1 + rf2 + plot_annotation(title = "RF") + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = rf,
filename = "rf.jpg",
width = 10,
 height = 8,
dpi = 600) 
          
################## GAM  ##########################                 
                 
 listProbGAM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GAM <- lapply(Rsdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.R <- data.frame(values = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.R <- data.frame(values = unlist(df_fav02GAM))


listProb04GAM <- lapply(Rsdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.R <- data.frame(values = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.R <- data.frame(values = unlist(df_fav04GAM))

listProb05GAM <- lapply(Rsdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.R <- data.frame(values = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.R <- data.frame(values = unlist(df_fav05GAM))


listProb06GAM <- lapply(Rsdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.R <- data.frame(values = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.R <- data.frame(values = unlist(df_fav06GAM))


listProb08GAM <- lapply(Rsdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.R <- data.frame(values = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.R <- data.frame(values = unlist(df_fav08GAM))

# Stratified

listProb02GAM <- lapply(Ssdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.S <- data.frame(values = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.S <- data.frame(values = unlist(df_fav02GAM))


listProb04GAM <- lapply(Ssdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.S <- data.frame(values = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.S <- data.frame(values = unlist(df_fav04GAM))

listProb05GAM <- lapply(Ssdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.S <- data.frame(values = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.S <- data.frame(values = unlist(df_fav05GAM))


listProb06GAM <- lapply(Ssdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.S <- data.frame(values = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.S <- data.frame(values = unlist(df_fav06GAM))


listProb08GAM <- lapply(Ssdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.S <- data.frame(values = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.S <- data.frame(values = unlist(df_fav08GAM))

###### #######

df_fav08GAM.R$prevalence <- "F0.8"
df_fav06GAM.R$prevalence <- "F0.6"
df_fav05GAM.R$prevalence <- "F0.5"
df_fav04GAM.R$prevalence <- "F0.4"
df_fav02GAM.R$prevalence <- "F0.2"

df_favGAM.R <- as.data.frame(rbind(df_fav02GAM.R, df_fav04GAM.R, df_fav05GAM.R, df_fav06GAM.R, df_fav08GAM.R))



df_prob08GAM.R$prevalence <- "P0.8"
df_prob06GAM.R$prevalence <- "P0.6"
df_prob05GAM.R$prevalence <- "P0.5"
df_prob04GAM.R$prevalence <- "P0.4"
df_prob02GAM.R$prevalence <- "P0.2"

df_probGAM.R <- as.data.frame(rbind(df_prob02GAM.R, df_prob04GAM.R, df_prob05GAM.R, df_prob06GAM.R, df_prob08GAM.R))



df_fav08GAM.S$prevalence <- "F0.8"
df_fav06GAM.S$prevalence <- "F0.6"
df_fav05GAM.S$prevalence <- "F0.5"
df_fav04GAM.S$prevalence <- "F0.4"
df_fav02GAM.S$prevalence <- "F0.2"

df_favGAM.S <- as.data.frame(rbind(df_fav02GAM.S, df_fav04GAM.S, df_fav05GAM.S, df_fav06GAM.S, df_fav08GAM.S))



df_prob08GAM.S$prevalence <- "P0.8"
df_prob06GAM.S$prevalence <- "P0.6"
df_prob05GAM.S$prevalence <- "P0.5"
df_prob04GAM.S$prevalence <- "P0.4"
df_prob02GAM.S$prevalence <- "P0.2"

df_probGAM.S <- as.data.frame(rbind(df_prob02GAM.S, df_prob04GAM.S, df_prob05GAM.S, df_prob06GAM.S, df_prob08GAM.S))                 
                 
                 
 df_rFGAM.R <- rbind(df_probGAM.R, df_favGAM.R)
                  
df_rFGAM.S <- rbind(df_probGAM.S, df_favGAM.S)                  

df_boxplotGAM.R <- mutate(df_rFGAM.R, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))

df_boxplotGAM.S <- mutate(df_rFGAM.S, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))
                  
Theme<-theme(panel.background= element_rect(color="black", fill="white"),
             panel.grid.major = element_blank(),
             plot.title = element_text(size=15,face = 'bold', hjust = 0.12),
             legend.title=element_text(size=15, face = 'bold'),
             legend.text = element_text(size=15, face = 'bold'),
             legend.key.size = unit(1.0, 'cm'),
             axis.title.x =  element_text(size = 18, face = 'bold'),
             axis.text.x = element_text(size = 15, face = 'bold'),
             axis.title.y = element_text(size = 18, face = 'bold'),
             axis.text.y = element_text(size = 15, face = 'bold'),
             plot.title.position ='plot',
             legend.position = 'bottom',
             )

ggplot(df_boxplotGAM.R, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="E")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> gam1

 ggplot(df_boxplotGAM.S, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="F")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> gam2

 gam <- gam1 + gam2 + plot_annotation(title = "GAM") + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = gam,
filename = "gam.jpg",
width = 10,
 height = 8,
dpi = 600) 
 
                  #################### GBM ##################
                  
listProbGBM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GBM <- lapply(Rsdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.R <- data.frame(values = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.R <- data.frame(values = unlist(df_fav02GBM))


listProb04GBM <- lapply(Rsdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.R <- data.frame(values = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.R <- data.frame(values = unlist(df_fav04GBM))

listProb05GBM <- lapply(Rsdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.R <- data.frame(values = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.R <- data.frame(values = unlist(df_fav05GBM))


listProb06GBM <- lapply(Rsdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.R <- data.frame(values = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.R <- data.frame(values = unlist(df_fav06GBM))


listProb08GBM <- lapply(Rsdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.R <- data.frame(values = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.R <- data.frame(values = unlist(df_fav08GBM))

# Stratified

listProb02GBM <- lapply(Ssdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.S <- data.frame(values = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.S <- data.frame(values = unlist(df_fav02GBM))


listProb04GBM <- lapply(Ssdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.S <- data.frame(values = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.S <- data.frame(values = unlist(df_fav04GBM))

listProb05GBM <- lapply(Ssdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.S <- data.frame(values = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.S <- data.frame(values = unlist(df_fav05GBM))


listProb06GBM <- lapply(Ssdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.S <- data.frame(values = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.S <- data.frame(values = unlist(df_fav06GBM))


listProb08GBM <- lapply(Ssdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.S <- data.frame(values = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.S <- data.frame(values = unlist(df_fav08GBM))

###### #######

df_fav08GBM.R$prevalence <- "F0.8"
df_fav06GBM.R$prevalence <- "F0.6"
df_fav05GBM.R$prevalence <- "F0.5"
df_fav04GBM.R$prevalence <- "F0.4"
df_fav02GBM.R$prevalence <- "F0.2"

df_favGBM.R <- as.data.frame(rbind(df_fav02GBM.R, df_fav04GBM.R, df_fav05GBM.R, df_fav06GBM.R, df_fav08GBM.R))



df_prob08GBM.R$prevalence <- "P0.8"
df_prob06GBM.R$prevalence <- "P0.6"
df_prob05GBM.R$prevalence <- "P0.5"
df_prob04GBM.R$prevalence <- "P0.4"
df_prob02GBM.R$prevalence <- "P0.2"

df_probGBM.R <- as.data.frame(rbind(df_prob02GBM.R, df_prob04GBM.R, df_prob05GBM.R, df_prob06GBM.R, df_prob08GBM.R))





df_fav08GBM.S$prevalence <- "F0.8"
df_fav06GBM.S$prevalence <- "F0.6"
df_fav05GBM.S$prevalence <- "F0.5"
df_fav04GBM.S$prevalence <- "F0.4"
df_fav02GBM.S$prevalence <- "F0.2"

df_favGBM.S <- as.data.frame(rbind(df_fav02GBM.S, df_fav04GBM.S, df_fav05GBM.S, df_fav06GBM.S, df_fav08GBM.S))



df_prob08GBM.S$prevalence <- "P0.8"
df_prob06GBM.S$prevalence <- "P0.6"
df_prob05GBM.S$prevalence <- "P0.5"
df_prob04GBM.S$prevalence <- "P0.4"
df_prob02GBM.S$prevalence <- "P0.2"

df_probGBM.S <- as.data.frame(rbind(df_prob02GBM.S, df_prob04GBM.S, df_prob05GBM.S, df_prob06GBM.S, df_prob08GBM.S)) 
                                    
                 
 df_rFGBM.R <- rbind(df_probGBM.R, df_favGBM.R)
                  
df_rFGBM.S <- rbind(df_probGBM.S, df_favGBM.S)                  

df_boxplotGBM.R <- mutate(df_rFGBM.R, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))

df_boxplotGBM.S <- mutate(df_rFGBM.S, Metric = ifelse(grepl("P", prevalence, ignore.case = TRUE), "Probability", "Favourability"))
                  
Theme<-theme(panel.background= element_rect(color="black", fill="white"),
             panel.grid.major = element_blank(),
             plot.title = element_text(size=15,face = 'bold', hjust = 0.12),
             legend.title=element_text(size=15, face = 'bold'),
             legend.text = element_text(size=15, face = 'bold'),
             legend.key.size = unit(1.0, 'cm'),
             axis.title.x =  element_text(size = 18, face = 'bold'),
             axis.text.x = element_text(size = 15, face = 'bold'),
             axis.title.y = element_text(size = 18, face = 'bold'),
             axis.text.y = element_text(size = 15, face = 'bold'),
             plot.title.position ='plot',
             legend.position = 'bottom',
             )

ggplot(df_boxplotGBM.R, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="G")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> gbm1

 ggplot(df_boxplotGBM.S, aes(x=prevalence, y=values, fill= Metric))+
  stat_boxplot(coef = 1.5, geom = 'errorbar', width=0.2)+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")+
  labs( y="Values", x="Sample prevalence", fill="Model", title ="H")+
  scale_fill_manual(values=c( "#E69F00", "#56B4E9"))  + Theme -> gbm2

 gbm <- gbm1 + gbm2 + plot_annotation(title = "BRT") + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = gbm,
filename = "gbm.jpg",
width = 10,
 height = 8,
dpi = 600)                  
                  
  
########################## Kruskal-Wallis Test and Dunn's Test #############################
############################################################################################                  

 ########### Kruskal_Wallis Test #########
                  
 #########  GLM  ##########
  listProbGLM <- function(x) {
  stackProb <- stack(x$Raster$pres_P)
  
  return(stackProb)
}

listProb02GLM <- lapply(Rsdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.R <- data.frame(values = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM )%>%drop_na() 
df_fav02GLM.R <- data.frame(values = unlist(df_fav02GLM))


listProb04GLM <- lapply(Rsdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.R <- data.frame(values = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na() 
df_fav04GLM.R <- data.frame(values = unlist(df_fav04GLM))

listProb05GLM <- lapply(Rsdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.R <- data.frame(values = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na() 
df_fav05GLM.R <- data.frame(values = unlist(df_fav05GLM))


listProb06GLM <- lapply(Rsdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.R <- data.frame(values = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na() 
df_fav06GLM.R <- data.frame(values = unlist(df_fav06GLM))


listProb08GLM <- lapply(Rsdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.R <- data.frame(values = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na() 
df_fav08GLM.R <- data.frame(values = unlist(df_fav08GLM))

# Stratified
listProb02GLM <- lapply(Ssdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.S <- data.frame(values = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM)%>%drop_na() 
df_fav02GLM.S <- data.frame(values = unlist(df_fav02GLM))
                  
# Stratified

listProb02GLM <- lapply(Ssdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.S <- data.frame(values = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM)%>%drop_na()
df_fav02GLM.S <- data.frame(values = unlist(df_fav02GLM))


listProb04GLM <- lapply(Ssdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.S <- data.frame(values = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na()
df_fav04GLM.S <- data.frame(values = unlist(df_fav04GLM))

listProb05GLM <- lapply(Ssdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.S <- data.frame(values = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na()
df_fav05GLM.S <- data.frame(values = unlist(df_fav05GLM))


listProb06GLM <- lapply(Ssdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.S <- data.frame(values = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na()
df_fav06GLM.S <- data.frame(values = unlist(df_fav06GLM))


listProb08GLM <- lapply(Ssdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.S <- data.frame(values = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na()
df_fav08GLM.S <- data.frame(values = unlist(df_fav08GLM))

###### #######

df_fav08GLM.R$prevalence <- "F0.8"
df_fav06GLM.R$prevalence <- "F0.6"
df_fav05GLM.R$prevalence <- "F0.5"
df_fav04GLM.R$prevalence <- "F0.4"
df_fav02GLM.R$prevalence <- "F0.2"

df_favGLM.R <- as.data.frame(rbind(df_fav02GLM.R, df_fav04GLM.R, df_fav05GLM.R, df_fav06GLM.R, df_fav08GLM.R))



df_prob08GLM.R$prevalence <- "P0.8"
df_prob06GLM.R$prevalence <- "P0.6"
df_prob05GLM.R$prevalence <- "P0.5"
df_prob04GLM.R$prevalence <- "P0.4"
df_prob02GLM.R$prevalence <- "P0.2"

df_probGLM.R <- as.data.frame(rbind(df_prob02GLM.R, df_prob04GLM.R, df_prob05GLM.R, df_prob06GLM.R, df_prob08GLM.R))






Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGLM.R)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGLM.R)




df_fav08GLM.S$prevalence <- "F0.8"
df_fav06GLM.S$prevalence <- "F0.6"
df_fav05GLM.S$prevalence <- "F0.5"
df_fav04GLM.S$prevalence <- "F0.4"
df_fav02GLM.S$prevalence <- "F0.2"

df_favGLM.S <- as.data.frame(rbind(df_fav02GLM.S, df_fav04GLM.S, df_fav05GLM.S, df_fav06GLM.S, df_fav08GLM.S))



df_prob08GLM.S$prevalence <- "P0.8"
df_prob06GLM.S$prevalence <- "P0.6"
df_prob05GLM.S$prevalence <- "P0.5"
df_prob04GLM.S$prevalence <- "P0.4"
df_prob02GLM.S$prevalence <- "P0.2"

df_probGLM.S <- as.data.frame(rbind(df_prob02GLM.S, df_prob04GLM.S, df_prob05GLM.S, df_prob06GLM.S, df_prob08GLM.S)) 

Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGLM.S)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGLM.S)

                  

######## RF ########

listProbRF <- function(x) {
  stackProb <- stack(x$Raster$Pred.predictions)
  
  return(stackProb)
}



# Random 

listProb02RF <- lapply(Rsdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.R <- data.frame(values = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)%>%drop_na()
df_fav02RF.R <- data.frame(values = unlist(df_fav02RF))


listProb04RF <- lapply(Rsdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.R <- data.frame(values = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)%>%drop_na()
df_fav04RF.R <- data.frame(values = unlist(df_fav04RF))

listProb05RF <- lapply(Rsdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.R <- data.frame(values = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)%>%drop_na()
df_fav05RF.R <- data.frame(values = unlist(df_fav05RF))


listProb06RF <- lapply(Rsdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.R <- data.frame(values = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)%>%drop_na()
df_fav06RF.R <- data.frame(values = unlist(df_fav06RF))


listProb08RF <- lapply(Rsdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.R <- data.frame(values = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)%>%drop_na()
df_fav08RF.R <- data.frame(values = unlist(df_fav08RF))

# Stratified

listProb02RF <- lapply(Ssdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.S <- data.frame(values = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)%>%drop_na()
df_fav02RF.S <- data.frame(values = unlist(df_fav02RF))


listProb04RF <- lapply(Ssdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.S <- data.frame(values = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)%>%drop_na()
df_fav04RF.S <- data.frame(values = unlist(df_fav04RF))

listProb05RF <- lapply(Ssdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.S <- data.frame(values = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)%>%drop_na()
df_fav05RF.S <- data.frame(values = unlist(df_fav05RF))


listProb06RF <- lapply(Ssdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.S <- data.frame(values = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)%>%drop_na()
df_fav06RF.S <- data.frame(values = unlist(df_fav06RF))


listProb08RF <- lapply(Ssdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.S <- data.frame(values = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)%>%drop_na()
df_fav08RF.S <- data.frame(values = unlist(df_fav08RF))

###### #######

df_fav08RF.R$prevalence <- "F0.8"
df_fav06RF.R$prevalence <- "F0.6"
df_fav05RF.R$prevalence <- "F0.5"
df_fav04RF.R$prevalence <- "F0.4"
df_fav02RF.R$prevalence <- "F0.2"

df_favRF.R <- as.data.frame(rbind(df_fav02RF.R, df_fav04RF.R, df_fav05RF.R, df_fav06RF.R, df_fav08RF.R))



df_prob08RF.R$prevalence <- "P0.8"
df_prob06RF.R$prevalence <- "P0.6"
df_prob05RF.R$prevalence <- "P0.5"
df_prob04RF.R$prevalence <- "P0.4"
df_prob02RF.R$prevalence <- "P0.2"

df_probRF.R <- as.data.frame(rbind(df_prob02RF.R, df_prob04RF.R, df_prob05RF.R, df_prob06RF.R, df_prob08RF.R))






Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favRF.R)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probRF.R)


Kruscal_prevR_RF <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevR_RF,"Kruscal_prevRandom_RF.csv")


df_fav08RF.S$prevalence <- "F0.8"
df_fav06RF.S$prevalence <- "F0.6"
df_fav05RF.S$prevalence <- "F0.5"
df_fav04RF.S$prevalence <- "F0.4"
df_fav02RF.S$prevalence <- "F0.2"

df_favRF.S <- as.data.frame(rbind(df_fav02RF.S, df_fav04RF.S, df_fav05RF.S, df_fav06RF.S, df_fav08RF.S))



df_prob08RF.S$prevalence <- "P0.8"
df_prob06RF.S$prevalence <- "P0.6"
df_prob05RF.S$prevalence <- "P0.5"
df_prob04RF.S$prevalence <- "P0.4"
df_prob02RF.S$prevalence <- "P0.2"

df_probRF.S <- as.data.frame(rbind(df_prob02RF.S, df_prob04RF.S, df_prob05RF.S, df_prob06RF.S, df_prob08RF.S)) 

Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favRF.S)
Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probRF.S)
                 
Kruscal_prevS_RF <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevS_RF,"Kruscal_prevStratified_RF.csv")
                 
                  ####### GAM #########
                 
                

listProbGAM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GAM <- lapply(Rsdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.R <- data.frame(values = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.R <- data.frame(values = unlist(df_fav02GAM))


listProb04GAM <- lapply(Rsdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.R <- data.frame(values = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.R <- data.frame(values = unlist(df_fav04GAM))

listProb05GAM <- lapply(Rsdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.R <- data.frame(values = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.R <- data.frame(values = unlist(df_fav05GAM))


listProb06GAM <- lapply(Rsdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.R <- data.frame(values = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.R <- data.frame(values = unlist(df_fav06GAM))


listProb08GAM <- lapply(Rsdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.R <- data.frame(values = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.R <- data.frame(values = unlist(df_fav08GAM))

# Stratified

listProb02GAM <- lapply(Ssdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.S <- data.frame(values = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.S <- data.frame(values = unlist(df_fav02GAM))


listProb04GAM <- lapply(Ssdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.S <- data.frame(values = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.S <- data.frame(values = unlist(df_fav04GAM))

listProb05GAM <- lapply(Ssdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.S <- data.frame(values = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.S <- data.frame(values = unlist(df_fav05GAM))


listProb06GAM <- lapply(Ssdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.S <- data.frame(values = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.S <- data.frame(values = unlist(df_fav06GAM))


listProb08GAM <- lapply(Ssdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.S <- data.frame(values = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.S <- data.frame(values = unlist(df_fav08GAM))

###### #######

df_fav08GAM.R$prevalence <- "F0.8"
df_fav06GAM.R$prevalence <- "F0.6"
df_fav05GAM.R$prevalence <- "F0.5"
df_fav04GAM.R$prevalence <- "F0.4"
df_fav02GAM.R$prevalence <- "F0.2"

df_favGAM.R <- as.data.frame(rbind(df_fav02GAM.R, df_fav04GAM.R, df_fav05GAM.R, df_fav06GAM.R, df_fav08GAM.R))



df_prob08GAM.R$prevalence <- "P0.8"
df_prob06GAM.R$prevalence <- "P0.6"
df_prob05GAM.R$prevalence <- "P0.5"
df_prob04GAM.R$prevalence <- "P0.4"
df_prob02GAM.R$prevalence <- "P0.2"

df_probGAM.R <- as.data.frame(rbind(df_prob02GAM.R, df_prob04GAM.R, df_prob05GAM.R, df_prob06GAM.R, df_prob08GAM.R))






Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGAM.R)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGAM.R)


Kruscal_prevR_GAM <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevR_GAM,"Kruscal_prevRandom_GAM.csv")


df_fav08GAM.S$prevalence <- "F0.8"
df_fav06GAM.S$prevalence <- "F0.6"
df_fav05GAM.S$prevalence <- "F0.5"
df_fav04GAM.S$prevalence <- "F0.4"
df_fav02GAM.S$prevalence <- "F0.2"

df_favGAM.S <- as.data.frame(rbind(df_fav02GAM.S, df_fav04GAM.S, df_fav05GAM.S, df_fav06GAM.S, df_fav08GAM.S))



df_prob08GAM.S$prevalence <- "P0.8"
df_prob06GAM.S$prevalence <- "P0.6"
df_prob05GAM.S$prevalence <- "P0.5"
df_prob04GAM.S$prevalence <- "P0.4"
df_prob02GAM.S$prevalence <- "P0.2"

df_probGAM.S <- as.data.frame(rbind(df_prob02GAM.S, df_prob04GAM.S, df_prob05GAM.S, df_prob06GAM.S, df_prob08GAM.S)) 
                  
 Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGAM.S)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGAM.S)


Kruscal_prevS_GAM <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevS_GAM,"Kruscal_prevStratified_GAM.csv")  
                  
                  
####### GBM
                  
listProbGBM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GBM <- lapply(Rsdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.R <- data.frame(values = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.R <- data.frame(values = unlist(df_fav02GBM))


listProb04GBM <- lapply(Rsdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.R <- data.frame(values = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.R <- data.frame(values = unlist(df_fav04GBM))

listProb05GBM <- lapply(Rsdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.R <- data.frame(values = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.R <- data.frame(values = unlist(df_fav05GBM))


listProb06GBM <- lapply(Rsdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.R <- data.frame(values = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.R <- data.frame(values = unlist(df_fav06GBM))


listProb08GBM <- lapply(Rsdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.R <- data.frame(values = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.R <- data.frame(values = unlist(df_fav08GBM))

# Stratified

listProb02GBM <- lapply(Ssdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.S <- data.frame(values = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.S <- data.frame(values = unlist(df_fav02GBM))


listProb04GBM <- lapply(Ssdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.S <- data.frame(values = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.S <- data.frame(values = unlist(df_fav04GBM))

listProb05GBM <- lapply(Ssdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.S <- data.frame(values = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.S <- data.frame(values = unlist(df_fav05GBM))


listProb06GBM <- lapply(Ssdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.S <- data.frame(values = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.S <- data.frame(values = unlist(df_fav06GBM))


listProb08GBM <- lapply(Ssdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.S <- data.frame(values = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.S <- data.frame(values = unlist(df_fav08GBM))

###### #######

df_fav08GBM.R$prevalence <- "F0.8"
df_fav06GBM.R$prevalence <- "F0.6"
df_fav05GBM.R$prevalence <- "F0.5"
df_fav04GBM.R$prevalence <- "F0.4"
df_fav02GBM.R$prevalence <- "F0.2"

df_favGBM.R <- as.data.frame(rbind(df_fav02GBM.R, df_fav04GBM.R, df_fav05GBM.R, df_fav06GBM.R, df_fav08GBM.R))



df_prob08GBM.R$prevalence <- "P0.8"
df_prob06GBM.R$prevalence <- "P0.6"
df_prob05GBM.R$prevalence <- "P0.5"
df_prob04GBM.R$prevalence <- "P0.4"
df_prob02GBM.R$prevalence <- "P0.2"

df_probGBM.R <- as.data.frame(rbind(df_prob02GBM.R, df_prob04GBM.R, df_prob05GBM.R, df_prob06GBM.R, df_prob08GBM.R))






Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGBM.R)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGBM.R)


Kruscal_prevR_GBM <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevR_GBM,"Kruscal_prevRandom_GBM.csv")


df_fav08GBM.S$prevalence <- "F0.8"
df_fav06GBM.S$prevalence <- "F0.6"
df_fav05GBM.S$prevalence <- "F0.5"
df_fav04GBM.S$prevalence <- "F0.4"
df_fav02GBM.S$prevalence <- "F0.2"

df_favGBM.S <- as.data.frame(rbind(df_fav02GBM.S, df_fav04GBM.S, df_fav05GBM.S, df_fav06GBM.S, df_fav08GBM.S))



df_prob08GBM.S$prevalence <- "P0.8"
df_prob06GBM.S$prevalence <- "P0.6"
df_prob05GBM.S$prevalence <- "P0.5"
df_prob04GBM.S$prevalence <- "P0.4"
df_prob02GBM.S$prevalence <- "P0.2"

df_probGBM.S <- as.data.frame(rbind(df_prob02GBM.S, df_prob04GBM.S, df_prob05GBM.S, df_prob06GBM.S, df_prob08GBM.S)) 
                  
 Kruskal_F.tot <- kruskal.test(values ~ prevalence, data= df_favGBM.S)


Kruskal_P.tot <- kruskal.test(values ~ prevalence, data= df_probGBM.S)


Kruscal_prevS_GBM <- data.frame(Fav=Kruskal_F.tot$p.value, Prob=Kruskal_P.tot$p.value)
write.csv(Kruscal_prevS_GBM,"Kruscal_prevStratified_GBM.csv")                   

                  
####################################################                  
###################### Dunn ########################
#################################################### 
  
####### GLM 
              listProbGLM <- function(x) {
  stackProb <- stack(x$Raster$pres_P)
  
  return(stackProb)
}

listProb02GLM <- lapply(Rsdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.R <- data.frame(p02 = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM )%>%drop_na() 
df_fav02GLM.R <- data.frame(f02 = unlist(df_fav02GLM))


listProb04GLM <- lapply(Rsdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.R <- data.frame(p04 = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na() 
df_fav04GLM.R <- data.frame(f04 = unlist(df_fav04GLM))

listProb05GLM <- lapply(Rsdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.R <- data.frame(p05 = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na() 
df_fav05GLM.R <- data.frame(f05 = unlist(df_fav05GLM))


listProb06GLM <- lapply(Rsdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.R <- data.frame(p06 = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na() 
df_fav06GLM.R <- data.frame(f06 = unlist(df_fav06GLM))


listProb08GLM <- lapply(Rsdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.R <- data.frame(p08 = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na() 
df_fav08GLM.R <- data.frame(f08 = unlist(df_fav08GLM))

# Stratified
listProb02GLM <- lapply(Ssdm02GLM, listProbGLM)
stackprob02GLM <- stack(listProb02GLM)
fav0.2GLM <- calc(stackprob02GLM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GLM <- as.data.frame(stackprob02GLM)%>%drop_na() 
df_prob02GLM.S <- data.frame(p02 = unlist(df_prob02GLM))
df_fav02GLM <- as.data.frame(fav0.2GLM)%>%drop_na() 
df_fav02GLM.S <- data.frame(f02 = unlist(df_fav02GLM))


listProb04GLM <- lapply(Ssdm04GLM, listProbGLM)
stackprob04GLM <- stack(listProb04GLM)
fav0.4GLM <- calc(stackprob04GLM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GLM <- as.data.frame(stackprob04GLM)%>%drop_na() 
df_prob04GLM.S <- data.frame(p04 = unlist(df_prob04GLM))
df_fav04GLM <- as.data.frame(fav0.4GLM)%>%drop_na() 
df_fav04GLM.S <- data.frame(f04 = unlist(df_fav04GLM))

listProb05GLM <- lapply(Ssdm05GLM, listProbGLM)
stackprob05GLM <- stack(listProb05GLM)
fav0.5GLM <- calc(stackprob05GLM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GLM <- as.data.frame(stackprob05GLM)%>%drop_na() 
df_prob05GLM.S <- data.frame(p05 = unlist(df_prob05GLM))
df_fav05GLM <- as.data.frame(fav0.5GLM)%>%drop_na() 
df_fav05GLM.S <- data.frame(f05 = unlist(df_fav05GLM))


listProb06GLM <- lapply(Ssdm06GLM, listProbGLM)
stackprob06GLM <- stack(listProb06GLM)
fav0.6GLM <- calc(stackprob06GLM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GLM <- as.data.frame(stackprob06GLM)%>%drop_na() 
df_prob06GLM.S <- data.frame(p06 = unlist(df_prob06GLM))
df_fav06GLM <- as.data.frame(fav0.6GLM)%>%drop_na() 
df_fav06GLM.S <- data.frame(f06 = unlist(df_fav06GLM))


listProb08GLM <- lapply(Ssdm08GLM, listProbGLM)
stackprob08GLM <- stack(listProb08GLM)
fav0.8GLM <- calc(stackprob08GLM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GLM <- as.data.frame(stackprob08GLM)%>%drop_na() 
df_prob08GLM.S <- data.frame(p08 = unlist(df_prob08GLM))
df_fav08GLM <- as.data.frame(fav0.8GLM)%>%drop_na() 
df_fav08GLM.S <- data.frame(f08 = unlist(df_fav08GLM))      
                  
 ####### Random Forest
 ######
                  
 ######## Random
                  
                  
listProbRF <- function(x) {
  stackProb <- stack(x$Raster$Pred.predictions)
  
  return(stackProb)
}

# Random 

listProb02RF <- lapply(Rsdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.R <- data.frame(p02 = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)
df_fav02RF.R <- data.frame(f02 = unlist(df_fav02RF))


listProb04RF <- lapply(Rsdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.R <- data.frame(p04 = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)
df_fav04RF.R <- data.frame(f04 = unlist(df_fav04RF))

listProb05RF <- lapply(Rsdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.R <- data.frame(p05 = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)
df_fav05RF.R <- data.frame(f05 = unlist(df_fav05RF))


listProb06RF <- lapply(Rsdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.R <- data.frame(p06 = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)
df_fav06RF.R <- data.frame(f06 = unlist(df_fav06RF))


listProb08RF <- lapply(Rsdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.R <- data.frame(p08 = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)
df_fav08RF.R <- data.frame(f08 = unlist(df_fav08RF))

# Stratified

listProb02RF <- lapply(Ssdm02RF, listProbRF)
stackprob02RF <- stack(listProb02RF)
fav0.2RF <- calc(stackprob02RF, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02RF <- as.data.frame(stackprob02RF)%>%drop_na() 
df_prob02RF.S <- data.frame(p02 = unlist(df_prob02RF))
df_fav02RF <- as.data.frame(fav0.2RF)
df_fav02RF.S <- data.frame(f02 = unlist(df_fav02RF))


listProb04RF <- lapply(Ssdm04RF, listProbRF)
stackprob04RF <- stack(listProb04RF)
fav0.4RF <- calc(stackprob04RF, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04RF <- as.data.frame(stackprob04RF)%>%drop_na() 
df_prob04RF.S <- data.frame(p04 = unlist(df_prob04RF))
df_fav04RF <- as.data.frame(fav0.4RF)
df_fav04RF.S <- data.frame(f04 = unlist(df_fav04RF))

listProb05RF <- lapply(Ssdm05RF, listProbRF)
stackprob05RF <- stack(listProb05RF)
fav0.5RF <- calc(stackprob05RF, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05RF <- as.data.frame(stackprob05RF)%>%drop_na() 
df_prob05RF.S <- data.frame(p05 = unlist(df_prob05RF))
df_fav05RF <- as.data.frame(fav0.5RF)
df_fav05RF.S <- data.frame(f05 = unlist(df_fav05RF))


listProb06RF <- lapply(Ssdm06RF, listProbRF)
stackprob06RF <- stack(listProb06RF)
fav0.6RF <- calc(stackprob06RF, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06RF <- as.data.frame(stackprob06RF)%>%drop_na() 
df_prob06RF.S <- data.frame(p06 = unlist(df_prob06RF))
df_fav06RF <- as.data.frame(fav0.6RF)
df_fav06RF.S <- data.frame(f06 = unlist(df_fav06RF))


listProb08RF <- lapply(Ssdm08RF, listProbRF)
stackprob08RF <- stack(listProb08RF)
fav0.8RF <- calc(stackprob08RF, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08RF <- as.data.frame(stackprob08RF)%>%drop_na() 
df_prob08RF.S <- data.frame(p08 = unlist(df_prob08RF))
df_fav08RF <- as.data.frame(fav0.8RF)
df_fav08RF.S <- data.frame(f08 = unlist(df_fav08RF))





 ####### GAM
                 
          listProbGAM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GAM <- lapply(Rsdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.R <- data.frame(p02 = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.R <- data.frame(f02 = unlist(df_fav02GAM))


listProb04GAM <- lapply(Rsdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.R <- data.frame(p04 = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.R <- data.frame(f04 = unlist(df_fav04GAM))

listProb05GAM <- lapply(Rsdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.R <- data.frame(p05 = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.R <- data.frame(f05 = unlist(df_fav05GAM))


listProb06GAM <- lapply(Rsdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.R <- data.frame(p06 = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.R <- data.frame(f06 = unlist(df_fav06GAM))


listProb08GAM <- lapply(Rsdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.R <- data.frame(p08 = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.R <- data.frame(f08 = unlist(df_fav08GAM))

# Stratified

listProb02GAM <- lapply(Ssdm02GAM, listProbGAM)
stackprob02GAM <- stack(listProb02GAM)
fav0.2GAM <- calc(stackprob02GAM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GAM <- as.data.frame(stackprob02GAM)%>%drop_na() 
df_prob02GAM.S <- data.frame(p02 = unlist(df_prob02GAM))
df_fav02GAM <- as.data.frame(fav0.2GAM)%>%drop_na()
df_fav02GAM.S <- data.frame(f02 = unlist(df_fav02GAM))


listProb04GAM <- lapply(Ssdm04GAM, listProbGAM)
stackprob04GAM <- stack(listProb04GAM)
fav0.4GAM <- calc(stackprob04GAM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GAM <- as.data.frame(stackprob04GAM)%>%drop_na() 
df_prob04GAM.S <- data.frame(p04 = unlist(df_prob04GAM))
df_fav04GAM <- as.data.frame(fav0.4GAM)%>%drop_na()
df_fav04GAM.S <- data.frame(f04 = unlist(df_fav04GAM))

listProb05GAM <- lapply(Ssdm05GAM, listProbGAM)
stackprob05GAM <- stack(listProb05GAM)
fav0.5GAM <- calc(stackprob05GAM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GAM <- as.data.frame(stackprob05GAM)%>%drop_na() 
df_prob05GAM.S <- data.frame(p05 = unlist(df_prob05GAM))
df_fav05GAM <- as.data.frame(fav0.5GAM)%>%drop_na()
df_fav05GAM.S <- data.frame(f05 = unlist(df_fav05GAM))


listProb06GAM <- lapply(Ssdm06GAM, listProbGAM)
stackprob06GAM <- stack(listProb06GAM)
fav0.6GAM <- calc(stackprob06GAM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GAM <- as.data.frame(stackprob06GAM)%>%drop_na() 
df_prob06GAM.S <- data.frame(p06 = unlist(df_prob06GAM))
df_fav06GAM <- as.data.frame(fav0.6GAM)%>%drop_na()
df_fav06GAM.S <- data.frame(f06 = unlist(df_fav06GAM))


listProb08GAM <- lapply(Ssdm08GAM, listProbGAM)
stackprob08GAM <- stack(listProb08GAM)
fav0.8GAM <- calc(stackprob08GAM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GAM <- as.data.frame(stackprob08GAM)%>%drop_na() 
df_prob08GAM.S <- data.frame(p08 = unlist(df_prob08GAM))
df_fav08GAM <- as.data.frame(fav0.8GAM)%>%drop_na()
df_fav08GAM.S <- data.frame(f08 = unlist(df_fav08GAM))
              
                  
####### GBM
                  
                

listProbGBM <- function(x) {
  stackProb <- stack(x$Raster$df.prediction.Pred)
  
  return(stackProb)
}


# Random 

listProb02GBM <- lapply(Rsdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.R <- data.frame(p02 = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.R <- data.frame(f02 = unlist(df_fav02GBM))


listProb04GBM <- lapply(Rsdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.R <- data.frame(p04 = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.R <- data.frame(f04 = unlist(df_fav04GBM))

listProb05GBM <- lapply(Rsdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.R <- data.frame(p05 = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.R <- data.frame(f05 = unlist(df_fav05GBM))


listProb06GBM <- lapply(Rsdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.R <- data.frame(p06 = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.R <- data.frame(f06 = unlist(df_fav06GBM))


listProb08GBM <- lapply(Rsdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.R <- data.frame(p08 = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.R <- data.frame(f08 = unlist(df_fav08GBM))

# Stratified

listProb02GBM <- lapply(Ssdm02GBM, listProbGBM)
stackprob02GBM <- stack(listProb02GBM)
fav0.2GBM <- calc(stackprob02GBM, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))

df_prob02GBM <- as.data.frame(stackprob02GBM)%>%drop_na() 
df_prob02GBM.S <- data.frame(p02 = unlist(df_prob02GBM))
df_fav02GBM <- as.data.frame(fav0.2GBM)%>%drop_na()
df_fav02GBM.S <- data.frame(f02 = unlist(df_fav02GBM))


listProb04GBM <- lapply(Ssdm04GBM, listProbGBM)
stackprob04GBM <- stack(listProb04GBM)
fav0.4GBM <- calc(stackprob04GBM, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))

df_prob04GBM <- as.data.frame(stackprob04GBM)%>%drop_na() 
df_prob04GBM.S <- data.frame(p04 = unlist(df_prob04GBM))
df_fav04GBM <- as.data.frame(fav0.4GBM)%>%drop_na()
df_fav04GBM.S <- data.frame(f04 = unlist(df_fav04GBM))

listProb05GBM <- lapply(Ssdm05GBM, listProbGBM)
stackprob05GBM <- stack(listProb05GBM)
fav0.5GBM <- calc(stackprob05GBM, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))

df_prob05GBM <- as.data.frame(stackprob05GBM)%>%drop_na() 
df_prob05GBM.S <- data.frame(p05 = unlist(df_prob05GBM))
df_fav05GBM <- as.data.frame(fav0.5GBM)%>%drop_na()
df_fav05GBM.S <- data.frame(f05 = unlist(df_fav05GBM))


listProb06GBM <- lapply(Ssdm06GBM, listProbGBM)
stackprob06GBM <- stack(listProb06GBM)
fav0.6GBM <- calc(stackprob06GBM, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))

df_prob06GBM <- as.data.frame(stackprob06GBM)%>%drop_na() 
df_prob06GBM.S <- data.frame(p06 = unlist(df_prob06GBM))
df_fav06GBM <- as.data.frame(fav0.6GBM)%>%drop_na()
df_fav06GBM.S <- data.frame(f06 = unlist(df_fav06GBM))


listProb08GBM <- lapply(Ssdm08GBM, listProbGBM)
stackprob08GBM <- stack(listProb08GBM)
fav0.8GBM <- calc(stackprob08GBM, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

df_prob08GBM <- as.data.frame(stackprob08GBM)%>%drop_na() 
df_prob08GBM.S <- data.frame(p08 = unlist(df_prob08GBM))
df_fav08GBM <- as.data.frame(fav0.8GBM)%>%drop_na()
df_fav08GBM.S <- data.frame(f08 = unlist(df_fav08GBM))

 # GLM
   df_probGLM.R <- as.data.frame(cbind(df_prob02GLM.R, df_prob04GLM.R, df_prob05GLM.R, df_prob06GLM.R, df_prob08GLM.R))                
df_favGLM.R <- as.data.frame(cbind(df_fav02GLM.R, df_fav04GLM.R, df_fav05GLM.R, df_fav06GLM.R, df_fav08GLM.R))               

                  
 set.seed(999)
 v <- sample(1:1457750,10000, replace = FALSE)
 df_probGLM.R <- df_probGLM.R[v,]
 df_favGLM.R <- df_favGLM.R[v,] 

df_probGLM.S <- as.data.frame(cbind(df_prob02GLM.S, df_prob04GLM.S, df_prob05GLM.S, df_prob06GLM.S, df_prob08GLM.S))                
df_favGLM.S <- as.data.frame(cbind(df_fav02GLM.S, df_fav04GLM.S, df_fav05GLM.S, df_fav06GLM.S, df_fav08GLM.S))               

 df_probGLM.S <- df_probGLM.S[v,]
 df_favGLM.S <- df_favGLM.S[v,]
                  
                  
                  
 df_probGLM.S_Dunn <-  df_probGLM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_probGLM.S_Dunn$sampling <- "S"
 df_probGLM.S_Dunn <- unite( df_probGLM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_probGLM.R_Dunn <- df_probGLM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_probGLM.R_Dunn$sampling <- "R"
df_probGLM.R_Dunn <- unite(df_probGLM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")   
                  
                  
                  
                   
 df_favGLM.S_Dunn <-  df_favGLM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_favGLM.S_Dunn$sampling <- "S"
 df_favGLM.S_Dunn <- unite( df_favGLM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_favGLM.R_Dunn <- df_favGLM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_favGLM.R_Dunn$sampling <- "R"
df_favGLM.R_Dunn <- unite(df_favGLM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")
                  
                  
                  
df_DunnProbGLM <- rbind( df_probGLM.R_Dunn,  df_probGLM.S_Dunn)
df_DunnProbGLM$prev_samp <- as.factor(df_DunnProbGLM$prev_samp)
DunnProbGLM <- dunnTest(values ~ prev_samp,data=df_DunnProbGLM,method="bonferroni")
write.csv(DunnProbGLM[["res"]], "DunnProbGLM.csv")                  
                  
df_DunnFavGLM <- rbind( df_favGLM.R_Dunn,  df_favGLM.S_Dunn)
df_DunnFavGLM$prev_samp <- as.factor(df_DunnFavGLM$prev_samp)
DunnFavGLM <- dunnTest(values ~ prev_samp,data=df_DunnFavGLM,method="bonferroni")                   
write.csv(DunnFavGLM[["res"]], "DunnFavGLM.csv")                  
                  
 # RF                    
  
df_probRF.R <- as.data.frame(cbind(df_prob02RF.R, df_prob04RF.R, df_prob05RF.R, df_prob06RF.R, df_prob08RF.R))                
df_favRF.R <- as.data.frame(cbind(df_fav02RF.R, df_fav04RF.R, df_fav05RF.R, df_fav06RF.R, df_fav08RF.R))               
df_favRF.R <- df_favRF.R%>%drop_na() 
                  
 set.seed(999)
 v <- sample(1:1457750,10000, replace = FALSE)
 df_probRF.R <- df_probRF.R[v,]


 df_favRF.R <- df_favRF.R[v,] 

df_probRF.S <- as.data.frame(cbind(df_prob02RF.S, df_prob04RF.S, df_prob05RF.S, df_prob06RF.S, df_prob08RF.S))                
df_favRF.S <- as.data.frame(cbind(df_fav02RF.S, df_fav04RF.S, df_fav05RF.S, df_fav06RF.S, df_fav08RF.S))               
df_favRF.S <- df_favRF.S%>%drop_na()   
                  

 df_probRF.S <- df_probRF.S[v,]

 df_favRF.S <- df_favRF.S[v,]
                  
                  
                  
 df_probRF.S_Dunn <-  df_probRF.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_probRF.S_Dunn$sampling <- "S"
 df_probRF.S_Dunn <- unite( df_probRF.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_probRF.R_Dunn <- df_probRF.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_probRF.R_Dunn$sampling <- "R"
df_probRF.R_Dunn <- unite(df_probRF.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")   
                  
                  
                  
                   
 df_favRF.S_Dunn <-  df_favRF.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_favRF.S_Dunn$sampling <- "S"
 df_favRF.S_Dunn <- unite( df_favRF.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_favRF.R_Dunn <- df_favRF.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_favRF.R_Dunn$sampling <- "R"
df_favRF.R_Dunn <- unite(df_favRF.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")
                  
                  
                  
df_DunnProbRF <- rbind( df_probRF.R_Dunn,  df_probRF.S_Dunn)
df_DunnProbRF$prev_samp <- as.factor(df_DunnProbRF$prev_samp)
DunnProbRF <- dunnTest(values ~ prev_samp,data=df_DunnProbRF,method="bonferroni")
write.csv(DunnProbRF[["res"]], "DunnProbRF.csv")                 
                  
df_DunnFavRF <- rbind( df_favRF.R_Dunn,  df_favRF.S_Dunn)
df_DunnFavRF$prev_samp <- as.factor(df_DunnFavRF$prev_samp)
DunnFavRF <- dunnTest(values ~ prev_samp,data=df_DunnFavRF,method="bonferroni")  
write.csv(DunnFavRF[["res"]], "DunnFavRF.csv")
                  
                  
 # GAM
                  
 df_probGAM.R <- as.data.frame(cbind(df_prob02GAM.R, df_prob04GAM.R, df_prob05GAM.R, df_prob06GAM.R, df_prob08GAM.R))                
df_favGAM.R <- as.data.frame(cbind(df_fav02GAM.R, df_fav04GAM.R, df_fav05GAM.R, df_fav06GAM.R, df_fav08GAM.R))               

                  
 set.seed(999)
 v <- sample(1:1457750,10000, replace = FALSE)
 df_probGAM.R <- df_probGAM.R[v,]
 df_favGAM.R <- df_favGAM.R[v,] 

df_probGAM.S <- as.data.frame(cbind(df_prob02GAM.S, df_prob04GAM.S, df_prob05GAM.S, df_prob06GAM.S, df_prob08GAM.S))                
df_favGAM.S <- as.data.frame(cbind(df_fav02GAM.S, df_fav04GAM.S, df_fav05GAM.S, df_fav06GAM.S, df_fav08GAM.S))               
 
                  

 df_probGAM.S <- df_probGAM.S[v,]
 df_favGAM.S <- df_favGAM.S[v,]
                  
                  
                  
 df_probGAM.S_Dunn <-  df_probGAM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_probGAM.S_Dunn$sampling <- "S"
 df_probGAM.S_Dunn <- unite( df_probGAM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_probGAM.R_Dunn <- df_probGAM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_probGAM.R_Dunn$sampling <- "R"
df_probGAM.R_Dunn <- unite(df_probGAM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")   
                  
                  
                  
                   
 df_favGAM.S_Dunn <-  df_favGAM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_favGAM.S_Dunn$sampling <- "S"
 df_favGAM.S_Dunn <- unite( df_favGAM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_favGAM.R_Dunn <- df_favGAM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_favGAM.R_Dunn$sampling <- "R"
df_favGAM.R_Dunn <- unite(df_favGAM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")
                  
                  
                  
df_DunnProbGAM <- rbind( df_probGAM.R_Dunn,  df_probGAM.S_Dunn)
df_DunnProbGAM$prev_samp <- as.factor(df_DunnProbGAM$prev_samp)
DunnProbGAM <- dunnTest(values ~ prev_samp,data=df_DunnProbGAM,method="bonferroni")
write.csv(DunnProbGAM[["res"]], "DunnProbGAM.csv")                  
                  
df_DunnFavGAM <- rbind( df_favGAM.R_Dunn,  df_favGAM.S_Dunn)
df_DunnFavGAM$prev_samp <- as.factor(df_DunnFavGAM$prev_samp)
DunnFavGAM <- dunnTest(values ~ prev_samp,data=df_DunnFavGAM,method="bonferroni")                   
write.csv(DunnFavGAM[["res"]], "DunnFavGAM.csv")       
                  

# GBM
                  

                  
 df_probGBM.R <- as.data.frame(cbind(df_prob02GBM.R, df_prob04GBM.R, df_prob05GBM.R, df_prob06GBM.R, df_prob08GBM.R))                
df_favGBM.R <- as.data.frame(cbind(df_fav02GBM.R, df_fav04GBM.R, df_fav05GBM.R, df_fav06GBM.R, df_fav08GBM.R))               

                  
 set.seed(999)
 v <- sample(1:1457750,10000, replace = FALSE)
 df_probGBM.R <- df_probGBM.R[v,]
 df_favGBM.R <- df_favGBM.R[v,] 

df_probGBM.S <- as.data.frame(cbind(df_prob02GBM.S, df_prob04GBM.S, df_prob05GBM.S, df_prob06GBM.S, df_prob08GBM.S))                
df_favGBM.S <- as.data.frame(cbind(df_fav02GBM.S, df_fav04GBM.S, df_fav05GBM.S, df_fav06GBM.S, df_fav08GBM.S))               
 
                  
 df_probGBM.S <- df_probGBM.S[v,]
 df_favGBM.S <- df_favGBM.S[v,]
                  
                  
                  
 df_probGBM.S_Dunn <-  df_probGBM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_probGBM.S_Dunn$sampling <- "S"
 df_probGBM.S_Dunn <- unite( df_probGBM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_probGBM.R_Dunn <- df_probGBM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_probGBM.R_Dunn$sampling <- "R"
df_probGBM.R_Dunn <- unite(df_probGBM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")   
                  
                  
                  
                   
 df_favGBM.S_Dunn <-  df_favGBM.S%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                values_to = "values")
 df_favGBM.S_Dunn$sampling <- "S"
 df_favGBM.S_Dunn <- unite( df_favGBM.S_Dunn, prevalence, sampling, col = "prev_samp", sep = "")

df_favGBM.R_Dunn <- df_favGBM.R%>% pivot_longer( cols = 1:5, names_to ="prevalence", 
                                           values_to = "values")
 df_favGBM.R_Dunn$sampling <- "R"
df_favGBM.R_Dunn <- unite(df_favGBM.R_Dunn, prevalence, sampling, col = "prev_samp", sep = "")
                  
                  
                  
df_DunnProbGBM <- rbind( df_probGBM.R_Dunn,  df_probGBM.S_Dunn)
df_DunnProbGBM$prev_samp <- as.factor(df_DunnProbGBM$prev_samp)
DunnProbGBM <- dunnTest(values ~ prev_samp,data=df_DunnProbGBM,method="bonferroni")
write.csv(DunnProbGBM[["res"]], "DunnProbGBM.csv")                  
                  
df_DunnFavGBM <- rbind( df_favGBM.R_Dunn,  df_favGBM.S_Dunn)
df_DunnFavGBM$prev_samp <- as.factor(df_DunnFavGBM$prev_samp)
DunnFavGBM <- dunnTest(values ~ prev_samp,data=df_DunnFavGBM,method="bonferroni")                   
write.csv(DunnFavGBM[["res"]], "DunnFavGBM.csv")     
                  
 ############ mean of models for prevalence and then cv ############
e <- extent(-24.33333, 40.16667, 27.66667, 80.5)
r2 <- crop(Rsdm02GLM$Raster$pres_P, e)
r4 <- crop(Rsdm04GLM$Raster$pres_P, e)
r5 <- crop(Rsdm05GLM$Raster$pres_P, e)
r6 <- crop(Rsdm06GLM$Raster$pres_P, e)
r8 <- crop(Rsdm08GLM$Raster$pres_P, e)
                  
                  
Prob02R <- stack(r2, Rsdm02RF$Raster$Pred.predictions, Rsdm02GAM$Raster$df.prediction.Pred, Rsdm02GBM$Raster$df.prediction.Pred)
Prob04R <- stack(r4, Rsdm04RF$Raster$Pred.predictions, Rsdm04GAM$Raster$df.prediction.Pred, Rsdm04GBM$Raster$df.prediction.Pred)
Prob05R <- stack(r5, Rsdm05RF$Raster$Pred.predictions, Rsdm05GAM$Raster$df.prediction.Pred, Rsdm05GBM$Raster$df.prediction.Pred)
Prob06R <- stack(r6, Rsdm06RF$Raster$Pred.predictions, Rsdm06GAM$Raster$df.prediction.Pred, Rsdm06GBM$Raster$df.prediction.Pred)
Prob08R <- stack(r8, Rsdm08RF$Raster$Pred.predictions, Rsdm08GAM$Raster$df.prediction.Pred, Rsdm08GBM$Raster$df.prediction.Pred)               

fav0.2GLM <- calc(Rsdm02GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GLM <- calc(Rsdm04GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GLM <- calc(Rsdm05GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GLM <- calc(Rsdm06GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GLM <- calc(Rsdm08GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2RF <- calc(Rsdm02RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4RF <- calc(Rsdm04RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5RF <- calc(Rsdm05RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6RF <- calc(Rsdm06RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8RF <- calc(Rsdm08RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2GAM <- calc(Rsdm02GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GAM <- calc(Rsdm04GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GAM <- calc(Rsdm05GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GAM <- calc(Rsdm06GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GAM <- calc(Rsdm08GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2GBM <- calc(Rsdm02GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GBM <- calc(Rsdm04GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GBM <- calc(Rsdm05GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GBM <- calc(Rsdm06GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GBM <- calc(Rsdm08GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

                  
fr2 <- crop(fav0.2GLM, e)
fr4 <- crop(fav0.4GLM, e)
fr5 <- crop(fav0.5GLM, e)
fr6 <- crop(fav0.6GLM, e)
fr8 <- crop(fav0.8GLM, e)
                  
Fav02R <- stack(fr2, fav0.2RF, fav0.2GAM, fav0.2GBM)                  
Fav04R <- stack(fr4, fav0.4RF, fav0.4GAM, fav0.4GBM) 
Fav05R <- stack(fr5, fav0.5RF, fav0.5GAM, fav0.5GBM) 
Fav06R <- stack(fr6, fav0.6RF, fav0.6GAM, fav0.6GBM) 
Fav08R <- stack(fr8, fav0.8RF, fav0.8GAM, fav0.8GBM)                   

mProb02R <- calc(Prob02R, fun = mean)                  
mProb04R <- calc(Prob04R, fun = mean)  
mProb05R <- calc(Prob05R, fun = mean)                    
mProb06R <- calc(Prob06R, fun = mean)                    
mProb08R <- calc(Prob08R, fun = mean) 
                  
mFav02R <- calc(Fav02R, fun = mean)                  
mFav04R <- calc(Fav04R, fun = mean)  
mFav05R <- calc(Fav05R, fun = mean)                    
mFav06R <- calc(Fav06R, fun = mean)                    
mFav08R <- calc(Fav08R, fun = mean)                  
                  
CVprobRmean <- cv(mProb02R, mProb04R, mProb05R, mProb06R, mProb08R)
CVfavRmean <- cv(mFav02R, mFav04R, mFav05R, mFav06R, mFav08R)
difCvRmean <- CVprobRmean - CVfavRmean                  

######### stratified ############
                  
Ssdm02GLM <- SsdmGLM[[1]]
Ssdm04GLM <- SsdmGLM[[2]]
Ssdm05GLM <- SsdmGLM[[3]]
Ssdm06GLM <- SsdmGLM[[4]]
Ssdm08GLM <- SsdmGLM[[5]]
                  
 
Ssdm02RF <- SsdmRF[[1]]
Ssdm04RF <- SsdmRF[[2]]
Ssdm05RF <- SsdmRF[[3]]
Ssdm06RF <- SsdmRF[[4]]
Ssdm08RF <- SsdmRF[[5]] 
                  
                  
Ssdm02GAM <- SsdmGAM[[1]]
Ssdm04GAM <- SsdmGAM[[2]]
Ssdm05GAM <- SsdmGAM[[3]]
Ssdm06GAM <- SsdmGAM[[4]]
Ssdm08GAM <- SsdmGAM[[5]]
                  
Ssdm02GBM <- SsdmGBM[[1]]
Ssdm04GBM <- SsdmGBM[[2]]
Ssdm05GBM <- SsdmGBM[[3]]
Ssdm06GBM <- SsdmGBM[[4]]
Ssdm08GBM <- SsdmGBM[[5]]                    
                  
s2 <- crop(Ssdm02GLM$Raster$pres_P, e)
s4 <- crop(Ssdm04GLM$Raster$pres_P, e)
s5 <- crop(Ssdm05GLM$Raster$pres_P, e)
s6 <- crop(Ssdm06GLM$Raster$pres_P, e)
s8 <- crop(Ssdm08GLM$Raster$pres_P, e)                  

fav0.2GLM <- calc(Ssdm02GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GLM <- calc(Ssdm04GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GLM <- calc(Ssdm05GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GLM <- calc(Ssdm06GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GLM <- calc(Ssdm08GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2RF <- calc(Ssdm02RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4RF <- calc(Ssdm04RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5RF <- calc(Ssdm05RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6RF <- calc(Ssdm06RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8RF <- calc(Ssdm08RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2GAM <- calc(Ssdm02GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GAM <- calc(Ssdm04GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GAM <- calc(Ssdm05GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GAM <- calc(Ssdm06GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GAM <- calc(Ssdm08GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

fav0.2GBM <- calc(Ssdm02GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GBM <- calc(Ssdm04GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GBM <- calc(Ssdm05GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GBM <- calc(Ssdm06GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GBM <- calc(Ssdm08GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))
                 
                  
Prob02S <- stack(s2, Ssdm02RF$Raster$Pred.predictions, Ssdm02GAM$Raster$df.prediction.Pred, Ssdm02GBM$Raster$df.prediction.Pred)
Prob04S <- stack(s4, Ssdm04RF$Raster$Pred.predictions, Ssdm04GAM$Raster$df.prediction.Pred, Ssdm04GBM$Raster$df.prediction.Pred)
Prob05S <- stack(s5, Ssdm05RF$Raster$Pred.predictions, Ssdm05GAM$Raster$df.prediction.Pred, Ssdm05GBM$Raster$df.prediction.Pred)
Prob06S <- stack(s6, Ssdm06RF$Raster$Pred.predictions, Ssdm06GAM$Raster$df.prediction.Pred, Ssdm06GBM$Raster$df.prediction.Pred)
Prob08S <- stack(s8, Ssdm08RF$Raster$Pred.predictions, Ssdm08GAM$Raster$df.prediction.Pred, Ssdm08GBM$Raster$df.prediction.Pred)               
 
fs2 <- crop(fav0.2GLM, e)
fs4 <- crop(fav0.4GLM, e)
fs5 <- crop(fav0.5GLM, e)
fs6 <- crop(fav0.6GLM, e)
fs8 <- crop(fav0.8GLM, e)                 
                  
Fav02S <- stack(fs2, fav0.2RF, fav0.2GAM, fav0.2GBM)                  
Fav04S <- stack(fs4, fav0.4RF, fav0.4GAM, fav0.4GBM) 
Fav05S <- stack(fs5, fav0.5RF, fav0.5GAM, fav0.5GBM) 
Fav06S <- stack(fs6, fav0.6RF, fav0.6GAM, fav0.6GBM) 
Fav08S <- stack(fs8, fav0.8RF, fav0.8GAM, fav0.8GBM)                   
                  
                  
mProb02S <- calc(Prob02S, fun = mean)                  
mProb04S <- calc(Prob04S, fun = mean)  
mProb05S <- calc(Prob05S, fun = mean)                    
mProb06S <- calc(Prob06S, fun = mean)                    
mProb08S <- calc(Prob08S, fun = mean) 
                  
mFav02S <- calc(Fav02S, fun = mean)                  
mFav04S <- calc(Fav04S, fun = mean)  
mFav05S <- calc(Fav05S, fun = mean)                    
mFav06S <- calc(Fav06S, fun = mean)                    
mFav08S <- calc(Fav08S, fun = mean)  
                  
CVprobSmean <- cv(mProb02S, mProb04S, mProb05S, mProb06S, mProb08S)
CVfavSmean <- cv(mFav02S, mFav04S, mFav05S, mFav06S, mFav08S)
difCvSmean <- CVprobSmean - CVfavSmean                    

                  
                  
stack_probR <- stack(CVprobRmean, CVprobSmean)


stack_favR <- stack(CVfavRmean,CVfavSmean)


stack_diffR <- stack(difCvRmean,  difCvSmean)

 stack_probR_df <-
  as.data.frame(stack_probR, xy = TRUE) %>%
  na.omit()

stack_probR_df <- 
  stack_probR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")




stack_favR_df <-
  as.data.frame(stack_favR, xy = TRUE) %>%
  na.omit()

stack_favR_df <- 
  stack_favR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")

stack_diffR_df <-
  as.data.frame(stack_diffR, xy = TRUE) %>%
  na.omit()

stack_diffR_df <- 
  stack_diffR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")
                  
world <- ne_coastline(scale = "medium", returnclass = "sf")                  

sampling_names <- c(
  'layer.1'="Random sampling",
  'layer.2'="Stratified sampling"
)

                  
 r1<- stack_probR_df %>%
  mutate(across(variable, factor, levels=c("layer.1","layer.2"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 2, labeller=as_labeller(sampling_names)) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(2.772969,108.33145),
                   oob = scales::squish, na.value="transparent")+
  labs(x="Longitude",y="Latitude", fill="CV", title="Probability")+
  theme_light()+
  theme(
    legend.position = "bottom",  
    plot.title = element_text(size=30,face = 'bold',hjust = 0.5),
    legend.title=element_text(size=20,face = 'bold'),
    legend.text = element_text(size=18,face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(size=19,face = 'bold'),
    axis.text.x = element_text(size = 19, face = 'bold'),
    axis.title.y = element_text(size=23,face = 'bold'),
    axis.text.y = element_text(size = 19, face = 'bold'),
    axis.ticks.y=element_blank(),
    strip.text = element_text(size = 18,face = 'bold', colour = "black"))+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 21, barheight = 1.9),
         size = guide_legend(title.position="top", title.hjust = 0.5))+
         coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)

r2<- stack_favR_df %>%
  mutate(across(variable, factor, levels=c("layer.1","layer.2"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 2, labeller=as_labeller(sampling_names)) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(0.08155169,93.44030),
                   oob = scales::squish, na.value="transparent")+
  labs(x="Longitude",y="Latitude", fill="CV", title="Favourability")+
  theme_light()+
  theme(
    legend.position = "bottom",  
    plot.title = element_text(size=30,face = 'bold',hjust = 0.5),
    legend.title=element_text(size=20,face = 'bold'),
    legend.text = element_text(size=18,face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(size=19,face = 'bold'),
    axis.text.x = element_text(size = 19, face = 'bold'),
    axis.title.y = element_text(size=23,face = 'bold'),
    axis.text.y = element_text(size = 19, face = 'bold'),
    axis.ticks.y=element_blank(),
    strip.text = element_text(size = 18,face = 'bold', colour = "black"))+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 21, barheight = 1.9),
         size = guide_legend(title.position="top", title.hjust = 0.5))+
         coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)
         
         r3<- stack_diffR_df %>%
           mutate(across(variable, factor, levels=c("layer.1","layer.2"))) %>%
           ggplot() +
           geom_tile(aes(x = x, y = y, fill = value)) +
           facet_wrap(~ variable, nrow = 2, labeller=as_labeller(sampling_names)) + 
           geom_sf(data=world,
                   colour = "black", fill = "transparent", size=0.3)+  
           scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(-32.16242,40.90599),
                            oob = scales::squish, na.value="transparent")+
           labs(x="Longitude",y="Latitude", fill="CV", title="Difference")+
           theme_light()+
           theme(
             legend.position = "bottom",  
             plot.title = element_text(size=30,face = 'bold',hjust = 0.5),
             legend.title=element_text(size=20,face = 'bold'),
             legend.text = element_text(size=18,face = 'bold'),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             axis.title.x = element_text(size=19,face = 'bold'),
             axis.text.x = element_text(size = 19, face = 'bold'),
             axis.title.y = element_text(size=23,face = 'bold'),
             axis.text.y = element_text(size = 19, face = 'bold'),
             axis.ticks.y=element_blank(),
             strip.text = element_text(size = 18,face = 'bold', colour = "black"))+
           guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 21, barheight = 1.9),
                  size = guide_legend(title.position="top", title.hjust = 0.5))+
           coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)
         
         r <- r1 + r2 + r3  
         
         ggsave(plot = r,
                filename = "ESDM_cv.jpg",
                width = 15,
                height = 15,
                dpi = 600)
         
