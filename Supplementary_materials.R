list.of.packages <- c("raster", "tidyverse", "sp", "sf", "virtualspecies", "ggplot2", "rgdal", "fuzzySim", "rasterVis", "viridis", "RStoolbox", "rnaturalearth", "scico","ranger","mgcv", "dismo", "patchwork","FSA","ROCR")
lapply(list.of.packages, library, character.only = TRUE




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
                  
  
