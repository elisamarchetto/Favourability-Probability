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



