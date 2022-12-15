########### Boyce functions for favpurability and probability of each statistical model #########



library(groupdata2)


BoyceGLM02 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGLM04 <- function(x){
  library(ecospat)
  
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
  
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGLM05 <- function(x){
  library(ecospat)
  
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
  
  
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGLM06 <- function(x){
  library(ecospat)
  
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
  
  
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGLM08 <- function(x){
  library(ecospat)
  
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
  
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGLMFav02 <- function(x){
  library(ecospat)
  
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

  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  
  Pred$Fav <- ((Pred$pres_P)/(1-Pred$pres_P))/(0.2 + (Pred$pres_P)/(1-Pred$pres_P))
  
  b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGLMFav04 <- function(x){
  library(ecospat)
  
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
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
 
  
 Pred$Fav <- ((Pred$pres_P)/(1-Pred$pres_P))/(0.4 + (Pred$pres_P)/(1-Pred$pres_P))
  
  b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGLMFav05 <- function(x){
  library(ecospat)
  
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
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  
  
  Pred$Fav <- ((Pred$pres_P)/(1-Pred$pres_P))/(0.5 + (Pred$pres_P)/(1-Pred$pres_P))
  
  b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGLMFav06 <- function(x){
  library(ecospat)
  
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
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  
 
 Pred$Fav <- ((Pred$pres_P)/(1-Pred$pres_P))/(0.6 + (Pred$pres_P)/(1-Pred$pres_P))
  
  b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGLMFav08 <- function(x){
  library(ecospat)
  
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
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  
  
  Pred$Fav <- ((Pred$pres_P)/(1-Pred$pres_P))/(0.8 + (Pred$pres_P)/(1-Pred$pres_P))
  
  b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

##########################
##########################

BoyceRF02 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceRF04 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceRF05 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceRF06 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceRF08 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceRFFav02 <- function(x){
  library(ecospat)
  
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

#Favourability
Model<-ranger(training$pres ~., data= training, importance='impurity') 
Pred<- predict(
  Model,
  data = validation[2:ncol(validation)],
  predict.all = FALSE,
  num.trees = Model$num.trees)
Pred <- data.frame(predictions= Pred[["predictions"]])
Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
Pred$Fav <- ((Pred$predictions)/(1-Pred$predictions))/(0.2 + (Pred$predictions)/(1-Pred$predictions))

b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
obs <- (b$pred
        [which(b$pres==1)])

boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor

  return(boyce)
}


BoyceRFFav04 <- function(x){
  library(ecospat)
  
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

#Favourability
Model<-ranger(training$pres ~., data= training, importance='impurity') 
Pred<- predict(
  Model,
  data = validation[2:ncol(validation)],
  predict.all = FALSE,
  num.trees = Model$num.trees)
Pred <- data.frame(predictions= Pred[["predictions"]])
Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
Pred$Fav <- ((Pred$predictions)/(1-Pred$predictions))/(0.4 + (Pred$predictions)/(1-Pred$predictions))

b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
obs <- (b$pred
        [which(b$pres==1)])

boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor

  return(boyce)
}


BoyceRFFav05 <- function(x){
  library(ecospat)
  
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

#Favourability
Model<-ranger(training$pres ~., data= training, importance='impurity') 
Pred<- predict(
  Model,
  data = validation[2:ncol(validation)],
  predict.all = FALSE,
  num.trees = Model$num.trees)
Pred <- data.frame(predictions= Pred[["predictions"]])
Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
Pred$Fav <- ((Pred$predictions)/(1-Pred$predictions))/(0.5 + (Pred$predictions)/(1-Pred$predictions))

b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
obs <- (b$pred
        [which(b$pres==1)])

boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor

  return(boyce)
}


BoyceRFFav06 <- function(x){
  library(ecospat)
  
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

#Favourability
Model<-ranger(training$pres ~., data= training, importance='impurity') 
Pred<- predict(
  Model,
  data = validation[2:ncol(validation)],
  predict.all = FALSE,
  num.trees = Model$num.trees)
Pred <- data.frame(predictions= Pred[["predictions"]])
Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
Pred$Fav <- ((Pred$predictions)/(1-Pred$predictions))/(0.6 + (Pred$predictions)/(1-Pred$predictions))

b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
obs <- (b$pred
        [which(b$pres==1)])

boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor

  return(boyce)
}


BoyceRFFav08 <- function(x){
  library(ecospat)
  
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

#Favourability
Model<-ranger(training$pres ~., data= training, importance='impurity') 
Pred<- predict(
  Model,
  data = validation[2:ncol(validation)],
  predict.all = FALSE,
  num.trees = Model$num.trees)
Pred <- data.frame(predictions= Pred[["predictions"]])
Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
Pred$Fav <- ((Pred$predictions)/(1-Pred$predictions))/(0.8 + (Pred$predictions)/(1-Pred$predictions))

b <- data.frame(cbind(pred=Pred$Fav, pres=validation$pres))
obs <- (b$pred
        [which(b$pres==1)])

boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor

  return(boyce)
}


#######################################
#################

BoyceGAM02 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGAM04 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGAM05 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGAM06 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGAM08 <- function(x){
  library(ecospat)
  
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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGAMFav02 <- function(x){
  library(ecospat)

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

#Favourability
sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.2 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)

}


BoyceGAMFav04 <- function(x){
  library(ecospat)
  
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

#Favourability
sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.4 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGAMFav05 <- function(x){
  library(ecospat)
  
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

#Favourability
sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.5 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGAMFav06 <- function(x){
  library(ecospat)
  
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

#Favourability
sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.6 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGAMFav08 <- function(x){
  library(ecospat)
  
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

#Favourability
sp_cols <- 1
    pred_cols <- 2:6
    names(training)[sp_cols]
    names(training)[pred_cols]
    #Favourability
    
    form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
    Model <- gam(form_gam, family = binomial, data = training)
    prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
    df.prediction <- data.frame(Pred=prediction)
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.8 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


#######################################
BoyceGBM02 <- function(x){
  library(ecospat)

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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGBM04 <- function(x){
  library(ecospat)

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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGBM05 <- function(x){
  library(ecospat)

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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGBM06 <- function(x){
  library(ecospat)

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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGBM08 <- function(x){
  library(ecospat)

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
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGBMFav02 <- function(x){
  library(ecospat)
  
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

#Favourability
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
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.2 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)

}


BoyceGBMFav04 <- function(x){
  library(ecospat)
  
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

#Favourability
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
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.4 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGBMFav05 <- function(x){
  library(ecospat)
  
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

#Favourability
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
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.5 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGBMFav06 <- function(x){
  library(ecospat)
  
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

#Favourability
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
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.6 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}


BoyceGBMFav08 <- function(x){
  library(ecospat)
  
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

#Favourability
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
    
  df.prediction$Fav <- ((df.prediction$Pred)/(1-df.prediction$Pred))/(0.8 + (df.prediction$Pred)/(1-df.prediction$Pred))
  b <- data.frame(cbind(pred=df.prediction$Fav, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

####################################
####################################

####################################
####################################
  

            
              
              
 BoyceFunz02GLM.R <- lapply(Rsdm02GLM, BoyceGLM02)
BoyceFunz04GLM.R <- lapply(Rsdm04GLM, BoyceGLM04)
BoyceFunz05GLM.R <- lapply(Rsdm05GLM, BoyceGLM05)
BoyceFunz06GLM.R <- lapply(Rsdm06GLM, BoyceGLM06)
BoyceFunz08GLM.R <- lapply(Rsdm08GLM, BoyceGLM08)
            

Boyce02GLM.R <- data.frame(Boyce02=unlist(BoyceFunz02GLM.R))
Boyce04GLM.R <- data.frame(Boyce04=unlist(BoyceFunz04GLM.R))
Boyce05GLM.R <- data.frame(Boyce05=unlist(BoyceFunz05GLM.R))
Boyce06GLM.R <- data.frame(Boyce06=unlist(BoyceFunz06GLM.R))
Boyce08GLM.R <- data.frame(Boyce08=unlist(BoyceFunz08GLM.R))

BoyceGLM.R_Prob <- cbind(Boyce02GLM.R, Boyce04GLM.R, Boyce05GLM.R, Boyce06GLM.R, Boyce08GLM.R)
colnames(BoyceGLM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.R_Prob, "BoyceGLM.R_Prob.csv")

              
BoyceFunz02GLM.R <- lapply(Rsdm02GLM, BoyceGLMFav02)
BoyceFunz04GLM.R <- lapply(Rsdm04GLM, BoyceGLMFav04)
BoyceFunz05GLM.R <- lapply(Rsdm05GLM, BoyceGLMFav05)
BoyceFunz06GLM.R <- lapply(Rsdm06GLM, BoyceGLMFav06)
BoyceFunz08GLM.R <- lapply(Rsdm08GLM, BoyceGLMFav08)
            

Boyce02GLM.R <- data.frame(Boyce02=unlist(BoyceFunz02GLM.R))
Boyce04GLM.R <- data.frame(Boyce04=unlist(BoyceFunz04GLM.R))
Boyce05GLM.R <- data.frame(Boyce05=unlist(BoyceFunz05GLM.R))
Boyce06GLM.R <- data.frame(Boyce06=unlist(BoyceFunz06GLM.R))
Boyce08GLM.R <- data.frame(Boyce08=unlist(BoyceFunz08GLM.R))

BoyceGLM.R_Fav <- cbind(Boyce02GLM.R, Boyce04GLM.R, Boyce05GLM.R, Boyce06GLM.R, Boyce08GLM.R)
colnames(BoyceGLM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.R_Fav, "BoyceGLM.R_Fav.csv")

              
#########
              
BoyceFunz02RF.R <- lapply(Rsdm02RF, BoyceRF02)
BoyceFunz04RF.R <- lapply(Rsdm04RF, BoyceRF04)
BoyceFunz05RF.R <- lapply(Rsdm05RF, BoyceRF05)
BoyceFunz06RF.R <- lapply(Rsdm06RF, BoyceRF06)
BoyceFunz08RF.R <- lapply(Rsdm08RF, BoyceRF08)
            

Boyce02RF.R <- data.frame(Boyce02=unlist(BoyceFunz02RF.R))
Boyce04RF.R <- data.frame(Boyce04=unlist(BoyceFunz04RF.R))
Boyce05RF.R <- data.frame(Boyce05=unlist(BoyceFunz05RF.R))
Boyce06RF.R <- data.frame(Boyce06=unlist(BoyceFunz06RF.R))
Boyce08RF.R <- data.frame(Boyce08=unlist(BoyceFunz08RF.R))

BoyceRF.R_Prob <- cbind(Boyce02RF.R, Boyce04RF.R, Boyce05RF.R, Boyce06RF.R, Boyce08RF.R)
colnames(BoyceRF.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.R_Prob, "BoyceRF.R_Prob.csv")

              
              
BoyceFunz02RF.R <- lapply(Rsdm02RF, BoyceRFFav02)
BoyceFunz04RF.R <- lapply(Rsdm04RF, BoyceRFFav04)
BoyceFunz05RF.R <- lapply(Rsdm05RF, BoyceRFFav05)
BoyceFunz06RF.R <- lapply(Rsdm06RF, BoyceRFFav06)
BoyceFunz08RF.R <- lapply(Rsdm08RF, BoyceRFFav08)
            

Boyce02RF.R <- data.frame(Boyce02=unlist(BoyceFunz02RF.R))
Boyce04RF.R <- data.frame(Boyce04=unlist(BoyceFunz04RF.R))
Boyce05RF.R <- data.frame(Boyce05=unlist(BoyceFunz05RF.R))
Boyce06RF.R <- data.frame(Boyce06=unlist(BoyceFunz06RF.R))
Boyce08RF.R <- data.frame(Boyce08=unlist(BoyceFunz08RF.R))

BoyceRF.R_Fav <- cbind(Boyce02RF.R, Boyce04RF.R, Boyce05RF.R, Boyce06RF.R, Boyce08RF.R)
colnames(BoyceRF.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.R_Fav, "BoyceRF.R_Fav.csv")

              
              
 #############             
  
BoyceFunz02GAM.R <- lapply(Rsdm02GAM, BoyceGAM02)
BoyceFunz04GAM.R <- lapply(Rsdm04GAM, BoyceGAM04)
BoyceFunz05GAM.R <- lapply(Rsdm05GAM, BoyceGAM05)
BoyceFunz06GAM.R <- lapply(Rsdm06GAM, BoyceGAM06)
BoyceFunz08GAM.R <- lapply(Rsdm08GAM, BoyceGAM08)
            

Boyce02GAM.R <- data.frame(Boyce02=unlist(BoyceFunz02GAM.R))
Boyce04GAM.R <- data.frame(Boyce04=unlist(BoyceFunz04GAM.R))
Boyce05GAM.R <- data.frame(Boyce05=unlist(BoyceFunz05GAM.R))
Boyce06GAM.R <- data.frame(Boyce06=unlist(BoyceFunz06GAM.R))
Boyce08GAM.R <- data.frame(Boyce08=unlist(BoyceFunz08GAM.R))

BoyceGAM.R_Prob <- cbind(Boyce02GAM.R, Boyce04GAM.R, Boyce05GAM.R, Boyce06GAM.R, Boyce08GAM.R)
colnames(BoyceGAM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.R_Prob, "BoyceGAM.R_Prob.csv")              

BoyceFunz02GAM.R <- lapply(Rsdm02GAM, BoyceGAMFav02)
BoyceFunz04GAM.R <- lapply(Rsdm04GAM, BoyceGAMFav04)
BoyceFunz05GAM.R <- lapply(Rsdm05GAM, BoyceGAMFav05)
BoyceFunz06GAM.R <- lapply(Rsdm06GAM, BoyceGAMFav06)
BoyceFunz08GAM.R <- lapply(Rsdm08GAM, BoyceGAMFav08)
            

Boyce02GAM.R <- data.frame(Boyce02=unlist(BoyceFunz02GAM.R))
Boyce04GAM.R <- data.frame(Boyce04=unlist(BoyceFunz04GAM.R))
Boyce05GAM.R <- data.frame(Boyce05=unlist(BoyceFunz05GAM.R))
Boyce06GAM.R <- data.frame(Boyce06=unlist(BoyceFunz06GAM.R))
Boyce08GAM.R <- data.frame(Boyce08=unlist(BoyceFunz08GAM.R))

BoyceGAM.R_Fav <- cbind(Boyce02GAM.R, Boyce04GAM.R, Boyce05GAM.R, Boyce06GAM.R, Boyce08GAM.R)
colnames(BoyceGAM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.R_Fav, "BoyceGAM.R_Fav.csv")              


              
###########              
 
              
BoyceFunz02GBM.R <- lapply(Rsdm02GBM, BoyceGBM02)
BoyceFunz04GBM.R <- lapply(Rsdm04GBM, BoyceGBM04)
BoyceFunz05GBM.R <- lapply(Rsdm05GBM, BoyceGBM05)
BoyceFunz06GBM.R <- lapply(Rsdm06GBM, BoyceGBM06)
BoyceFunz08GBM.R <- lapply(Rsdm08GBM, BoyceGBM08)
            

Boyce02GBM.R <- data.frame(Boyce02=unlist(BoyceFunz02GBM.R))
Boyce04GBM.R <- data.frame(Boyce04=unlist(BoyceFunz04GBM.R))
Boyce05GBM.R <- data.frame(Boyce05=unlist(BoyceFunz05GBM.R))
Boyce06GBM.R <- data.frame(Boyce06=unlist(BoyceFunz06GBM.R))
Boyce08GBM.R <- data.frame(Boyce08=unlist(BoyceFunz08GBM.R))

BoyceGBM.R_Prob <- cbind(Boyce02GBM.R, Boyce04GBM.R, Boyce05GBM.R, Boyce06GBM.R, Boyce08GBM.R)
colnames(BoyceGBM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGBM.R_Prob, "BoyceGBM.R_Prob.csv")

              
              
BoyceFunz02GBM.R <- lapply(Rsdm02GBM, BoyceGBMFav02)
BoyceFunz04GBM.R <- lapply(Rsdm04GBM, BoyceGBMFav04)
BoyceFunz05GBM.R <- lapply(Rsdm05GBM, BoyceGBMFav05)
BoyceFunz06GBM.R <- lapply(Rsdm06GBM, BoyceGBMFav06)
BoyceFunz08GBM.R <- lapply(Rsdm08GBM, BoyceGBMFav08)
            

Boyce02GBM.R <- data.frame(Boyce02=unlist(BoyceFunz02GBM.R))
Boyce04GBM.R <- data.frame(Boyce04=unlist(BoyceFunz04GBM.R))
Boyce05GBM.R <- data.frame(Boyce05=unlist(BoyceFunz05GBM.R))
Boyce06GBM.R <- data.frame(Boyce06=unlist(BoyceFunz06GBM.R))
Boyce08GBM.R <- data.frame(Boyce08=unlist(BoyceFunz08GBM.R))

BoyceGBM.R_Fav <- cbind(Boyce02GBM.R, Boyce04GBM.R, Boyce05GBM.R, Boyce06GBM.R, Boyce08GBM.R)
colnames(BoyceGBM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGBM.R_Fav, "BoyceGBM.R_Fav.csv")

                            
 ############## Stratified #######
 #################################
  
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



BoyceFunz02GLM.S <- lapply(Ssdm02GLM, BoyceGLM02)
BoyceFunz04GLM.S <- lapply(Ssdm04GLM, BoyceGLM04)
BoyceFunz05GLM.S <- lapply(Ssdm05GLM, BoyceGLM05)
BoyceFunz06GLM.S <- lapply(Ssdm06GLM, BoyceGLM06)
BoyceFunz08GLM.S <- lapply(Ssdm08GLM, BoyceGLM08)
            

Boyce02GLM.S <- data.frame(Boyce02=unlist(BoyceFunz02GLM.S))
Boyce04GLM.S <- data.frame(Boyce04=unlist(BoyceFunz04GLM.S))
Boyce05GLM.S <- data.frame(Boyce05=unlist(BoyceFunz05GLM.S))
Boyce06GLM.S <- data.frame(Boyce06=unlist(BoyceFunz06GLM.S))
Boyce08GLM.S <- data.frame(Boyce08=unlist(BoyceFunz08GLM.S))

BoyceGLM.S_Prob <- cbind(Boyce02GLM.S, Boyce04GLM.S, Boyce05GLM.S, Boyce06GLM.S, Boyce08GLM.S)
colnames(BoyceGLM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.S_Prob, "BoyceGLM.S_Prob.csv")

  

BoyceFunz02GLM.S <- lapply(Ssdm02GLM, BoyceGLMFav02)
BoyceFunz04GLM.S <- lapply(Ssdm04GLM, BoyceGLMFav04)
BoyceFunz05GLM.S <- lapply(Ssdm05GLM, BoyceGLMFav05)
BoyceFunz06GLM.S <- lapply(Ssdm06GLM, BoyceGLMFav06)
BoyceFunz08GLM.S <- lapply(Ssdm08GLM, BoyceGLMFav08)
            

Boyce02GLM.S <- data.frame(Boyce02=unlist(BoyceFunz02GLM.S))
Boyce04GLM.S <- data.frame(Boyce04=unlist(BoyceFunz04GLM.S))
Boyce05GLM.S <- data.frame(Boyce05=unlist(BoyceFunz05GLM.S))
Boyce06GLM.S <- data.frame(Boyce06=unlist(BoyceFunz06GLM.S))
Boyce08GLM.S <- data.frame(Boyce08=unlist(BoyceFunz08GLM.S))

BoyceGLM.S_Fav <- cbind(Boyce02GLM.S, Boyce04GLM.S, Boyce05GLM.S, Boyce06GLM.S, Boyce08GLM.S)
colnames(BoyceGLM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.S_Fav, "BoyceGLM.S_Fav.csv")

  
  
 ########### 

BoyceFunz02RF.S <- lapply(Ssdm02RF, BoyceRF02)
BoyceFunz04RF.S <- lapply(Ssdm04RF, BoyceRF04)
BoyceFunz05RF.S <- lapply(Ssdm05RF, BoyceRF05)
BoyceFunz06RF.S <- lapply(Ssdm06RF, BoyceRF06)
BoyceFunz08RF.S <- lapply(Ssdm08RF, BoyceRF08)
            

Boyce02RF.S <- data.frame(Boyce02=unlist(BoyceFunz02RF.S))
Boyce04RF.S <- data.frame(Boyce04=unlist(BoyceFunz04RF.S))
Boyce05RF.S <- data.frame(Boyce05=unlist(BoyceFunz05RF.S))
Boyce06RF.S <- data.frame(Boyce06=unlist(BoyceFunz06RF.S))
Boyce08RF.S <- data.frame(Boyce08=unlist(BoyceFunz08RF.S))

BoyceRF.S_Prob <- cbind(Boyce02RF.S, Boyce04RF.S, Boyce05RF.S, Boyce06RF.S, Boyce08RF.S)
colnames(BoyceRF.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.S_Prob, "BoyceRF.S_Prob.csv")

  
BoyceFunz02RF.S <- lapply(Ssdm02RF, BoyceRFFav02)
BoyceFunz04RF.S <- lapply(Ssdm04RF, BoyceRFFav04)
BoyceFunz05RF.S <- lapply(Ssdm05RF, BoyceRFFav05)
BoyceFunz06RF.S <- lapply(Ssdm06RF, BoyceRFFav06)
BoyceFunz08RF.S <- lapply(Ssdm08RF, BoyceRFFav08)
            

Boyce02RF.S <- data.frame(Boyce02=unlist(BoyceFunz02RF.S))
Boyce04RF.S <- data.frame(Boyce04=unlist(BoyceFunz04RF.S))
Boyce05RF.S <- data.frame(Boyce05=unlist(BoyceFunz05RF.S))
Boyce06RF.S <- data.frame(Boyce06=unlist(BoyceFunz06RF.S))
Boyce08RF.S <- data.frame(Boyce08=unlist(BoyceFunz08RF.S))

BoyceRF.S_Fav <- cbind(Boyce02RF.S, Boyce04RF.S, Boyce05RF.S, Boyce06RF.S, Boyce08RF.S)
colnames(BoyceRF.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.S_Fav, "BoyceRF.S_Fav.csv")

  
###############  

BoyceFunz02GAM.S <- lapply(Ssdm02GAM, BoyceGAM02)
BoyceFunz04GAM.S <- lapply(Ssdm04GAM, BoyceGAM04)
BoyceFunz05GAM.S <- lapply(Ssdm05GAM, BoyceGAM05)
BoyceFunz06GAM.S <- lapply(Ssdm06GAM, BoyceGAM06)
BoyceFunz08GAM.S <- lapply(Ssdm08GAM, BoyceGAM08)
            

Boyce02GAM.S <- data.frame(Boyce02=unlist(BoyceFunz02GAM.S))
Boyce04GAM.S <- data.frame(Boyce04=unlist(BoyceFunz04GAM.S))
Boyce05GAM.S <- data.frame(Boyce05=unlist(BoyceFunz05GAM.S))
Boyce06GAM.S <- data.frame(Boyce06=unlist(BoyceFunz06GAM.S))
Boyce08GAM.S <- data.frame(Boyce08=unlist(BoyceFunz08GAM.S))

BoyceGAM.S_Prob <- cbind(Boyce02GAM.S, Boyce04GAM.S, Boyce05GAM.S, Boyce06GAM.S, Boyce08GAM.S)
colnames(BoyceGAM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Prob, "BoyceGAM.S_Prob.csv")  

  
BoyceFunz02GAM.S <- lapply(Ssdm02GAM, BoyceGAMFav02)
BoyceFunz04GAM.S <- lapply(Ssdm04GAM, BoyceGAMFav04)
BoyceFunz05GAM.S <- lapply(Ssdm05GAM, BoyceGAMFav05)
BoyceFunz06GAM.S <- lapply(Ssdm06GAM, BoyceGAMFav06)
BoyceFunz08GAM.S <- lapply(Ssdm08GAM, BoyceGAMFav08)
            

Boyce02GAM.S <- data.frame(Boyce02=unlist(BoyceFunz02GAM.S))
Boyce04GAM.S <- data.frame(Boyce04=unlist(BoyceFunz04GAM.S))
Boyce05GAM.S <- data.frame(Boyce05=unlist(BoyceFunz05GAM.S))
Boyce06GAM.S <- data.frame(Boyce06=unlist(BoyceFunz06GAM.S))
Boyce08GAM.S <- data.frame(Boyce08=unlist(BoyceFunz08GAM.S))

BoyceGAM.S_Fav <- cbind(Boyce02GAM.S, Boyce04GAM.S, Boyce05GAM.S, Boyce06GAM.S, Boyce08GAM.S)
colnames(BoyceGAM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Fav, "BoyceGAM.S_Fav.csv")  

  
 
################  

BoyceFunz02GBM.S <- lapply(Ssdm02GBM, BoyceGBM02)
BoyceFunz04GBM.S <- lapply(Ssdm04GBM, BoyceGBM04)
BoyceFunz05GBM.S <- lapply(Ssdm05GBM, BoyceGBM05)
BoyceFunz06GBM.S <- lapply(Ssdm06GBM, BoyceGBM06)
BoyceFunz08GBM.S <- lapply(Ssdm08GBM, BoyceGBM08)
            

Boyce02GBM.S <- data.frame(Boyce02=unlist(BoyceFunz02GBM.S))
Boyce04GBM.S <- data.frame(Boyce04=unlist(BoyceFunz04GBM.S))
Boyce05GBM.S <- data.frame(Boyce05=unlist(BoyceFunz05GBM.S))
Boyce06GBM.S <- data.frame(Boyce06=unlist(BoyceFunz06GBM.S))
Boyce08GBM.S <- data.frame(Boyce08=unlist(BoyceFunz08GBM.S))

BoyceGBM.S_Prob <- cbind(Boyce02GBM.S, Boyce04GBM.S, Boyce05GBM.S, Boyce06GBM.S, Boyce08GBM.S)
colnames(BoyceGBM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Prob, "BoyceGAM.S_Prob.csv")  



BoyceFunz02GBM.S <- lapply(Ssdm02GBM, BoyceGBMFav02)
BoyceFunz04GBM.S <- lapply(Ssdm04GBM, BoyceGBMFav04)
BoyceFunz05GBM.S <- lapply(Ssdm05GBM, BoyceGBMFav05)
BoyceFunz06GBM.S <- lapply(Ssdm06GBM, BoyceGBMFav06)
BoyceFunz08GBM.S <- lapply(Ssdm08GBM, BoyceGBMFav08)
            

Boyce02GBM.S <- data.frame(Boyce02=unlist(BoyceFunz02GBM.S))
Boyce04GBM.S <- data.frame(Boyce04=unlist(BoyceFunz04GBM.S))
Boyce05GBM.S <- data.frame(Boyce05=unlist(BoyceFunz05GBM.S))
Boyce06GBM.S <- data.frame(Boyce06=unlist(BoyceFunz06GBM.S))
Boyce08GBM.S <- data.frame(Boyce08=unlist(BoyceFunz08GBM.S))

BoyceGBM.S_Fav <- cbind(Boyce02GBM.S, Boyce04GBM.S, Boyce05GBM.S, Boyce06GBM.S, Boyce08GBM.S)
colnames(BoyceGBM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Fav, "BoyceGAM.S_Fav.csv")  
 
  
 ################# boxplots ################# 
   
  
########## GLM

colnames(BoyceGLM.R_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGLM.R_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceRGLM_Prob_boxplot <- BoyceGLM.R_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGLM_Prob_boxplot$pred <- "Probability"
boyceRGLM_Fav_boxplot <- BoyceGLM.R_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGLM_Fav_boxplot$pred <- "Favourability"
boyceGLMR_tot <- rbind(boyceRGLM_Prob_boxplot, boyceRGLM_Fav_boxplot)

colnames(BoyceGLM.S_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGLM.S_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceGLMS_Prob_boxplot <- BoyceGLM.S_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGLMS_Prob_boxplot$pred <- "Probability"
boyceGLMS_Fav_boxplot <- BoyceGLM.S_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGLMS_Fav_boxplot$pred <- "Favourability"

boyceGLMS_tot <- rbind(boyceGLMS_Prob_boxplot, boyceGLMS_Fav_boxplot)  
  
  
boyceGLMR_tot$sampling <- "Random sampling"
boyceGLMS_tot$sampling <- "Stratified sampling"
rb.boyceGLM <- rbind(boyceGLMR_tot, boyceGLMS_tot)  
  
colnames(BoyceRF.R_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceRF.R_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")



boyceRFR_Prob_boxplot <- BoyceRF.R_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRFR_Prob_boxplot$pred <- "Probability"
boyceRFR_Fav_boxplot <- BoyceRF.R_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRFR_Fav_boxplot$pred <- "Favourability"
boyceRFR_tot <- rbind(boyceRFR_Prob_boxplot, boyceRFR_Fav_boxplot)


colnames(BoyceRF.S_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceRF.S_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceRFS_Prob_boxplot <- BoyceRF.S_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRFS_Prob_boxplot$pred <- "Probability"
boyceRFS_Fav_boxplot <- BoyceRF.S_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRFS_Fav_boxplot$pred <- "Favourability"
boyceRFS_tot <- rbind(boyceRFS_Prob_boxplot, boyceRFS_Fav_boxplot)

boyceRFR_tot$sampling <- "Random sampling"
boyceRFS_tot$sampling <- "Stratified sampling"
rb.boyceRF <- rbind(boyceRFR_tot, boyceRFS_tot)

  
########## GAM


colnames(BoyceGAM.R_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGAM.R_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceRGAM_Prob_boxplot <- BoyceGAM.R_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGAM_Prob_boxplot$pred <- "Probability"
boyceRGAM_Fav_boxplot <- BoyceGAM.R_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGAM_Fav_boxplot$pred <- "Favourability"
boyceGAMR_tot <- rbind(boyceRGAM_Prob_boxplot, boyceRGAM_Fav_boxplot)

colnames(BoyceGAM.S_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGAM.S_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceGAMS_Prob_boxplot <- BoyceGAM.S_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGAMS_Prob_boxplot$pred <- "Probability"
boyceGAMS_Fav_boxplot <- BoyceGAM.S_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGAMS_Fav_boxplot$pred <- "Favourability"
boyceGAMS_tot <- rbind(boyceGAMS_Prob_boxplot, boyceGAMS_Fav_boxplot)


boyceGAMR_tot$sampling <- "Random sampling"
boyceGAMS_tot$sampling <- "Stratified sampling"
rb.boyceGAM <- rbind(boyceGAMR_tot, boyceGAMS_tot)  
  
  
 ########## GBM


colnames(BoyceGBM.R_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGBM.R_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceRGBM_Prob_boxplot <- BoyceGBM.R_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGBM_Prob_boxplot$pred <- "Probability"
boyceRGBM_Fav_boxplot <- BoyceGBM.R_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceRGBM_Fav_boxplot$pred <- "Favourability"
boyceGBMR_tot <- rbind(boyceRGBM_Prob_boxplot, boyceRGBM_Fav_boxplot)

colnames(BoyceGBM.S_Fav) <- c("F0.2","F0.4","F0.5","F0.6","F0.8")
colnames(BoyceGBM.S_Prob) <- c("P0.2","P0.4","P0.5","P0.6","P0.8")


boyceGBMS_Prob_boxplot <- BoyceGBM.S_Prob %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGBMS_Prob_boxplot$pred <- "Probability"
boyceGBMS_Fav_boxplot <- BoyceGBM.S_Fav %>% pivot_longer( cols = 1:5, names_to ="prevalence", values_to = "values")
boyceGBMS_Fav_boxplot$pred <- "Favourability"
boyceGBMS_tot <- rbind(boyceGBMS_Prob_boxplot, boyceGBMS_Fav_boxplot) 
  
boyceGBMR_tot$sampling <- "Random sampling"
boyceGBMS_tot$sampling <- "Stratified sampling"
rb.boyceGBM <- rbind(boyceGBMR_tot, boyceGBMS_tot)  
 
 gam <- ggplot(rb.boyceGAM, aes(prevalence, values, fill = pred))+
  geom_half_violin(alpha = 0.6, side = "l")+
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, side = "r")+
  facet_wrap(~ sampling, nrow = 1) +
  scale_fill_manual(values=c("#800080", "#E69F00"))+
  labs(x = "Sample prevalence", y="Continuous Boyce Index",fill = "Prediction", title="GAM")+
  scale_x_discrete(limits=c("F0.2","P0.2", "F0.4", "P0.4", "F0.5", "P0.5","F0.6","P0.6", "F0.8","P0.8"))+
  theme_light() +
  theme(plot.margin = margin(.3,.3,.3,.3, "cm"),
        legend.position = "bottom",  
        plot.title = element_text(size=15,face = 'bold'),
        legend.title=element_text(size=16,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=15,face = 'bold'),
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size=15,face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=16), 
        strip.text = element_text(size= 14,face = 'bold', colour = "black"),
        legend.text = element_text(size=16,angle = 0), 
        legend.key.size = unit(1.2, 'cm'))   

glm <- ggplot(rb.boyceGLM, aes(prevalence, values, fill = pred))+
  geom_half_violin(alpha = 0.6, side = "l")+
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, side = "r")+
  facet_wrap(~ sampling, nrow = 1) +
  scale_fill_manual(values=c("#800080", "#E69F00"))+
  labs(x = "Sample prevalence", y="Continuous Boyce Index", fill = "Prediction", title="GLM")+
  scale_x_discrete(limits=c("F0.2","P0.2", "F0.4", "P0.4", "F0.5", "P0.5","F0.6","P0.6", "F0.8","P0.8"))+
  theme_light() +
  theme(plot.margin = margin(.3,.3,.3,.3, "cm"),
        legend.position = "bottom",  
        plot.title = element_text(size=15,face = 'bold'),
        legend.title=element_text(size=16,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=15,face = 'bold'),
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size=15,face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=16), 
        strip.text = element_text(size= 14,face = 'bold', colour = "black"),
        legend.text = element_text(size=16,angle = 0), 
        legend.key.size = unit(1.2, 'cm'))    


rf <- ggplot(rb.boyceRF, aes(prevalence, values, fill = pred))+
  geom_half_violin(alpha = 0.6, side = "l")+
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, side = "r")+
  facet_wrap(~ sampling, nrow = 1) +
  scale_fill_manual(values=c("#800080", "#E69F00"))+
  labs(x = "Sample prevalence", y="Continuous Boyce Index",fill = "Prediction", title="RF")+
  scale_x_discrete(limits=c("F0.2","P0.2", "F0.4", "P0.4", "F0.5", "P0.5","F0.6","P0.6", "F0.8","P0.8"))+
  theme_light() +
  theme(plot.margin = margin(.3,.3,.3,.3, "cm"),
        legend.position = "bottom",  
        plot.title = element_text(size=15,face = 'bold'),
        legend.title=element_text(size=16,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=15,face = 'bold'),
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size=15,face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=16), 
        strip.text = element_text(size=14,face = 'bold', colour = "black"),
        legend.text = element_text(size=16,angle = 0), 
        legend.key.size = unit(1.2, 'cm'))    


gbm <- ggplot(rb.boyceGBM, aes(prevalence, values, fill = pred))+
  geom_half_violin(alpha = 0.6, side = "l")+
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, side = "r")+
  facet_wrap(~ sampling, nrow = 1) +
  scale_fill_manual(values=c("#800080", "#E69F00"))+
  labs(x = "Sample prevalence", y="Continuous Boyce Index",fill = "Prediction", title="BRT")+
  scale_x_discrete(limits=c("F0.2","P0.2", "F0.4", "P0.4", "F0.5", "P0.5","F0.6","P0.6", "F0.8","P0.8"))+
  theme_light() +
  theme(plot.margin = margin(.3,.3,.3,.3, "cm"),
        legend.position = "bottom",  
        plot.title = element_text(size=15,face = 'bold'),
        legend.title=element_text(size=16,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=15,face = 'bold'),
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size=15,face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=16),
        strip.text = element_text(size=14,face = 'bold', colour = "black"),
        legend.text = element_text(size=16,angle = 0), 
        legend.key.size = unit(1.2, 'cm'))    


f <- glm + gam + rf + gbm + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(plot = f,
       filename = "f.jpeg",
       width = 18,
       height = 10,
       dpi = 600)
