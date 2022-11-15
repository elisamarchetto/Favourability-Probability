list.of.packages <- c("raster","tidyverse","sp","sf","virtualspecies","ggplot2","rgdal","fuzzySim","rasterVis","viridis","RStoolbox","rnaturalearth","caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)


Worldclim<-raster::getData('worldclim', var='bio', res=10) 

Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation')
envData<-crop(Worldclim, Europe)
envData <- mask(envData, Europe)


Random.SDM <- function(alpha=-0.05, value=NULL, Predictors=NULL, SamplePoints=NULL) {
  
  set.seed(999)
  myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
  Outputs.l <- list()
  
  
  for (i in 1:length(myRandNum)) {
    
  set.seed(999)    
  random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]], convert.to.PA = FALSE, species.type = "additive", realistic.sp = TRUE, plot = FALSE)
    
  set.seed(999)
  new.pres<-convertToPA(random.sp, beta="random", alpha = alpha, plot = FALSE, species.prevalence = 0.2) 
  
  
  presence.points <- sampleOccurrences(new.pres, n = SamplePoints, type = "presence-absence", sample.prevalence = value, detection.probability = 1, correct.by.suitability = FALSE, plot = FALSE)  
  
  PresAbs=presence.points$sample.points[, c( "x", "y", "Observed")]
  coordinates(PresAbs)<-~x+y 
  crs(PresAbs)<-crs(Predictors) 
  values <- raster::extract(Predictors[[myRandNum[[i]]]], PresAbs, df=T)
  values <- values[, -1]
  modSpecies<- data.frame(pres = PresAbs@data[,1], x = PresAbs$x, y = PresAbs$y, values[1:ncol(values)])
  
  ## newdata for prediction
  
  preds <- Predictors[[myRandNum[[i]]]] 
  
  ## Favourability and Probability
  
  Model<-multGLM(modSpecies, sp.cols = 1, var.cols=4:ncol(modSpecies), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
  Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = FALSE)
  crs(Pred) <- crs(Predictors)
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(value + (x)/(1-x)))  
  
  Outputs=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"=modSpecies)
  Outputs.l[[i]] <- Outputs 
}

return(Outputs.l)

}

listV <- list(0.2, 0.4, 0.5, 0.6, 0.8)


for(i in 1:5) {
  if(i==1){
    
    Rsdm02GLM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm02GLM)
    
  }else if(i==2){
    
    Rsdm04GLM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm04GLM)
    
  }else if(i==3){
    
    
    Rsdm05GLM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm05GLM)
    
  }else if(i==4){
    
    
    Rsdm06GLM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm06GLM)
    
  }else if(i==5){
    
    
    Rsdm08GLM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print( Rsdm08GLM)
    
  }
  
}
########
########

Stratified.SDM <- function(Predictors=NULL, alpha= -0.05) {
  
  set.seed(999)
  myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
  
  Outputs.l <- list()
  
  #widespread species, at the moment
  for (i in 1:length(myRandNum)) {
  
  set.seed(999)    
  random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]], convert.to.PA = FALSE, species.type = "additive", realistic.sp = TRUE, plot = FALSE)
  
  set.seed(999)
  new.pres<-convertToPA(random.sp, beta="random", alpha = alpha, plot = FALSE, species.prevalence = 0.2) 
  
  #stratified sampling
  
  r <- new.pres$pa.raster
  r.grid <- new.pres$pa.raster
  res(r.grid) <- 0.3
  p <- rasterToPolygons(r.grid, n=4, na.rm = T)
  p <- as(p, "SpatialPolygons")
  
  df.r <- as.data.frame(r, xy=TRUE, na.rm=TRUE)
  df.r$layer[df.r$layer==TRUE]<-1
  df.r$layer[df.r$layer==FALSE]<-0
  
  coordinates(df.r)<- ~ x + y
  crs(df.r) <- crs(p)
  id <- over(df.r,p) 
  df.r <- data.frame(x = coordinates(df.r)[,1], y = coordinates(df.r)[,2], pa = df.r@data[,1])
  df.r <- cbind(df.r, id)
  
  p.sf <- st_as_sf(p)
  p.centroids <- st_centroid(p.sf)
  p.centroids <- p.centroids %>% rownames_to_column(var="id")
  p.ctrd.sp <- as_Spatial(p.centroids)
  pa.centroids <- merge(df.r, p.ctrd.sp, by="id")
  df.pa.centroids <- pa.centroids %>% group_by(id)%>% mutate(pres = if_else(any(pa == 1), 1, 0))%>% dplyr::select(pres, coords.x1, coords.x2)%>%unique()
  df.pa.centroids <- df.pa.centroids[,-1]
  
  coordinates(df.pa.centroids)<- ~ coords.x1 + coords.x2
  crs(df.pa.centroids) <- crs(Predictors)
  values <- raster::extract(Predictors[[myRandNum[[i]]]], df.pa.centroids, df=T, na.rm = TRUE)
  values <- values[,-1]
  df.pred.centroids <- cbind(pres=df.pa.centroids@data[,1], x=df.pa.centroids$coords.x1, y= df.pa.centroids$coords.x2, values)
  df.pred.centroids <- df.pred.centroids %>% filter(complete.cases(.))
  
  sample.p0.5 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(333)
  sample.a0.5 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(667) 
  sample.0.5 <- rbind(sample.p0.5, sample.a0.5)
  
  sample.p0.2 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(167)
  sample.a0.2 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(833)
  sample.0.2 <- rbind(sample.p0.2, sample.a0.2)
  
  sample.p0.4 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(286)
  sample.a0.4 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(714)
  sample.0.4 <- rbind(sample.p0.4, sample.a0.4)
  
  sample.p0.6 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(375)
  sample.a0.6 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(625)
  sample.0.6 <- rbind(sample.p0.6, sample.a0.6)
  
  sample.p0.8 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(444)
  sample.a0.8 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(556)
  sample.0.8 <- rbind(sample.p0.8, sample.a0.8)
  
  modSpecies <- list(sample.0.2, sample.0.4, sample.0.5, sample.0.6, sample.0.8)
  
  for(j in 1:5) {
    if(j==1){
      
      preds <- Predictors[[myRandNum[[i]]]]
      
      Model<-multGLM( modSpecies[[j]], sp.cols = 1, var.cols=4:ncol( modSpecies[[j]]), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
      Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE)
      crs(Pred) <- crs(Predictors)
      Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))  

      Outputs02=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
      
      print(Outputs02)
      
    }else if(j==2){
      
      preds <- Predictors[[myRandNum[[i]]]] 
      
      Model<-multGLM( modSpecies[[j]], sp.cols = 1, var.cols=4:ncol( modSpecies[[j]]), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
      Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE)
      crs(Pred) <- crs(Predictors)
      Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))  

      Outputs04=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
      
      print(Outputs04)
      
    }else if(j==3){
      
      preds <- Predictors[[myRandNum[[i]]]] 
      
      Model<-multGLM( modSpecies[[j]], sp.cols = 1, var.cols=4:ncol( modSpecies[[j]]), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
      Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE)
      crs(Pred) <- crs(Predictors)
      Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))  

      Outputs05=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
      print(Outputs05)
      
    }else if(j==4){
      
      preds <- Predictors[[myRandNum[[i]]]] 
      
      Model<-multGLM( modSpecies[[j]], sp.cols = 1, var.cols=4:ncol( modSpecies[[j]]), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
      Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE)
      crs(Pred) <- crs(Predictors)
      Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))  

      Outputs06=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
      print(Outputs06)
      
    }else if(j==5){
      
      preds <- Predictors[[myRandNum[[i]]]] 
      
      Model<-multGLM( modSpecies[[j]], sp.cols = 1, var.cols=4:ncol( modSpecies[[j]]), family = "binomial", step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE) 
      Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE)
      crs(Pred) <- crs(Predictors)
      Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))  

      Outputs08=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
      
      print(Outputs08)
      
     
    }
    
  }
  
  Outputs=list( Outputs02, Outputs04, Outputs05, Outputs06, Outputs08)
  Outputs.l[[i]] <- Outputs 
  }
  
  return(Outputs.l)
  
}
  
SsdmGLM <- Stratified.SDM(envData)

  
########### RANDOM FOREST #########
  
  ###### RANDOM SAMPLE  ######

  Random.SDM <- function(alpha=-0.05,value=NULL,Predictors=NULL, SamplePoints=NULL) {
  
    set.seed(999)
    myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
    
    Outputs.l <- list()
  
  #widespread species, at the moment
    for (i in 1:length(myRandNum)) {
  set.seed(999)    
  random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                convert.to.PA = FALSE,
                                                species.type = "additive",
                                                realistic.sp = TRUE,
                                                plot = FALSE)
  set.seed(999)
  new.pres<-convertToPA(random.sp, 
                        beta="random",
                        alpha = alpha, plot = FALSE, 
                        species.prevalence = 0.2) 

  
 
    presence.points <- sampleOccurrences(new.pres,
                                         n = SamplePoints, 
                                         type = "presence-absence",
                                         sample.prevalence = value,
                                         detection.probability = 1,
                                         correct.by.suitability = FALSE,
                                         plot = FALSE)  
  
  
  PresAbs=presence.points$sample.points[, c( "x", "y",  "Observed")]
  coordinates(PresAbs)<-~x+y 
  crs(PresAbs)<-crs(Predictors) 
  values <- raster::extract(Predictors[[myRandNum[[i]]]],PresAbs, df=T)
  values <- values[,-1]
 
  modSpecies<- data.frame(pres = PresAbs@data[,1], x = PresAbs$x, y = PresAbs$y, values[1:ncol(values)])
  
      
      
      
  sp_cols <- 1
  pred_cols <- 4:8
  names(modSpecies)[sp_cols]
  names(modSpecies)[pred_cols]
      
  ## newdata for prediction
  
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
  form_rf <- as.formula(paste0(names(modSpecies)[sp_cols], "~", paste0((names(modSpecies)[pred_cols]), collapse = "+")))
     
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies, importance='impurity') 
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16  
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(value + (x)/(1-x)))  
  
  Outputs=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"=modSpecies) 
              
 Outputs.l[[i]] <- Outputs 
  }
   
  return(Outputs.l)
  
}


listV <- list(0.2, 0.4, 0.5, 0.6, 0.8)


for(i in 1:5) {
  if(i==1){
    
    Rsdm02RF <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm02RF)
    
  }else if(i==2){
    
    Rsdm04RF <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm04RF)
    
  }else if(i==3){
    
    
    Rsdm05RF <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm05RF)
    
  }else if(i==4){
    
    
    Rsdm06RF <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm06RF)
    
  }else if(i==5){
    
    
    Rsdm08RF <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print( Rsdm08RF)
    
   
  }
  
}

    ####### STRATIFIED SAMPLE ######
  
Stratified.SDM <- function(Predictors=NULL, alpha= -0.05) {
  
  
  set.seed(999)
  myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
  
  Outputs.l <- list()
  
  #widespread species, at the moment
  for (i in 1:length(myRandNum)) {
    set.seed(999)    
    random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                  convert.to.PA = FALSE,
                                                  species.type = "additive",
                                                  realistic.sp = TRUE,
                                                  plot = FALSE)
    set.seed(999)
    new.pres<-convertToPA(random.sp, 
                          beta="random",
                          alpha = alpha, plot = FALSE, 
                          species.prevalence = 0.2) 
    
    
    
    r <- new.pres$pa.raster
  r.grid <- new.pres$pa.raster
  res(r.grid) <- 0.3
  p <- rasterToPolygons(r.grid, n=4, na.rm = T)
  p <- as(p, "SpatialPolygons")
  
  df.r <- as.data.frame(r, xy=TRUE, na.rm=TRUE)
  df.r$layer[df.r$layer==TRUE]<-1
  df.r$layer[df.r$layer==FALSE]<-0
  
  coordinates(df.r)<- ~ x + y
  crs(df.r) <- crs(p)
  id <- over(df.r,p) 
  df.r <- data.frame(x = coordinates(df.r)[,1], y = coordinates(df.r)[,2], pa = df.r@data[,1])
  df.r <- cbind(df.r, id)
  
  p.sf <- st_as_sf(p)
  p.centroids <- st_centroid(p.sf)
  p.centroids <- p.centroids %>% rownames_to_column(var="id")
  p.ctrd.sp <- as_Spatial(p.centroids)
  pa.centroids <- merge(df.r, p.ctrd.sp, by="id")
  df.pa.centroids <- pa.centroids %>% group_by(id)%>% mutate(pres = if_else(any(pa == 1), 1, 0))%>% dplyr::select(pres, coords.x1, coords.x2)%>%unique()
  df.pa.centroids <- df.pa.centroids[,-1]
  
  coordinates(df.pa.centroids)<- ~ coords.x1 + coords.x2
  crs(df.pa.centroids) <- crs(Predictors)
  values <- raster::extract(Predictors[[myRandNum[[i]]]], df.pa.centroids, df=T, na.rm = TRUE)
  values <- values[,-1]
  df.pred.centroids <- cbind(pres=df.pa.centroids@data[,1], x=df.pa.centroids$coords.x1, y= df.pa.centroids$coords.x2, values)
  df.pred.centroids <- df.pred.centroids %>% filter(complete.cases(.))
    
    sample.p0.5 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(333)
    sample.a0.5 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(667) 
    sample.0.5 <- rbind(sample.p0.5, sample.a0.5)
     sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.5)[sp_cols]
  names(sample.0.5)[pred_cols]
    
    sample.p0.2 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(167)
    sample.a0.2 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(833)
    sample.0.2 <- rbind(sample.p0.2, sample.a0.2)
     sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.2)[sp_cols]
  names(sample.0.2)[pred_cols]
    
    
    sample.p0.4 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(286)
    sample.a0.4 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(714)
    sample.0.4 <- rbind(sample.p0.4, sample.a0.4)
     sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.4)[sp_cols]
  names(sample.0.4)[pred_cols]
    
    sample.p0.6 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(375)
    sample.a0.6 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(625)
    sample.0.6 <- rbind(sample.p0.6, sample.a0.6)
     sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.6)[sp_cols]
  names(sample.0.6)[pred_cols]
    
    sample.p0.8 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(444)
    sample.a0.8 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(556)
    sample.0.8 <- rbind(sample.p0.8, sample.a0.8)
     sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.8)[sp_cols]
  names(sample.0.8)[pred_cols]
    
    modSpecies <- list(sample.0.2, sample.0.4, sample.0.5, sample.0.6, sample.0.8)
    
    
    for(j in 1:5) {
      if(j==1){
        
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  form_rf <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0((names(modSpecies[[j]])[pred_cols]), collapse = "+")))
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies[[j]], importance='impurity') 
 
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))  

      Outputs02=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs02)
        
      }else if(j==2){
        
        
          preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
        
  form_rf <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0((names(modSpecies[[j]])[pred_cols]), collapse = "+")))
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies[[j]], importance='impurity') 
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))  

      Outputs04=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs04)
        
      }else if(j==3){
        
        
         preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
 form_rf <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0((names(modSpecies[[j]])[pred_cols]), collapse = "+")))
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies[[j]], importance='impurity') 
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16      
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))  

      Outputs05=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs05)
        
      }else if(j==4){
        
        
     preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
 form_rf <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0((names(modSpecies[[j]])[pred_cols]), collapse = "+")))
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies[[j]], importance='impurity')  
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))  

      Outputs06=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs06)
        
      }else if(j==5){
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
 form_rf <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0((names(modSpecies[[j]])[pred_cols]), collapse = "+")))
  
  ## Favourability and Probability
  
  Model<-ranger(form_rf, data= modSpecies[[j]], importance='impurity') 
  Pred<- predict(
    Model,
    data =preds,
    predict.all = FALSE,
    num.trees = Model$num.trees)

  prediction <- data.frame(predsXY[,1:2], Pred$predictions)
  prediction$Pred.predictions[ prediction$Pred.predictions == 1] <- 1 - 2.2e-16
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))  

      Outputs08=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs08)
        
      
      }
      
    }
    
    Outputs=list( Outputs02, Outputs04, Outputs05, Outputs06, Outputs08)
    Outputs.l[[i]] <- Outputs 
  }
  
  return(Outputs.l)
  
}



SsdmRF <- Stratified.SDM(envData)
  
  
  
 #########  GAM  #########
  
   ###### RANDOM SAMPLE  ######

  Random.SDM <- function(alpha=-0.05,value=NULL,Predictors=NULL, SamplePoints=NULL) {
  
    set.seed(999)
    myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
    
    Outputs.l <- list()
  
  #widespread species, at the moment
    for (i in 1:length(myRandNum)) {
  set.seed(999)    
  random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                convert.to.PA = FALSE,
                                                species.type = "additive",
                                                realistic.sp = TRUE,
                                                plot = FALSE)
  set.seed(999)
  new.pres<-convertToPA(random.sp, 
                        beta="random",
                        alpha = alpha, plot = FALSE, 
                        species.prevalence = 0.2) 

  
 
    presence.points <- sampleOccurrences(new.pres,
                                         n = SamplePoints, 
                                         type = "presence-absence",
                                         sample.prevalence = value,
                                         detection.probability = 1,
                                         correct.by.suitability = FALSE,
                                         plot = FALSE)  
  
  
  PresAbs=presence.points$sample.points[, c( "x", "y",  "Observed")]
  coordinates(PresAbs)<-~x+y 
  crs(PresAbs)<-crs(Predictors) 
  values <- raster::extract(Predictors[[myRandNum[[i]]]],PresAbs, df=T)
  values <- values[,-1]
 
  modSpecies<- data.frame(pres = PresAbs@data[,1], x = PresAbs$x, y = PresAbs$y, values[1:ncol(values)])
  sp_cols <- 1
  pred_cols <- 4:8
  names(modSpecies)[sp_cols]
  names(modSpecies)[pred_cols]
      
  ## newdata for prediction
  
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
  form_gam <- as.formula(paste0(names(modSpecies)[sp_cols], "~", paste0("s(", names(modSpecies)[pred_cols], ")", collapse = "+")))
  Model <- gam(form_gam, family = binomial, data = modSpecies)
  prediction <- predict(Model, newdata = preds, type = "response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)

        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(value + (x)/(1-x)))  
  
  Outputs=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"=modSpecies) 
              
 Outputs.l[[i]] <- Outputs 
  }
   
  return(Outputs.l)
  
}


listV <- list(0.2, 0.4, 0.5, 0.6, 0.8)


for(i in 1:5) {
  if(i==1){
    
    Rsdm02GAM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm02GAM)
    
  }else if(i==2){
    
    Rsdm04GAM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm04GAM)
    
  }else if(i==3){
    
    
    Rsdm05GAM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm05GAM)
    
  }else if(i==4){
    
    
    Rsdm06GAM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm06GAM)
    
  }else if(i==5){
    
    
    Rsdm08GAM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print( Rsdm08GAM)
    
    
  }
  
}

    ####### STRATIFIE SAMPLE ######
  
Stratified.SDM <- function(Predictors=NULL, alpha= -0.05) {
  
  
  set.seed(999)
  myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
  
  Outputs.l <- list()
  
  #widespread species, at the moment
  for (i in 1:length(myRandNum)) {
    set.seed(999)    
    random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                  convert.to.PA = FALSE,
                                                  species.type = "additive",
                                                  realistic.sp = TRUE,
                                                  plot = FALSE)
    set.seed(999)
    new.pres<-convertToPA(random.sp, 
                          beta="random",
                          alpha = alpha, plot = FALSE, 
                          species.prevalence = 0.2) 
    
    
    
   r <- new.pres$pa.raster
  r.grid <- new.pres$pa.raster
  res(r.grid) <- 0.3
  p <- rasterToPolygons(r.grid, n=4, na.rm = T)
  p <- as(p, "SpatialPolygons")
  
  df.r <- as.data.frame(r, xy=TRUE, na.rm=TRUE)
  df.r$layer[df.r$layer==TRUE]<-1
  df.r$layer[df.r$layer==FALSE]<-0
  
  coordinates(df.r)<- ~ x + y
  crs(df.r) <- crs(p)
  id <- over(df.r,p) 
  df.r <- data.frame(x = coordinates(df.r)[,1], y = coordinates(df.r)[,2], pa = df.r@data[,1])
  df.r <- cbind(df.r, id)
  
  p.sf <- st_as_sf(p)
  p.centroids <- st_centroid(p.sf)
  p.centroids <- p.centroids %>% rownames_to_column(var="id")
  p.ctrd.sp <- as_Spatial(p.centroids)
  pa.centroids <- merge(df.r, p.ctrd.sp, by="id")
  df.pa.centroids <- pa.centroids %>% group_by(id)%>% mutate(pres = if_else(any(pa == 1), 1, 0))%>% dplyr::select(pres, coords.x1, coords.x2)%>%unique()
  df.pa.centroids <- df.pa.centroids[,-1]
  
  coordinates(df.pa.centroids)<- ~ coords.x1 + coords.x2
  crs(df.pa.centroids) <- crs(Predictors)
  values <- raster::extract(Predictors[[myRandNum[[i]]]], df.pa.centroids, df=T, na.rm = TRUE)
  values <- values[,-1]
  df.pred.centroids <- cbind(pres=df.pa.centroids@data[,1], x=df.pa.centroids$coords.x1, y= df.pa.centroids$coords.x2, values)
  df.pred.centroids <- df.pred.centroids %>% filter(complete.cases(.))
    
    sample.p0.5 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(333)
    sample.a0.5 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(667) 
    sample.0.5 <- rbind(sample.p0.5, sample.a0.5)
    sp_cols <- 1
    pred_cols <- 4:8
    names(sample.0.5)[sp_cols]
    names(sample.0.5)[pred_cols]
    
    sample.p0.2 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(167)
    sample.a0.2 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(833)
    sample.0.2 <- rbind(sample.p0.2, sample.a0.2)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.2)[sp_cols]
  names(sample.0.2)[pred_cols]
    
    sample.p0.4 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(286)
    sample.a0.4 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(714)
    sample.0.4 <- rbind(sample.p0.4, sample.a0.4)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.4)[sp_cols]
  names(sample.0.4)[pred_cols]
    
    sample.p0.6 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(375)
    sample.a0.6 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(625)
    sample.0.6 <- rbind(sample.p0.6, sample.a0.6)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.6)[sp_cols]
  names(sample.0.6)[pred_cols]
    
    sample.p0.8 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(444)
    sample.a0.8 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(556)
    sample.0.8 <- rbind(sample.p0.8, sample.a0.8)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.8)[sp_cols]
  names(sample.0.8)[pred_cols]
    
    modSpecies <- list(sample.0.2, sample.0.4, sample.0.5, sample.0.6, sample.0.8)
    
    
    for(j in 1:5) {
      if(j==1){
        
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 form_gam <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0("s(", names(modSpecies[[j]])[pred_cols], ")", collapse = "+")))
      Model <- gam(form_gam, family = binomial, data = modSpecies[[j]])
      prediction <- predict(Model, newdata = preds, type = "response")
      df.prediction <- data.frame(Pred=prediction)
      prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))  

      Outputs02=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs02)
        
      }else if(j==2){
        
        
          preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 form_gam <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0("s(", names(modSpecies[[j]])[pred_cols], ")", collapse = "+")))
      Model <- gam(form_gam, family = binomial, data = modSpecies[[j]])
      prediction <- predict(Model, newdata = preds, type = "response")
      df.prediction <- data.frame(Pred=prediction)
      prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))  

      Outputs04=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs04)
        
      }else if(j==3){
        
        
         preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 form_gam <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0("s(", names(modSpecies[[j]])[pred_cols], ")", collapse = "+")))
      Model <- gam(form_gam, family = binomial, data = modSpecies[[j]])
      prediction <- predict(Model, newdata = preds, type = "response")
      df.prediction <- data.frame(Pred=prediction)
      prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
   Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))  

      Outputs05=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs05)
        
      }else if(j==4){
        
        
     preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 form_gam <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0("s(", names(modSpecies[[j]])[pred_cols], ")", collapse = "+")))
      Model <- gam(form_gam, family = binomial, data = modSpecies[[j]])
      prediction <- predict(Model, newdata = preds, type = "response")
      df.prediction <- data.frame(Pred=prediction)
      prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))  

      Outputs06=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs06)
        
      }else if(j==5){
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 form_gam <- as.formula(paste0(names(modSpecies[[j]])[sp_cols], "~", paste0("s(", names(modSpecies[[j]])[pred_cols], ")", collapse = "+")))
      Model <- gam(form_gam, family = binomial, data = modSpecies[[j]])
      prediction <- predict(Model, newdata = preds, type = "response")
      df.prediction <- data.frame(Pred=prediction)
      prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
        
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
   Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))  

      Outputs08=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs08)
        
      
      }
      
    }
    
    Outputs=list( Outputs02, Outputs04, Outputs05, Outputs06, Outputs08)
    Outputs.l[[i]] <- Outputs 
  }
  
  return(Outputs.l)
  
}



SsdmGAM <- Stratified.SDM(envData)
  
  
 ########## GBM ##########
  
  
   ###### RANDOM SAMPLE  ######

  Random.SDM <- function(alpha=-0.05,value=NULL,Predictors=NULL, SamplePoints=NULL) {
  
    set.seed(999)
    myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
    
    Outputs.l <- list()
  
  #widespread species, at the moment
    for (i in 1:length(myRandNum)) {
  set.seed(999)    
  random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                convert.to.PA = FALSE,
                                                species.type = "additive",
                                                realistic.sp = TRUE,
                                                plot = FALSE)
  set.seed(999)
  new.pres<-convertToPA(random.sp, 
                        beta="random",
                        alpha = alpha, plot = FALSE, 
                        species.prevalence = 0.2) 

  
 
    presence.points <- sampleOccurrences(new.pres,
                                         n = SamplePoints, 
                                         type = "presence-absence",
                                         sample.prevalence = value,
                                         detection.probability = 1,
                                         correct.by.suitability = FALSE,
                                         plot = FALSE)  
  
  
  PresAbs=presence.points$sample.points[, c( "x", "y",  "Observed")]
  coordinates(PresAbs)<-~x+y 
  crs(PresAbs)<-crs(Predictors) 
  values <- raster::extract(Predictors[[myRandNum[[i]]]],PresAbs, df=T)
  values <- values[,-1]
 
  modSpecies<- data.frame(pres = PresAbs@data[,1], x = PresAbs$x, y = PresAbs$y, values[1:ncol(values)])
  sp_cols <- 1
  pred_cols <- 4:8
  names(modSpecies)[sp_cols]
  names(modSpecies)[pred_cols]
      
  ## newdata for prediction
  
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 Model <- gbm.step(data = modSpecies, 
                                    gbm.x = names(modSpecies)[pred_cols],
                                     gbm.y = names(modSpecies)[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(value + (x)/(1-x)))  
  
  Outputs=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"=modSpecies) 
              
 Outputs.l[[i]] <- Outputs 
  }
   
  return(Outputs.l)
  
}


listV <- list(0.2, 0.4, 0.5, 0.6, 0.8)


for(i in 1:5) {
  if(i==1){
    
    Rsdm02GBM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm02GBM)
    
  }else if(i==2){
    
    Rsdm04GBM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm04GBM)
    
  }else if(i==3){
    
    
    Rsdm05GBM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm05GBM)
    
  }else if(i==4){
    
    
    Rsdm06GBM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print(Rsdm06GBM)
    
  }else if(i==5){
    
    
    Rsdm08GBM <- Random.SDM(alpha=-0.05,value= listV[[i]],Predictors=envData, SamplePoints=1000)
    
    
    print( Rsdm08GBM)
    
    
  }
  
}

    ####### STRATIFIED SAMPLE ######
  
Stratified.SDM <- function(Predictors=NULL, alpha= -0.05) {
  
  
  set.seed(999)
  myRandNum=replicate(50, sample(1:19,size=5, replace = FALSE), simplify = FALSE)
  
  Outputs.l <- list()
  
  #widespread species, at the moment
  for (i in 1:length(myRandNum)) {
    set.seed(999)    
    random.sp <- virtualspecies::generateRandomSp(Predictors[[myRandNum[[i]]]],
                                                  convert.to.PA = FALSE,
                                                  species.type = "additive",
                                                  realistic.sp = TRUE,
                                                  plot = FALSE)
    set.seed(999)
    new.pres<-convertToPA(random.sp, 
                          beta="random",
                          alpha = alpha, plot = FALSE, 
                          species.prevalence = 0.2) 
    
    
    
   r <- new.pres$pa.raster
  r.grid <- new.pres$pa.raster
  res(r.grid) <- 0.3
  p <- rasterToPolygons(r.grid, n=4, na.rm = T)
  p <- as(p, "SpatialPolygons")
  
  df.r <- as.data.frame(r, xy=TRUE, na.rm=TRUE)
  df.r$layer[df.r$layer==TRUE]<-1
  df.r$layer[df.r$layer==FALSE]<-0
  
  coordinates(df.r)<- ~ x + y
  crs(df.r) <- crs(p)
  id <- over(df.r,p) 
  df.r <- data.frame(x = coordinates(df.r)[,1], y = coordinates(df.r)[,2], pa = df.r@data[,1])
  df.r <- cbind(df.r, id)
  
  p.sf <- st_as_sf(p)
  p.centroids <- st_centroid(p.sf)
  p.centroids <- p.centroids %>% rownames_to_column(var="id")
  p.ctrd.sp <- as_Spatial(p.centroids)
  pa.centroids <- merge(df.r, p.ctrd.sp, by="id")
  df.pa.centroids <- pa.centroids %>% group_by(id)%>% mutate(pres = if_else(any(pa == 1), 1, 0))%>% dplyr::select(pres, coords.x1, coords.x2)%>%unique()
  df.pa.centroids <- df.pa.centroids[,-1]
  
  coordinates(df.pa.centroids)<- ~ coords.x1 + coords.x2
  crs(df.pa.centroids) <- crs(Predictors)
  values <- raster::extract(Predictors[[myRandNum[[i]]]], df.pa.centroids, df=T, na.rm = TRUE)
  values <- values[,-1]
  df.pred.centroids <- cbind(pres=df.pa.centroids@data[,1], x=df.pa.centroids$coords.x1, y= df.pa.centroids$coords.x2, values)
  df.pred.centroids <- df.pred.centroids %>% filter(complete.cases(.))
    
    sample.p0.5 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(333)
    sample.a0.5 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(667) 
    sample.0.5 <- rbind(sample.p0.5, sample.a0.5)
    sp_cols <- 1
    pred_cols <- 4:8
    names(sample.0.5)[sp_cols]
    names(sample.0.5)[pred_cols]
    
    sample.p0.2 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(167)
    sample.a0.2 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(833)
    sample.0.2 <- rbind(sample.p0.2, sample.a0.2)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.2)[sp_cols]
  names(sample.0.2)[pred_cols]
    
    sample.p0.4 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(286)
    sample.a0.4 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(714)
    sample.0.4 <- rbind(sample.p0.4, sample.a0.4)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.4)[sp_cols]
  names(sample.0.4)[pred_cols]
    
    sample.p0.6 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(375)
    sample.a0.6 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(625)
    sample.0.6 <- rbind(sample.p0.6, sample.a0.6)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.6)[sp_cols]
  names(sample.0.6)[pred_cols]
    
    sample.p0.8 <- df.pred.centroids %>%filter(!pres==0)%>% as.data.frame()%>% sample_n(444)
    sample.a0.8 <- df.pred.centroids %>%filter(!pres==1)%>% as.data.frame()%>% sample_n(556)
    sample.0.8 <- rbind(sample.p0.8, sample.a0.8)
    sp_cols <- 1
  pred_cols <- 4:8
  names(sample.0.8)[sp_cols]
  names(sample.0.8)[pred_cols]
    
    modSpecies <- list(sample.0.2, sample.0.4, sample.0.5, sample.0.6, sample.0.8)
    
    
    for(j in 1:5) {
      if(j==1){
        
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 Model <- gbm.step(data = modSpecies[[j]], 
                                    gbm.x = names(modSpecies[[j]])[pred_cols],
                                     gbm.y = names(modSpecies[[j]])[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
   Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))  

      Outputs02=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs02)
        
      }else if(j==2){
        
        
          preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 Model <- gbm.step(data = modSpecies[[j]], 
                                    gbm.x = names(modSpecies[[j]])[pred_cols],
                                     gbm.y = names(modSpecies[[j]])[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))  

      Outputs04=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs04)
        
      }else if(j==3){
        
        
         preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 Model <- gbm.step(data = modSpecies[[j]], 
                                    gbm.x = names(modSpecies[[j]])[pred_cols],
                                     gbm.y = names(modSpecies[[j]])[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))  

      Outputs05=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs05)
        
      }else if(j==4){
        
        
     preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
 Model <- gbm.step(data = modSpecies[[j]], 
                                    gbm.x = names(modSpecies[[j]])[pred_cols],
                                     gbm.y = names(modSpecies[[j]])[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))  

      Outputs06=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs06)
        
      }else if(j==5){
        
  preds <- as.data.frame(Predictors[[myRandNum[[i]]]]) %>%drop_na()
  
  predsXY <- as.data.frame(Predictors[[myRandNum[[i]]]], xy=T) %>%drop_na()
  
  ## Favourability and Probability
  
  Model <- gbm.step(data = modSpecies[[j]], 
                                    gbm.x = names(modSpecies[[j]])[pred_cols],
                                     gbm.y = names(modSpecies[[j]])[sp_cols], 
                                     family = 'bernoulli',
                                     tree.complexity = 5,
                                     bag.fraction = 0.75,
                                    learning.rate = 0.005,
                                     verbose=F)
  prediction <- predict.gbm(Model, newdata = preds, n.trees=Model$gbm.call$best.trees, type="response")
  df.prediction <- data.frame(Pred=prediction)
  prediction <- data.frame(predsXY[,1:2], df.prediction$Pred)
  
  Pred <- rasterFromXYZ(prediction, crs="+proj=longlat +datum=WGS84 +no_defs")
  Fav <- calc(Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))  

      Outputs08=list( "Mods"=Model, "Raster"=Pred, "RasterF" = Fav, "ModelDatabase"= modSpecies[[j]])
        print(Outputs08)
        
      
      }
      
    }
    
    Outputs=list( Outputs02, Outputs04, Outputs05, Outputs06, Outputs08)
    Outputs.l[[i]] <- Outputs 
  }
  
  return(Outputs.l)
  
}



SsdmGBM <- Stratified.SDM(envData)
  
