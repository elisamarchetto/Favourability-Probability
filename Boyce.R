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
  

              
########### Boyce functions for favpurability and probability of each statistical model #########
              
              
  BoyceGLM <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$Raster$pres_P, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGLMFav <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$RasterF$layer, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}
              
  BoyceRF <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$Raster$Pred.predictions, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceRFFav <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$RasterF$layer, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}          
              
  BoyceGAM <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$Raster$df.prediction.Pred, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

   BoyceGAMFav <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$RasterF$layer, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}             
              
BoyceGBM <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$Raster$df.prediction.Pred, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

              BoyceGBMFav <- function(x){
  library(ecospat)
  obs <- x$ModelDatabase[which(x$ModelDatabase$pres==1),]
  boyce <- ecospat.boyce(x$RasterF$layer, obs[,2:3], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}
            
              
######### return values #########
 #### Random ####

BoyceFunz02GLM.R <- lapply(Rsdm02GLM, BoyceGLM)
BoyceFunz04GLM.R <- lapply(Rsdm04GLM, BoyceGLM)
BoyceFunz05GLM.R <- lapply(Rsdm05GLM, BoyceGLM)
BoyceFunz06GLM.R <- lapply(Rsdm06GLM, BoyceGLM)
BoyceFunz08GLM.R <- lapply(Rsdm08GLM, BoyceGLM)
            

Boyce02GLM.R <- data.frame(Boyce02=unlist(BoyceFunz02GLM.R))
Boyce04GLM.R <- data.frame(Boyce04=unlist(BoyceFunz04GLM.R))
Boyce05GLM.R <- data.frame(Boyce05=unlist(BoyceFunz05GLM.R))
Boyce06GLM.R <- data.frame(Boyce06=unlist(BoyceFunz06GLM.R))
Boyce08GLM.R <- data.frame(Boyce08=unlist(BoyceFunz08GLM.R))

BoyceGLM.R_Prob <- cbind(Boyce02GLM.R, Boyce04GLM.R, Boyce05GLM.R, Boyce06GLM.R, Boyce08GLM.R)
colnames(BoyceGLM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.R_Prob, "BoyceGLM.R_Prob.csv")
BoyceGLM.R_Prob_mean <- apply(BoyceGLM.R_Prob, 2, mean)
BoyceGLM.R_Prob_median <- apply(BoyceGLM.R_Prob, 2, median)
              
BoyceFunz02GLM.R <- lapply(Rsdm02GLM, BoyceGLMFav)
BoyceFunz04GLM.R <- lapply(Rsdm04GLM, BoyceGLMFav)
BoyceFunz05GLM.R <- lapply(Rsdm05GLM, BoyceGLMFav)
BoyceFunz06GLM.R <- lapply(Rsdm06GLM, BoyceGLMFav)
BoyceFunz08GLM.R <- lapply(Rsdm08GLM, BoyceGLMFav)
            

Boyce02GLM.R <- data.frame(Boyce02=unlist(BoyceFunz02GLM.R))
Boyce04GLM.R <- data.frame(Boyce04=unlist(BoyceFunz04GLM.R))
Boyce05GLM.R <- data.frame(Boyce05=unlist(BoyceFunz05GLM.R))
Boyce06GLM.R <- data.frame(Boyce06=unlist(BoyceFunz06GLM.R))
Boyce08GLM.R <- data.frame(Boyce08=unlist(BoyceFunz08GLM.R))

BoyceGLM.R_Fav <- cbind(Boyce02GLM.R, Boyce04GLM.R, Boyce05GLM.R, Boyce06GLM.R, Boyce08GLM.R)
colnames(BoyceGLM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.R_Fav, "BoyceGLM.R_Fav.csv")
BoyceGLM.R_Fav_mean <- apply(BoyceGLM.R_Fav, 2, mean)
BoyceGLM.R_Fav_median <- apply(BoyceGLM.R_Fav, 2, median)
              
#########
              
BoyceFunz02RF.R <- lapply(Rsdm02RF, BoyceRF)
BoyceFunz04RF.R <- lapply(Rsdm04RF, BoyceRF)
BoyceFunz05RF.R <- lapply(Rsdm05RF, BoyceRF)
BoyceFunz06RF.R <- lapply(Rsdm06RF, BoyceRF)
BoyceFunz08RF.R <- lapply(Rsdm08RF, BoyceRF)
            

Boyce02RF.R <- data.frame(Boyce02=unlist(BoyceFunz02RF.R))
Boyce04RF.R <- data.frame(Boyce04=unlist(BoyceFunz04RF.R))
Boyce05RF.R <- data.frame(Boyce05=unlist(BoyceFunz05RF.R))
Boyce06RF.R <- data.frame(Boyce06=unlist(BoyceFunz06RF.R))
Boyce08RF.R <- data.frame(Boyce08=unlist(BoyceFunz08RF.R))

BoyceRF.R_Prob <- cbind(Boyce02RF.R, Boyce04RF.R, Boyce05RF.R, Boyce06RF.R, Boyce08RF.R)
colnames(BoyceRF.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.R_Prob, "BoyceRF.R_Prob.csv")
BoyceRF.R_Prob_mean <- apply(BoyceRF.R_Prob, 2, mean)
BoyceRF.R_Prob_median <- apply(BoyceRF.R_Prob, 2, median)
              
              
BoyceFunz02RF.R <- lapply(Rsdm02RF, BoyceRFFav)
BoyceFunz04RF.R <- lapply(Rsdm04RF, BoyceRFFav)
BoyceFunz05RF.R <- lapply(Rsdm05RF, BoyceRFFav)
BoyceFunz06RF.R <- lapply(Rsdm06RF, BoyceRFFav)
BoyceFunz08RF.R <- lapply(Rsdm08RF, BoyceRFFav)
            

Boyce02RF.R <- data.frame(Boyce02=unlist(BoyceFunz02RF.R))
Boyce04RF.R <- data.frame(Boyce04=unlist(BoyceFunz04RF.R))
Boyce05RF.R <- data.frame(Boyce05=unlist(BoyceFunz05RF.R))
Boyce06RF.R <- data.frame(Boyce06=unlist(BoyceFunz06RF.R))
Boyce08RF.R <- data.frame(Boyce08=unlist(BoyceFunz08RF.R))

BoyceRF.R_Fav <- cbind(Boyce02RF.R, Boyce04RF.R, Boyce05RF.R, Boyce06RF.R, Boyce08RF.R)
colnames(BoyceRF.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.R_Fav, "BoyceRF.R_Fav.csv")
BoyceRF.R_Fav_mean <- apply(BoyceRF.R_Fav, 2, mean)
BoyceRF.R_Fav_median <- apply(BoyceRF.R_Fav, 2, median)
              
              
 #############             
  
BoyceFunz02GAM.R <- lapply(Rsdm02GAM, BoyceGAM)
BoyceFunz04GAM.R <- lapply(Rsdm04GAM, BoyceGAM)
BoyceFunz05GAM.R <- lapply(Rsdm05GAM, BoyceGAM)
BoyceFunz06GAM.R <- lapply(Rsdm06GAM, BoyceGAM)
BoyceFunz08GAM.R <- lapply(Rsdm08GAM, BoyceGAM)
            

Boyce02GAM.R <- data.frame(Boyce02=unlist(BoyceFunz02GAM.R))
Boyce04GAM.R <- data.frame(Boyce04=unlist(BoyceFunz04GAM.R))
Boyce05GAM.R <- data.frame(Boyce05=unlist(BoyceFunz05GAM.R))
Boyce06GAM.R <- data.frame(Boyce06=unlist(BoyceFunz06GAM.R))
Boyce08GAM.R <- data.frame(Boyce08=unlist(BoyceFunz08GAM.R))

BoyceGAM.R_Prob <- cbind(Boyce02GAM.R, Boyce04GAM.R, Boyce05GAM.R, Boyce06GAM.R, Boyce08GAM.R)
colnames(BoyceGAM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.R_Prob, "BoyceGAM.R_Prob.csv")              
BoyceGAM.R_Prob_mean <- apply(BoyceGAM.R_Prob, 2, mean)
BoyceGAM.R_Prob_median <- apply(BoyceGAM.R_Prob, 2, median)
  
BoyceFunz02GAM.R <- lapply(Rsdm02GAM, BoyceGAMFav)
BoyceFunz04GAM.R <- lapply(Rsdm04GAM, BoyceGAMFav)
BoyceFunz05GAM.R <- lapply(Rsdm05GAM, BoyceGAMFav)
BoyceFunz06GAM.R <- lapply(Rsdm06GAM, BoyceGAMFav)
BoyceFunz08GAM.R <- lapply(Rsdm08GAM, BoyceGAMFav)
            

Boyce02GAM.R <- data.frame(Boyce02=unlist(BoyceFunz02GAM.R))
Boyce04GAM.R <- data.frame(Boyce04=unlist(BoyceFunz04GAM.R))
Boyce05GAM.R <- data.frame(Boyce05=unlist(BoyceFunz05GAM.R))
Boyce06GAM.R <- data.frame(Boyce06=unlist(BoyceFunz06GAM.R))
Boyce08GAM.R <- data.frame(Boyce08=unlist(BoyceFunz08GAM.R))

BoyceGAM.R_Fav <- cbind(Boyce02GAM.R, Boyce04GAM.R, Boyce05GAM.R, Boyce06GAM.R, Boyce08GAM.R)
colnames(BoyceGAM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.R_Fav, "BoyceGAM.R_Fav.csv")              
BoyceGAM.R_Fav_mean <- apply(BoyceGAM.R_Fav, 2, mean)
BoyceGAM.R_Fav_median <- apply(BoyceGAM.R_Fav, 2, median)

              
###########              
 
              
BoyceFunz02GBM.R <- lapply(Rsdm02GBM, BoyceGBM)
BoyceFunz04GBM.R <- lapply(Rsdm04GBM, BoyceGBM)
BoyceFunz05GBM.R <- lapply(Rsdm05GBM, BoyceGBM)
BoyceFunz06GBM.R <- lapply(Rsdm06GBM, BoyceGBM)
BoyceFunz08GBM.R <- lapply(Rsdm08GBM, BoyceGBM)
            

Boyce02GBM.R <- data.frame(Boyce02=unlist(BoyceFunz02GBM.R))
Boyce04GBM.R <- data.frame(Boyce04=unlist(BoyceFunz04GBM.R))
Boyce05GBM.R <- data.frame(Boyce05=unlist(BoyceFunz05GBM.R))
Boyce06GBM.R <- data.frame(Boyce06=unlist(BoyceFunz06GBM.R))
Boyce08GBM.R <- data.frame(Boyce08=unlist(BoyceFunz08GBM.R))

BoyceGBM.R_Prob <- cbind(Boyce02GBM.R, Boyce04GBM.R, Boyce05GBM.R, Boyce06GBM.R, Boyce08GBM.R)
colnames(BoyceGBM.R_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGBM.R_Prob, "BoyceGBM.R_Prob.csv")
BoyceGBM.R_Prob_mean <- apply(BoyceGBM.R_Prob, 2, mean)
BoyceGBM.R_Prob_median <- apply(BoyceGBM.R_Prob, 2, median)
              
              
BoyceFunz02GBM.R <- lapply(Rsdm02GBM, BoyceGBMFav)
BoyceFunz04GBM.R <- lapply(Rsdm04GBM, BoyceGBMFav)
BoyceFunz05GBM.R <- lapply(Rsdm05GBM, BoyceGBMFav)
BoyceFunz06GBM.R <- lapply(Rsdm06GBM, BoyceGBMFav)
BoyceFunz08GBM.R <- lapply(Rsdm08GBM, BoyceGBMFav)
            

Boyce02GBM.R <- data.frame(Boyce02=unlist(BoyceFunz02GBM.R))
Boyce04GBM.R <- data.frame(Boyce04=unlist(BoyceFunz04GBM.R))
Boyce05GBM.R <- data.frame(Boyce05=unlist(BoyceFunz05GBM.R))
Boyce06GBM.R <- data.frame(Boyce06=unlist(BoyceFunz06GBM.R))
Boyce08GBM.R <- data.frame(Boyce08=unlist(BoyceFunz08GBM.R))

BoyceGBM.R_Fav <- cbind(Boyce02GBM.R, Boyce04GBM.R, Boyce05GBM.R, Boyce06GBM.R, Boyce08GBM.R)
colnames(BoyceGBM.R_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGBM.R_Fav, "BoyceGBM.R_Fav.csv")
BoyceGBM.R_Fav_mean <- apply(BoyceGBM.R_Fav, 2, mean)
BoyceGBM.R_Fav_median <- apply(BoyceGBM.R_Fav, 2, median)
                            
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

Ssdm.pGLM <- Ssdm.l.prev(Ssdm)

Ssdm02GLM <- Ssdm.pGLM[[1]]
Ssdm04GLM <- Ssdm.pGLM[[2]]
Ssdm05GLM <- Ssdm.pGLM[[3]]
Ssdm06GLM <- Ssdm.pGLM[[4]]
Ssdm08GLM <- Ssdm.pGLM[[5]]
  
Ssdm.pRF <- Ssdm.l.prev(Ssdm)

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



BoyceFunz02GLM.S <- lapply(Ssdm02GLM, BoyceGLM)
BoyceFunz04GLM.S <- lapply(Ssdm04GLM, BoyceGLM)
BoyceFunz05GLM.S <- lapply(Ssdm05GLM, BoyceGLM)
BoyceFunz06GLM.S <- lapply(Ssdm06GLM, BoyceGLM)
BoyceFunz08GLM.S <- lapply(Ssdm08GLM, BoyceGLM)
            

Boyce02GLM.S <- data.frame(Boyce02=unlist(BoyceFunz02GLM.S))
Boyce04GLM.S <- data.frame(Boyce04=unlist(BoyceFunz04GLM.S))
Boyce05GLM.S <- data.frame(Boyce05=unlist(BoyceFunz05GLM.S))
Boyce06GLM.S <- data.frame(Boyce06=unlist(BoyceFunz06GLM.S))
Boyce08GLM.S <- data.frame(Boyce08=unlist(BoyceFunz08GLM.S))

BoyceGLM.S_Prob <- cbind(Boyce02GLM.S, Boyce04GLM.S, Boyce05GLM.S, Boyce06GLM.S, Boyce08GLM.S)
colnames(BoyceGLM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.S_Prob, "BoyceGLM.S_Prob.csv")
BoyceGLM.S_Prob_mean <- apply(BoyceGLM.S_Prob, 2, mean)
BoyceGLM.S_Prob_median <- apply(BoyceGLM.S_Prob, 2, median)
  

BoyceFunz02GLM.S <- lapply(Ssdm02GLM, BoyceGLMFav)
BoyceFunz04GLM.S <- lapply(Ssdm04GLM, BoyceGLMFav)
BoyceFunz05GLM.S <- lapply(Ssdm05GLM, BoyceGLMFav)
BoyceFunz06GLM.S <- lapply(Ssdm06GLM, BoyceGLMFav)
BoyceFunz08GLM.S <- lapply(Ssdm08GLM, BoyceGLMFav)
            

Boyce02GLM.S <- data.frame(Boyce02=unlist(BoyceFunz02GLM.S))
Boyce04GLM.S <- data.frame(Boyce04=unlist(BoyceFunz04GLM.S))
Boyce05GLM.S <- data.frame(Boyce05=unlist(BoyceFunz05GLM.S))
Boyce06GLM.S <- data.frame(Boyce06=unlist(BoyceFunz06GLM.S))
Boyce08GLM.S <- data.frame(Boyce08=unlist(BoyceFunz08GLM.S))

BoyceGLM.S_Fav <- cbind(Boyce02GLM.S, Boyce04GLM.S, Boyce05GLM.S, Boyce06GLM.S, Boyce08GLM.S)
colnames(BoyceGLM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGLM.S_Fav, "BoyceGLM.S_Fav.csv")
BoyceGLM.S_Fav_mean <- apply(BoyceGLM.S_Fav, 2, mean)
BoyceGLM.S_Fav_median <- apply(BoyceGLM.S_Fav, 2, median)
  
  
 ########### 

BoyceFunz02RF.S <- lapply(Ssdm02RF, BoyceRF)
BoyceFunz04RF.S <- lapply(Ssdm04RF, BoyceRF)
BoyceFunz05RF.S <- lapply(Ssdm05RF, BoyceRF)
BoyceFunz06RF.S <- lapply(Ssdm06RF, BoyceRF)
BoyceFunz08RF.S <- lapply(Ssdm08RF, BoyceRF)
            

Boyce02RF.S <- data.frame(Boyce02=unlist(BoyceFunz02RF.S))
Boyce04RF.S <- data.frame(Boyce04=unlist(BoyceFunz04RF.S))
Boyce05RF.S <- data.frame(Boyce05=unlist(BoyceFunz05RF.S))
Boyce06RF.S <- data.frame(Boyce06=unlist(BoyceFunz06RF.S))
Boyce08RF.S <- data.frame(Boyce08=unlist(BoyceFunz08RF.S))

BoyceRF.S_Prob <- cbind(Boyce02RF.S, Boyce04RF.S, Boyce05RF.S, Boyce06RF.S, Boyce08RF.S)
colnames(BoyceRF.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.S_Prob, "BoyceRF.S_Prob.csv")
BoyceRF.S_Prob_mean <- apply(BoyceRF.S_Prob, 2, mean)
BoyceRF.S_Prob_median <- apply(BoyceRF.S_Prob, 2, median)
  
BoyceFunz02RF.S <- lapply(Ssdm02RF, BoyceRFFav)
BoyceFunz04RF.S <- lapply(Ssdm04RF, BoyceRFFav)
BoyceFunz05RF.S <- lapply(Ssdm05RF, BoyceRFFav)
BoyceFunz06RF.S <- lapply(Ssdm06RF, BoyceRFFav)
BoyceFunz08RF.S <- lapply(Ssdm08RF, BoyceRFFav)
            

Boyce02RF.S <- data.frame(Boyce02=unlist(BoyceFunz02RF.S))
Boyce04RF.S <- data.frame(Boyce04=unlist(BoyceFunz04RF.S))
Boyce05RF.S <- data.frame(Boyce05=unlist(BoyceFunz05RF.S))
Boyce06RF.S <- data.frame(Boyce06=unlist(BoyceFunz06RF.S))
Boyce08RF.S <- data.frame(Boyce08=unlist(BoyceFunz08RF.S))

BoyceRF.S_Fav <- cbind(Boyce02RF.S, Boyce04RF.S, Boyce05RF.S, Boyce06RF.S, Boyce08RF.S)
colnames(BoyceRF.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceRF.S_Fav, "BoyceRF.S_Fav.csv")
BoyceRF.S_Fav_mean <- apply(BoyceRF.S_Fav, 2, mean)
BoyceRF.S_Fav_median <- apply(BoyceRF.S_Fav, 2, median)
  
###############  

BoyceFunz02GAM.S <- lapply(Ssdm02GAM, BoyceGAM)
BoyceFunz04GAM.S <- lapply(Ssdm04GAM, BoyceGAM)
BoyceFunz05GAM.S <- lapply(Ssdm05GAM, BoyceGAM)
BoyceFunz06GAM.S <- lapply(Ssdm06GAM, BoyceGAM)
BoyceFunz08GAM.S <- lapply(Ssdm08GAM, BoyceGAM)
            

Boyce02GAM.S <- data.frame(Boyce02=unlist(BoyceFunz02GAM.S))
Boyce04GAM.S <- data.frame(Boyce04=unlist(BoyceFunz04GAM.S))
Boyce05GAM.S <- data.frame(Boyce05=unlist(BoyceFunz05GAM.S))
Boyce06GAM.S <- data.frame(Boyce06=unlist(BoyceFunz06GAM.S))
Boyce08GAM.S <- data.frame(Boyce08=unlist(BoyceFunz08GAM.S))

BoyceGAM.S_Prob <- cbind(Boyce02GAM.S, Boyce04GAM.S, Boyce05GAM.S, Boyce06GAM.S, Boyce08GAM.S)
colnames(BoyceGAM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Prob, "BoyceGAM.S_Prob.csv")  
BoyceGAM.S_Prob_mean <- apply(BoyceGAM.S_Prob, 2, mean)
BoyceGAM.S_Prob_median <- apply(BoyceGAM.S_Prob, 2, median)
  
  
BoyceFunz02GAM.S <- lapply(Ssdm02GAM, BoyceGAMFav)
BoyceFunz04GAM.S <- lapply(Ssdm04GAM, BoyceGAMFav)
BoyceFunz05GAM.S <- lapply(Ssdm05GAM, BoyceGAMFav)
BoyceFunz06GAM.S <- lapply(Ssdm06GAM, BoyceGAMFav)
BoyceFunz08GAM.S <- lapply(Ssdm08GAM, BoyceGAMFav)
            

Boyce02GAM.S <- data.frame(Boyce02=unlist(BoyceFunz02GAM.S))
Boyce04GAM.S <- data.frame(Boyce04=unlist(BoyceFunz04GAM.S))
Boyce05GAM.S <- data.frame(Boyce05=unlist(BoyceFunz05GAM.S))
Boyce06GAM.S <- data.frame(Boyce06=unlist(BoyceFunz06GAM.S))
Boyce08GAM.S <- data.frame(Boyce08=unlist(BoyceFunz08GAM.S))

BoyceGAM.S_Fav <- cbind(Boyce02GAM.S, Boyce04GAM.S, Boyce05GAM.S, Boyce06GAM.S, Boyce08GAM.S)
colnames(BoyceGAM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Fav, "BoyceGAM.S_Fav.csv")  
BoyceGAM.S_Fav_mean <- apply(BoyceGAM.S_Fav, 2, mean)
BoyceGAM.S_Fav_median <- apply(BoyceGAM.S_Fav, 2, median)
  
 
################  

BoyceFunz02GBM.S <- lapply(Ssdm02GBM, BoyceGBM)
BoyceFunz04GBM.S <- lapply(Ssdm04GBM, BoyceGBM)
BoyceFunz05GBM.S <- lapply(Ssdm05GBM, BoyceGBM)
BoyceFunz06GBM.S <- lapply(Ssdm06GBM, BoyceGBM)
BoyceFunz08GBM.S <- lapply(Ssdm08GBM, BoyceGBM)
            

Boyce02GBM.S <- data.frame(Boyce02=unlist(BoyceFunz02GBM.S))
Boyce04GBM.S <- data.frame(Boyce04=unlist(BoyceFunz04GBM.S))
Boyce05GBM.S <- data.frame(Boyce05=unlist(BoyceFunz05GBM.S))
Boyce06GBM.S <- data.frame(Boyce06=unlist(BoyceFunz06GBM.S))
Boyce08GBM.S <- data.frame(Boyce08=unlist(BoyceFunz08GBM.S))

BoyceGBM.S_Prob <- cbind(Boyce02GBM.S, Boyce04GBM.S, Boyce05GBM.S, Boyce06GBM.S, Boyce08GBM.S)
colnames(BoyceGBM.S_Prob) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Prob, "BoyceGAM.S_Prob.csv")  
BoyceGBM.S_Prob_mean <- apply(BoyceGBM.S_Prob, 2, mean)
BoyceGBM.S_Prob_median <- apply(BoyceGBM.S_Prob, 2, median)


BoyceFunz02GBM.S <- lapply(Ssdm02GBM, BoyceGBMFav)
BoyceFunz04GBM.S <- lapply(Ssdm04GBM, BoyceGBMFav)
BoyceFunz05GBM.S <- lapply(Ssdm05GBM, BoyceGBMFav)
BoyceFunz06GBM.S <- lapply(Ssdm06GBM, BoyceGBMFav)
BoyceFunz08GBM.S <- lapply(Ssdm08GBM, BoyceGBMFav)
            

Boyce02GBM.S <- data.frame(Boyce02=unlist(BoyceFunz02GBM.S))
Boyce04GBM.S <- data.frame(Boyce04=unlist(BoyceFunz04GBM.S))
Boyce05GBM.S <- data.frame(Boyce05=unlist(BoyceFunz05GBM.S))
Boyce06GBM.S <- data.frame(Boyce06=unlist(BoyceFunz06GBM.S))
Boyce08GBM.S <- data.frame(Boyce08=unlist(BoyceFunz08GBM.S))

BoyceGBM.S_Fav <- cbind(Boyce02GBM.S, Boyce04GBM.S, Boyce05GBM.S, Boyce06GBM.S, Boyce08GBM.S)
colnames(BoyceGBM.S_Fav) <- c("0.2","0.4","0.5","0.6","0.8")
write.csv(BoyceGAM.S_Fav, "BoyceGAM.S_Fav.csv")  
BoyceGBM.S_Fav_mean <- apply(BoyceGBM.S_Fav, 2, mean)
BoyceGBM.S_Fav_median <- apply(BoyceGBM.S_Fav, 2, median)  
  
  
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
        legend.title=element_text(size=12,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(.7, 'cm'))    

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
        legend.title=element_text(size=12,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(.7, 'cm'))    

  
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
        legend.title=element_text(size=12,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(.7, 'cm'))    
 
  
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
        legend.title=element_text(size=12,face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(.7, 'cm'))    


  f <- glm + gam + rf + gbm + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
  ggsave(plot = f,
       filename = "f.jpeg",
       width = 18,
       height = 10,
       dpi = 600) 
  
