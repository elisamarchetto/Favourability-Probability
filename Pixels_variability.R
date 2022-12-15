########## SINGLE SPECIES CODE ##########
          #####################
  
  library(tidyverse)
  library(scico)
  library(raster)
  library(ggplot2)
  library(rnaturalearth)
  library(sf)
  library(patchwork)
  library(gghalves)
  
###### Random sampling ##########

#### GLM ####

rstack_GLMR <- stack(Rsdm02GLM$Raster$pres_P, Rsdm04GLM$Raster$pres_P, Rsdm05GLM$Raster$pres_P, Rsdm06GLM$Raster$pres_P, Rsdm08GLM$Raster$pres_P)
CVglmR <- cv(rstack_GLMR)  
  #FAV
 
fav0.2GLM <- calc(Rsdm02GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GLM <- calc(Rsdm04GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GLM <- calc(Rsdm05GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GLM <- calc(Rsdm06GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GLM <- calc(Rsdm08GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_RGLM <- stack(fav0.2GLM, fav0.4GLM, fav0.5GLM, fav0.6GLM, fav0.8GLM)
CVFav_RGLM <- cv(rstack_Fav_RGLM)
                  
                  
                  
diff_probfavGLM.R <- CVglmR - CVFav_RGLM              

  # RANDOM FOREST

rstack_rfR <- stack(Rsdm02RF$Raster$Pred.predictions, Rsdm04RF$Raster$Pred.predictions, Rsdm05RF$Raster$Pred.predictions, Rsdm06RF$Raster$Pred.predictions, Rsdm08RF$Raster$Pred.predictions)
CVrfR <- cv(rstack_rfR)

fav0.2RF <- calc(Rsdm02RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4RF <- calc(Rsdm04RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5RF <- calc(Rsdm05RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6RF <- calc(Rsdm06RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8RF <- calc(Rsdm08RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_RFR <- stack(fav0.2RF, fav0.4RF, fav0.5RF, fav0.6RF, fav0.8RF)
CVFav_RFR <- cv(rstack_Fav_RFR)
                 
                 
diff_probfavRF.R <- CVrfR - CVFav_RFR  
                 
##### GAM

rstack_gamR<- stack(Rsdm02GAM$Raster$df.prediction.Pred, Rsdm04GAM$Raster$df.prediction.Pred, Rsdm05GAM$Raster$df.prediction.Pred, Rsdm06GAM$Raster$df.prediction.Pred, Rsdm08GAM$Raster$df.prediction.Pred)
CVgamR <- cv(rstack_gamR)

fav0.2GAM <- calc(Rsdm02GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GAM <- calc(Rsdm04GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GAM <- calc(Rsdm05GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GAM <- calc(Rsdm06GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GAM <- calc(Rsdm08GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_GAMR <- stack(fav0.2GAM, fav0.4GAM, fav0.5GAM, fav0.6GAM, fav0.8GAM)
CVFav_GAMR <- cv(rstack_Fav_GAMR)                 

diff_probfavGAM.R <- CVgamR - CVFav_GAMR     

##### GBM
  
rstack_gbmR<- stack(Rsdm02GBM$Raster$df.prediction.Pred, Rsdm04GBM$Raster$df.prediction.Pred, Rsdm05GBM$Raster$df.prediction.Pred, Rsdm06GBM$Raster$df.prediction.Pred, Rsdm08GBM$Raster$df.prediction.Pred)
CVgbmR <- cv(rstack_gbmR)
                  
fav0.2GBM <- calc(Rsdm02GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GBM <- calc(Rsdm04GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GBM <- calc(Rsdm05GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GBM <- calc(Rsdm06GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GBM <- calc(Rsdm08GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_GBMR <- stack(fav0.2GBM, fav0.4GBM, fav0.5GBM, fav0.6GBM, fav0.8GBM)
CVFav_GBMR <- cv(rstack_Fav_GBMR)                   

                  
diff_probfavGBM.R <- CVgbmR - CVFav_GBMR


######## Stratified sampling ########


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


######## GLM

rstack_glmS <- stack(Ssdm02GLM$Raster$pres_P, Ssdm04GLM$Raster$pres_P, Ssdm05GLM$Raster$pres_P, Ssdm06GLM$Raster$pres_P, Ssdm08GLM$Raster$pres_P)
CVglmS <- cv(rstack_glmS)
  
  #FAV
fav0.2GLM <- calc(Ssdm02GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GLM <- calc(Ssdm04GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GLM <- calc(Ssdm05GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GLM <- calc(Ssdm06GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GLM <- calc(Ssdm08GLM$Raster$pres_P, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_SGLM <- stack(fav0.2GLM, fav0.4GLM, fav0.5GLM, fav0.6GLM, fav0.8GLM)
CVFav_SGLM <- cv(rstack_Fav_SGLM)   

 diff_probfavGLM.S <- CVglmS - CVFav_SGLM                  
                  
####### RANDOM FOREST

rstack_rfS <- stack(Ssdm02RF$Raster$Pred.predictions, Ssdm04RF$Raster$Pred.predictions, Ssdm05RF$Raster$Pred.predictions, Ssdm06RF$Raster$Pred.predictions, Ssdm08RF$Raster$Pred.predictions)
CVrfS <- cv(rstack_rfS)

fav0.2RF <- calc(Ssdm02RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4RF <- calc(Ssdm04RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5RF <- calc(Ssdm05RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6RF <- calc(Ssdm06RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8RF <- calc(Ssdm08RF$Raster$Pred.predictions, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_RFS <- stack(fav0.2RF, fav0.4RF, fav0.5RF, fav0.6RF, fav0.8RF)
CVFav_RFS <- cv(rstack_Fav_RFS)     
                 
                 
diff_probfavRF.S <- CVrfS - CVFav_RFS    

##### GAM

rstack_gamS<- stack(Ssdm02GAM$Raster$df.prediction.Pred, Ssdm04GAM$Raster$df.prediction.Pred, Ssdm05GAM$Raster$df.prediction.Pred, Ssdm06GAM$Raster$df.prediction.Pred, Ssdm08GAM$Raster$df.prediction.Pred)
CVgamS <- cv(rstack_gamS)

fav0.2GAM <- calc(Ssdm02GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GAM <- calc(Ssdm04GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GAM <- calc(Ssdm05GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GAM <- calc(Ssdm06GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GAM <- calc(Ssdm08GAM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_GAMS <- stack(fav0.2GAM, fav0.4GAM, fav0.5GAM, fav0.6GAM, fav0.8GAM)
CVFav_GAMS <- cv(rstack_Fav_GAMS) 
 
                  
 diff_probfavGAM.S <- CVgamS - CVFav_GAMS
                  
                  
  #GBM
  
rstack_gbmS<- stack(Ssdm02GBM$Raster$df.prediction.Pred, Ssdm04GBM$Raster$df.prediction.Pred, Ssdm05GBM$Raster$df.prediction.Pred, Ssdm06GBM$Raster$df.prediction.Pred, Ssdm08GBM$Raster$df.prediction.Pred)
CVgbmS <- cv(rstack_gbmS)
                  
fav0.2GBM <- calc(Ssdm02GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.2 + (x)/(1-x)))
fav0.4GBM <- calc(Ssdm04GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.4 + (x)/(1-x)))
fav0.5GBM <- calc(Ssdm05GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.5 + (x)/(1-x)))
fav0.6GBM <- calc(Ssdm06GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.6 + (x)/(1-x)))
fav0.8GBM <- calc(Ssdm08GBM$Raster$df.prediction.Pred, function(x) ((x)/(1-x))/(0.8 + (x)/(1-x)))

rstack_Fav_GBMS <- stack(fav0.2GBM, fav0.4GBM, fav0.5GBM, fav0.6GBM, fav0.8GBM)
CVFav_GBMS <- cv(rstack_Fav_GBMS) 
                  
diff_probfavGBM.S <- CVgbmS - CVFav_GBMS                  


############# BOXPLOT DIFF #############
                  
df.diff_probfavGLM.R <- as.data.frame(diff_probfavGLM.R)%>%drop_na()
colnames(df.diff_probfavGLM.R)<-"values"
df.diff_probfavGLM.S <- as.data.frame(diff_probfavGLM.S)%>%drop_na()
colnames(df.diff_probfavGLM.S)<-"values"
df.diff_probfavRF.R <- as.data.frame(diff_probfavRF.R)%>%drop_na()
colnames(df.diff_probfavRF.R)<-"values"
df.diff_probfavRF.S <- as.data.frame(diff_probfavRF.S)%>%drop_na()
colnames(df.diff_probfavRF.S)<-"values"
df.diff_probfavGAM.R <- as.data.frame(diff_probfavGAM.R)%>%drop_na()
colnames(df.diff_probfavGAM.R)<-"values"
df.diff_probfavGAM.S <- as.data.frame(diff_probfavGAM.S)%>%drop_na()
colnames(df.diff_probfavGAM.S)<-"values"
df.diff_probfavGBM.R <- as.data.frame(diff_probfavGBM.R)%>%drop_na()
colnames(df.diff_probfavGBM.R)<-"values"
df.diff_probfavGBM.S <- as.data.frame(diff_probfavGBM.S)%>%drop_na()
colnames(df.diff_probfavGBM.S)<-"values"

df.diff_probfavGLM.R$model <- "GLM A"
df.diff_probfavGLM.S$model <- "GLM B"
df.diff_probfavGLM.R$sampling <- "Random"
df.diff_probfavGLM.S$sampling <- "Stratified"

df.diff_probfavRF.R$model <- "RF A"
df.diff_probfavRF.S$model <- "RF B"
df.diff_probfavRF.R$sampling <- "Random"
df.diff_probfavRF.S$sampling <- "Stratified"                  


df.diff_probfavGAM.R$model <- "GAM A"
df.diff_probfavGAM.S$model <- "GAM B" 
df.diff_probfavGAM.R$sampling <- "Random"
df.diff_probfavGAM.S$sampling <- "Stratified"

df.diff_probfavGBM.R$model <- "BRT A"
df.diff_probfavGBM.S$model <- "BRT B" 
df.diff_probfavGBM.R$sampling <- "Random"
df.diff_probfavGBM.S$sampling <- "Stratified"

df.Tot <- rbind(df.diff_probfavGLM.R, df.diff_probfavGLM.S, df.diff_probfavRF.R, df.diff_probfavRF.S, df.diff_probfavGAM.R, df.diff_probfavGAM.S, df.diff_probfavGBM.R, df.diff_probfavGBM.S)



  pp <- c("GLM","GLM", "GAM", "GAM", "RF", "RF","BRT","BRT")

p <- ggplot(df.Tot, aes(model, values, fill = sampling))+
  geom_half_violin(alpha = 0.6, side = "l")+
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, side = "r")+
  geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6) +
  scale_fill_manual( values=c("#D55E00", "#0072B2"))+
  labs(x="Model", y="Difference value between CVs", fill="Sampling method")+
  scale_x_discrete(labels=pp, limits=c("GLM A","GLM B", "GAM A", "GAM B", "RF A", "RF B","BRT A","BRT B"))+
  theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(1.0, 'cm'))  

  

ggsave(plot = p,
       filename = "diffCV.jpeg",
       width = 10,
       height = 8,
       dpi = 600) 

########### Maps CV favourability probability and difference for each statistical model ###########

     e <- extent(-24.33333, 40.16667, 27.66667, 80.5)
     r1 <- crop( CVglmR, e)
     r2 <- crop( CVglmS, e)
     
     fr1 <- crop(CVFav_RGLM, e)
     fr2 <- crop(CVFav_SGLM, e)
     
     dr1 <- crop(diff_probfavGLM.R, e)
     dr2 <- crop(diff_probfavGLM.S, e)
     
     stack_probR <- stack(r1, CVgamR,CVrfR, CVgbmR)
     names(stack_probR) <- c("GLM", "GAM", "RF","BRT")
     stack_probS <- stack(r1, CVgamS, CVrfS,CVgbmS)
     names(stack_probS) <- c("GLM", "GAM", "RF","BRT")
     
     stack_favR <- stack(fr1, CVFav_GAMR, CVFav_RFR, CVFav_GBMR)
     names(stack_favR) <- c("GLM", "GAM", "RF","BRT")
     stack_favS <- stack(fr2, CVFav_GAMS, CVFav_RFS, CVFav_GBMS)
     names(stack_favS) <- c("GLM", "GAM", "RF","BRT")
     
     stack_diffR <- stack(dr1,  diff_probfavGAM.R,diff_probfavRF.R, diff_probfavGBM.R)
     names(stack_diffR) <- c("GLM", "GAM", "RF","BRT")
     stack_diffS <- stack(dr2,  diff_probfavGAM.S, diff_probfavRF.S,diff_probfavGBM.S)
     names(stack_diffS) <- c("GLM", "GAM", "RF","BRT")
     
     
      stack_probS_df <-
  as.data.frame(stack_probS, xy = TRUE) %>%
  na.omit()

GGstack_probS_df <- 
  stack_probS_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")

stack_probR_df <-
  as.data.frame(stack_probR, xy = TRUE) %>%
  na.omit()

GGstack_probR_df <- 
  stack_probR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")

stack_favS_df <-
  as.data.frame(stack_favS, xy = TRUE) %>%
  na.omit()

GGstack_favS_df <- 
  stack_favS_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")

stack_favR_df <-
  as.data.frame(stack_favR, xy = TRUE) %>%
  na.omit()

GGstack_favR_df <- 
  stack_favR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")



stack_diffS_df <-
  as.data.frame(stack_diffS, xy = TRUE) %>%
  na.omit()

GGstack_diffS_df <- 
  stack_diffS_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")


stack_diffR_df <-
  as.data.frame(stack_diffR, xy = TRUE) %>%
  na.omit()

GGstack_diffR_df <- 
  stack_diffR_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")


######################
######################
world <- ne_coastline(scale = "medium", returnclass = "sf")

s1<- GGstack_probS_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(0.00000000,223.6068),
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


s2<- GGstack_favS_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(0.00000000,223.6068),
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

s3<- GGstack_diffS_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(-64.161011,49.23872),
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

s <- s1 + s2 + s3  + plot_annotation(title = "Stratified sampling", theme = theme(plot.title = element_text(size = 25, face = 'bold')))
       
ggsave(plot = s,
       filename = "s2.jpg",
       width = 25,
       height = 25,
       dpi = 600)

r1<- GGstack_probR_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(0.00000000,223.6068),
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
  coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)


r2<- GGstack_favR_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(0.00000000,223.6068),
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
  coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)

r3<- GGstack_diffR_df %>%
  mutate(across(variable, factor, levels=c("GLM","GAM","RF","BRT"))) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 4) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_scico(palette = "batlow",direction = 1,alpha = 0.7, limits=c(-65.900472,47.70545),
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

r <- r1 + r2 + r3  + plot_annotation(title = "Random sampling", theme = theme(plot.title = element_text(size = 25, face = 'bold')))

ggsave(plot = r,
       filename = "r2.jpg",
       width = 25,
       height = 25,
       dpi = 600)
