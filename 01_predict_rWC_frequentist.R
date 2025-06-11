
# source("00_prepare_data.R")
source("01_prepare_libraries.R")

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean_f_diag.csv"),sep=";", 
             fileEncoding = "UTF-16LE")
head(df) 
df$rWC<-df$AF.angepasst

for (f in grep("F.*X",colnames(df) ,value=T)){
  df[,f] <- relevel(as.factor(df[,f]), ref = "none")
  
}
str(df)

dfn<-cbind.data.frame(id=df$id,
                      sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      age=scale(df$Alter,T, T),
                      psy=df$Arzt,
                      MICF_mean=df$MICF_mean,
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                                 grep("Mobilitat", colnames(df))],
                      rWC=df$rWC, 
                      df[,paste0("F",0:9 )])

head(dfn)


# simple linear regression
model_sim = lm(rWC ~ MICF_mean, data=dfn)
summary(model_sim)
tab_model(model_sim)
tab_model(model_sim, file="rWC_MICF.html")
# then take this html file and make .png file
webshot("rWC_MICF.html", "rWC_MICF.png")
# Adjusted R-squared:   0.5064 

confint(model_sim)
newdata<-cbind.data.frame(MICF_mean=c(0:4))
predict(model_sim,newdata = newdata,interval = "prediction",level = 0.95)
predict(model_sim,newdata = newdata,interval = "confidence",level = 0.95)


# multiple linear regression
model_multi = lm(rWC ~ .-id -psy, , data=dfn)
summary(model_multi)


# Adjusted R-squared:   0.5838 
confint(model_multi)
newdata<-cbind.data.frame(MICF_mean=c(0:4), 
                          sex_f=c(0),
                          age=0, 
                          Regeln.und.Routinen=c(0:4),
                          Aufgabenplanung=c(0:4),
                          Flexibilitat=c(0:4),
                          fachliche.Kompetenzen=c(0:4),
                          Entscheidungsfahigkeit=c(0:4),
                          Spontanaktivitaten=c(0:4),        
                          Durchhaltefahigkeit=c(0:4),
                          Selbstbehauptungsfahigkeit=c(0:4),
                          Kontaktfahigkeit=c(0:4),
                          Gruppenfahigkeit=c(0:4),
                          dyadische.Beziehungen=c(0:4),     
                          Selbstpflege=c(0:4),
                          Mobilitat=c(0:4),
                          F0=0,
                          F1=0,
                          F2=0,
                          F3=0,
                          F4=0,
                          F5=0, 
                          F6=0,
                          F7=0,                        
                          F8=0,
                          F9=0, 
                          psy="H",
                          id=1)
newdata
predict(model_multi,newdata = newdata,interval = "prediction",level = 0.95)
predict(model_multi,newdata = newdata,interval = "confidence",level = 0.95)

### add other FXX FXXX
dfn<-cbind.data.frame(dfn,df[,grep("F*.XXX",colnames(df),value=T )])
dfn$F7XX<-df$F7XX
dfn$F9XX<-df$F9XX
head(dfn)

# Mixed effects Model
modelal = lmer(rWC ~ MICF_mean +(1|psy) -id -psy   , #+ (1|F3) + (1|F3XX) + (1|F3XXX) 
               REML=T, data=dfn)
summary(modelal)
#  93.54 the mean rWC across all MICF_mean and psy:
#  369.45 the variance within patients
#  70.99 the variance between psy
# only 19.2% of total variability is due to difference between psychiatrists.
tab_model(modelal)
ranef(modelal)
predict(modelal)
predict(modelal,newdata = newdata,interval = "confidence",level = 0.95)
comp<-as.numeric(rownames(modelal@frame))
newdata<-dfn[comp, !colnames(dfn) %in% c("id")]
dim(newdata)
newdata <- newdata %>% 
  mutate(fit.m = predict(modelal, re.form = NA),
         fit.c = predict(modelal, re.form = NULL))
head(newdata)
newdata %>%
  ggplot(aes(x = MICF_mean  , y = rWC )) +
  geom_point(pch = 16, col = "grey") +
  geom_line(aes(y = fit.m), col = 1, size = 2) +
  coord_cartesian(ylim = c(0, 100))

newdata %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = psy)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = psy), size = 2) +
  facet_wrap(vars(psy)) +
  coord_cartesian(ylim = c(0, 100))

# Mixed effects Model A - unconditional means
# Is there variability in rWC achievement across psy and  diagnosis?
modelal = lmer(rWC ~ 1  +(1|psy)+ (1|F0XXX)+(1|F1XXX)+(1|F2XXX)+(1|F3XXX)+(1|F4XXX)+(1|F5XXX)+(1|F6XXX)+(1|F7XX)+(1|F8XXX)+(1|F9XX), #+ (1|F3) + (1|F3XX) + (1|F3XXX) 
               REML=T, data=dfn)
summary(modelal)
tab_model(modelal)
# This intercept represents the average rWC achievement score across diagnosis/psy. 
# The variance component, tells us how much variance there is between diagnosis in rWC.


# That is, does the slope for MICF_mean predicting rWC vary from diagnosis to diagnosis? 
# For that, we want to allow the slopes to vary across the different schools. 
# In our model, we would want to estimate a random intercept and slope for the minority predictor, 
# which will give us an estimate of the variance component associated with the minority predictor (the minority slope). 
# For that test, we will want to run the following model:
modelal = lmer(rWC ~ MICF_mean  +(1|psy)+ (1|F0XXX)+(1|F1XXX)+(1|F2XXX)+(1|F3XXX)+(1|F4XXX)+(1|F5XXX)+(1|F6XXX)+(1|F7XX)+(1|F8XXX)+(1|F9XX),
               REML=T, data=dfn)
summary(modelal)
tab_model(modelal)
tab_model(modelal, file="rWC_MICF_random_effects.html")
# then take this html file and make .png file
webshot("rWC_MICF_random_effects.html", "rWC_MICF_random_effects.png")
#marginal R^2 = 0.314 conditional R^2= 0.733
# ICC	0.61

### plot model
comp<-as.numeric(rownames(modelal@frame))
newdata<-dfn[comp, !colnames(dfn) %in% c("id")]
dim(newdata)
newdata <- newdata %>% 
  mutate(fit.m = predict(modelal, re.form = NA),
         fit.c = predict(modelal, re.form = NULL))
head(newdata)
newdata <- newdata %>%
  mutate(resid = resid(modelal))

for (f in names(ranef(modelal))){
  newdata$Diagnosis<-newdata[,f] 
  p<-ggplot(newdata,aes(x = MICF_mean, y = rWC, col = Diagnosis )) +
    geom_point(pch = 16) +
    geom_line(aes(y = fit.c, col = Diagnosis), size = 2) +
    facet_wrap(vars(Diagnosis )) +
    coord_cartesian(ylim = c(0, 100))+
    labs(title =gsub("XXX|XX", "", f))
  print(p)


  ggsave(filename=paste0( f, ".tiff"),
         plot=p,
         device="tiff",
         path=file.path(path,"Figures/MixedEffectsReg"),
         width = 10,
         height=10,
         units="in")
  
  # p2<-ggplot(newdata,aes(x = MICF_mean, y = fit.m + resid)) +
  #   geom_line(aes(y = fit.c, col = Diagnosis), size = 1) +
  #   geom_point(pch = 16, col = "grey") +
  #   geom_line(aes(y = fit.m), col = 1, size = 2) +
  #   coord_cartesian(ylim = c(0, 100))+
  #   coord_cartesian(xlim = c(0, 4))+
  #   labs(title =gsub("XXX|XX", "", f))
  # print(p2)
  # ggsave(filename=paste0(f,"_Marginal_with_random_intercepts_on_the_conditional_residuals.tiff"),
  #        plot=p2,
  #        device="tiff", 
  #        path=file.path(path,"Figures/MixedEffectsReg"), 
  #        width = 10,
  #        height=10,
  #        units="in")
  
}


newdata<-cbind.data.frame(MICF_mean=c(0:4))
newdata[,  names(ranef(modelal))]<- "none"
newdata$psy<-"H"

head(newdata)
(fit.m<-predict(modelal,newdata = newdata,re.form = NA,allow.new.levels=T, se.fit=T))
(fit.c<-predict(modelal,newdata = newdata,re.form = NULL, allow.new.levels=T))
### need to boot CI 
boot_ci <- bootMer(modelal,
                   nsim = 1000,
                   FUN = function(x) { predict(x, newdata = newdata, re.form = NULL, allow.new.levels=T) })

boot_ci
dim(boot_ci$t)
(q<-apply ( boot_ci$t, 2, quantile , probs=c(0.025,0.975)))

# Predict with 95% prediction intervals (includes random effects by default)
(fit.c<-preds <- predictInterval( modelal,  newdata = newdata,  level = 0.95,  n.sims = 100,  stat = "mean",  type = "linear.prediction"   ))       # could also be "median"  # or "response" 

