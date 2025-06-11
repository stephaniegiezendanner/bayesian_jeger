# source("00_prepare_data.R")
source("01_prepare_libraries.R")

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean_f_diag.csv"),sep=";", 
             fileEncoding = "UTF-16LE")
df$rWC<-df$AF.angepasst
table(df$F3,df$F3XX)
table(df$F3,df$F3XXX)


dfn<-cbind.data.frame(id=df$id,
                      sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      age=scale(df$Alter,T, T),
                      psy=df$Arzt,
                      # MICF_mean=df$Gesamtpunktzahl/13,
                      MICF_mean=df$MICF_mean,
                      #MICF_mean_cat=cut(df$Gesamtpunktzahl/13,breaks=c(0:4), include.lowest = T, right = F),
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))],
                      rWC=df$rWC, 
                      df[,paste0("F",0:9 )])

head(dfn)

# https://www.azandisresearch.com/2022/12/31/visualize-mixed-effect-regressions-in-r-with-ggplot2/

fit<-lm ( rWC~MICF_mean , data=df)
summary(fit)
pp.pi<-predict(fit,interval="prediction")
colnames(pp.pi)<-paste0(colnames(pp.pi),".PI")

pp.ci<-predict(fit,interval="confidence")
colnames(pp.ci)<-paste0(colnames(pp.ci),".CI")

df_model<-cbind.data.frame(fit$model, pp.pi, pp.ci)
head(df_model)

p<-ggplot(df_model, aes(x = MICF_mean, y = rWC)) +
  geom_point()+
  geom_ribbon(aes(ymin=lwr.CI,ymax=upr.CI, colour="95% CI"),alpha=0.5)+
  geom_line(aes(y=fit.PI, x=MICF_mean, colour="fit"),linewidth=1.5)+
  #geom_smooth(method="lm") +
  ylim(c(-60,140))+
  ylab("resid work capacity [%]") + 
  #xlab("averaged MINI-ICF-APP")+ 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))+ 
  scale_fill_manual(name='', values=c(  "grey", "red"))+
  scale_colour_manual(name='', values=c("grey", "red"))
 

p
p1<-p+geom_ribbon(aes(ymin=lwr.PI,ymax=upr.PI, colour="95% PI"),alpha=0.3)+
  scale_fill_manual(name='', values=c(  "grey","lightgrey", "red"))+
  scale_colour_manual(name='', values=c("grey","lightgrey", "red"))

print(p1)  

ggsave(filename="rWC_MICF_CI.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 5,
       height=4,
       units="in")
ggsave(filename="rWC_MICF_PI.tiff",
       plot=p1,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 5,
       height=4,
       units="in")
#######################################################################################################################################
### make long format with one column for F diagnoses
F_diag<-paste0("F",0:9 )
data_long <- gather(dfn, condition, Diagnosis, F_diag, factor_key=TRUE)
head(data_long)

# long <- melt(setDT(dfn), id.vars = c("id", "rWC","MICF_mean" ),measure.vars=c(paste0("F",0:9 )),
#              variable.name = "Diagnosis")
# head(long)
# long<-long[-which(long$value==0),]
# long$value<-NULL
# head(long)
# long<-long[order(long$id),]


### plot across first F diagnosis level
p<-ggplot(long, aes(x = MICF_mean, y = rWC, col = Diagnosis)) +
  geom_point() +
  facet_wrap(vars(Diagnosis))+
  geom_smooth(method="lm") +
  ylim(c(0,100))+labs(title = paste("Main F diagnoses"))
p
ggsave(filename=paste0("F_main.tiff"),
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 10,
       height=5,
       units="in")


### plot across second and third F diagnosis level
for (f in 0:9){
  # f<-3
  ### Diagnosis FXX
  f_sel<-colnames(df)[grepl(paste0("^F",f,".*"),colnames(df)) & nchar(colnames(df))==3]
  if(length(f_sel)>=1){
    long <- melt(setDT(df), id.vars = c("id", "rWC","MICF_mean" ),
                 measure.vars=f_sel,
                 variable.name = "Diagnosis")
    head(long)
    long<-long[-which(long$value==0),]
    long$value<-NULL
    long<-long[order(long$id),]
    head(long)
    table(long$Diagnosis)
    
    p_m<-ggplot(long, aes(x = MICF_mean, y = rWC, col =  Diagnosis)) +
      geom_point() +
      facet_wrap(vars( Diagnosis))+
      geom_smooth(method="lm") +
      ylim(c(0,100))+labs(title = paste0("F",f, " main diagnoses"))
    p_m
    ### Diagnosis FXXX
    f_sel<-colnames(df)[grepl(paste0("^F",f,".*"),colnames(df)) & nchar(colnames(df))==4]
    f_sel<-grep("XX", f_sel,invert = T,value = T)
    if(length(f_sel)>=1){
    long <- melt(setDT(df), id.vars = c("id", "rWC","MICF_mean" ),
                 measure.vars=f_sel,
                 variable.name = "Diagnosis")
    head(long)
    long<-long[-which(long$value==0),]
    long$value<-NULL
    long$Diagnoses<-substring(long$Diagnosis,1,3)
    long<-long[order(long$id),]
    head(long)
    table(long$Diagnosis)
    table(long$Diagnoses)
    
    p_s<-ggplot(long, aes(x = MICF_mean, y = rWC, col =  Diagnoses)) +
      geom_point() +
      facet_wrap(vars( Diagnosis))+
      geom_smooth(method="lm") +
      ylim(c(0,100))+labs(title = paste0("F",f, " sub diagnoses"))
    p_s
    p<-ggarrange(p_m, p_s)
    }else{
      p<-ggarrange(p_m) 
    }
    ggsave(filename=paste0("F_", f, ".tiff"),
           plot=p,
           device="tiff", 
           path=file.path(path,"Figures"), 
           width = 10,
           height=5,
           units="in")
  
  }
}

############### mini_icf
icf <- colnames(df)[grep("Regeln.und.Routinen", colnames(df)):
          grep("Mobilitat", colnames(df))]
icf
long <- melt(setDT(df), id.vars = c("id", "rWC","MICF_mean" ),
             measure.vars=icf,
             variable.name = "ICF")
long<-long[order(long$id),]
long$value<-factor(ceiling(long$value))
table(long$value)

head(long)
for (f in icf){

  ### Points in ICF subgroup
   head(long)
    p_m<-ggplot(filter(long, ICF == f),  
                aes(x = MICF_mean, y = rWC, col =  value)) +
      geom_point() +
      facet_wrap(vars( value))+
      geom_smooth(method="lm") +
      ylim(c(0,100))+ labs(title = paste("ICF:",f))
    p_m
    
    ggsave(filename=paste0("ICF_", f, ".tiff"),
           plot=p_m,
           device="tiff", 
           path=file.path(path,"Figures"), 
           width = 10,
           height=5,
           units="in")
    
}

###################################################################################
### e.g. mixed-effects model taking F3XXX into account
# Mixed effects Model
### add other FXX FXXX
dfn<-cbind.data.frame(dfn,df[,grep("F*.XXX",colnames(df),value=T )])
dfn$F7XX<-df$F7XX
dfn$F9XX<-df$F9XX
head(dfn)

modelal = lmer(rWC ~ MICF_mean +(MICF_mean|F3XXX) , #+ (1|F3) + (1|F3XX) + (1|F3XXX) 
               REML=T, data=dfn)
summary(modelal)
#  93.54 the mean rWC across all MICF_mean and psy:
#  369.45 the variance within patients
#  70.99 the variance between psy
# only 19.2% of total variability is due to difference between psychiatrists.
tab_model(modelal)
ranef(modelal)
predict(modelal)

comp<-as.numeric(rownames(modelal@frame))
newdata<-dfn[comp, !colnames(dfn) %in% c("id")]
dim(newdata)
newdata <- newdata %>% 
  mutate(fit.m = predict(modelal, re.form = NA),
         fit.c = predict(modelal, re.form = NULL))
head(newdata)

p<-newdata  %>% #which(newdata$F3XXX=="F320"|newdata$F3XXX=="F321"|newdata$F3XXX=="F322"),
  ggplot(aes(x = MICF_mean, y = rWC, col = F3XXX)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = F3XXX), size = 2) +
  facet_wrap(vars(F3XXX)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title=paste("F3 diagnosis: N =", nrow(newdata)))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F3XXX_facet_wrap.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 7,
       height=4,
       units="in")
p<-newdata[which(newdata$F3XXX=="F320"|newdata$F3XXX=="F321"|newdata$F3XXX=="F322"),] %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = F3XXX)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = F3XXX), size = 2) +
  #facet_wrap(vars(F3XXX)) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_line(aes(y = fit.m ),col = "black", size = 2)+
labs(title=paste("F3 diagnosis: N =", nrow(newdata)))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F3XXX_together.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 6,
       height=4,
       units="in")
###################################################################################
### e.g. mixed-effects model taking F3 into account
# Mixed effects Model
### add other FXX FXXX
dfn<-cbind.data.frame(dfn,df[,grep("F*.XXX",colnames(df),value=T )])
dfn$F7XX<-df$F7XX
dfn$F9XX<-df$F9XX
dfn$F3XX<-df$F3XX
dfn$F6XX<-df$F6XX

head(dfn)

dfn$F6XX<- as.factor(dfn$F6XX)

modelal = lmer(rWC ~ MICF_mean +(MICF_mean|F6XX) , #+ (1|F3) + (1|F6XXX) + (1|F6XXXX) 
               REML=T, data=dfn)
summary(modelal)
#  93.54 the mean rWC across all MICF_mean and psy:
#  369.45 the variance within patients
#  70.99 the variance between psy
# only 19.2% of total variability is due to difference between psychiatrists.
tab_model(modelal)
ranef(modelal)
predict(modelal)

comp<-as.numeric(rownames(modelal@frame))
newdata<-dfn[comp, !colnames(dfn) %in% c("id")]
dim(newdata)
newdata <- newdata %>% 
  mutate(fit.m = predict(modelal, re.form = NA),
         fit.c = predict(modelal, re.form = NULL))
head(newdata)

p<-newdata %>%
  ggplot(aes(x = MICF_mean  , y = rWC )) +
  geom_point(pch = 16,col="grey") +
  geom_line(aes(y = fit.m), col = 1, size = 2) +
  coord_cartesian(ylim = c(0, 100))+
  labs(title=paste("F3 diagnosis: N =", nrow(newdata)))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
p

ggsave(filename="rWC_MICF_in_F6XXX.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 5,
       height=4,
       units="in")

p<-newdata  %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = F6XX)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = F6XX), size = 2) +
  facet_wrap(vars(F6XX)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title=paste("F3 diagnosis: N =", nrow(newdata)))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F6XXXX_facet_wrap.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 7,
       height=4,
       units="in")
p<-newdata %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = F6XX)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = F6XX), size = 2) +
  #facet_wrap(vars(F6XXXX)) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_line(aes(y = fit.m ),col = "black", size = 2)+
  labs(title=paste("F3 diagnosis: N =", nrow(newdata)))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F6XX_together.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 6,
       height=4,
       units="in")

# Mixed effects Model A - unconditional means
# Is there variability in rWC achievement across psy and  diagnosis?
#######################################################################################################################
### make long format with one column for F diagnoses
dfn<-cbind.data.frame(id=df$id,
                      sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      age=scale(df$Alter,T, T),
                      psy=df$Arzt,
                      # MICF_mean=df$Gesamtpunktzahl/13,
                      MICF_mean=df$MICF_mean,
                      #MICF_mean_cat=cut(df$Gesamtpunktzahl/13,breaks=c(0:4), include.lowest = T, right = F),
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))],
                      rWC=df$rWC, 
                      df[,c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX")) ])
head(dfn)
F_diag<-c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX")) 
df_long <- gather(dfn, condition, Diagnosis, F_diag, factor_key=TRUE)
head(df_long)
df_long<-df_long[-which(df_long$Diagnosis=="none"),]
df_long<-df_long[order(df_long$id),]
head(df_long)
table(df_long$condition)

modelal = lmer(rWC ~ MICF_mean +(MICF_mean|condition/Diagnosis) , #+ (1|F3) + (1|F3XX) + (1|F3XXX) 
               REML=T, data=df_long)
summary(modelal)
#  93.54 the mean rWC across all MICF_mean and psy:
#  369.45 the variance within patients
#  70.99 the variance between psy
# only 19.2% of total variability is due to difference between psychiatrists.
tab_model(modelal)
ranef(modelal)

#comp<-as.numeric(rownames(modelal@frame))

newdata<-modelal@frame[, !colnames(modelal@frame) %in% c("id")]
dim(newdata)
head(newdata)
newdata <- newdata %>% 
  mutate(fit.m = predict(modelal, re.form = NA),
         fit.c = predict(modelal, re.form = NULL))
head(newdata)
newdata$Diagnosis<-as.factor(newdata$Diagnosis)
table(newdata$Diagnosis)

p<-newdata  %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = condition)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = condition), size = 2) +
  facet_wrap(vars(Diagnosis)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title=paste("Different F diagnosis"))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F_facet_wrap.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 10,
       height=10,
       units="in")
p<-newdata %>%
  ggplot(aes(x = MICF_mean, y = rWC, col = Diagnosis)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.c, col = Diagnosis), size = 2) +
  #facet_wrap(vars(condition)) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_line(aes(y = fit.m ),col = "black", size = 2)+
  labs(title=paste("Different F diagnosis"))+
  ylab("Resid. work capacity [%]") + 
  scale_x_continuous(breaks=0:4,
                     name="Impairment (averaged MINI-ICF-APP)",
                     label = c("no", 
                               "mild",
                               "moderate",
                               "severe",
                               "complete"))
print(p)
ggsave(filename="rWC_MICF_in_F_together.tiff",
       plot=p,
       device="tiff", 
       path=file.path(path,"Figures"), 
       width = 10,
       height=6,
       units="in")
