
# source("00_prepare_data.R")
source("01_prepare_libraries.R")

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean.csv"),sep="\t", 
             fileEncoding = "UTF-16LE")
head(df) 
df$Sex_f<- ifelse(df$Sex=="F", 1,0)
table(df$main_ICD,df$main_ICD_sub)
ind<-which(df$main_ICD=="F3"|df$main_ICD=="F4")
df$main_ICD[ind]<-df$main_ICD_sub[ind]
table(df$main_ICD)
##################################################################################
### hot encoding of main ICD
# dummify the data
dmy_main_ICD <- dummyVars(" AF.angepasst~ main_ICD", data = df,sep = "_", drop2nd=T)

trsf_main_ICD <- data.frame(predict(dmy_main_ICD, newdata = df))
head(trsf_main_ICD)

dfn<-cbind.data.frame(trsf_main_ICD, fem_sex=df$Sex_f, 
                      age=scale(df$Alter,T,T),
                      total=scale(df$Gesamtpunktzahl, T,T),
                      total_cat=cut(df$Gesamtpunktzahl, 6),
                      scale(df[,grep("Regeln.und.Routinen", colnames(df)):
                                 grep("Mobilitat", colnames(df))],T,T),
                      rWC=df$AF.angepasst/100)
#pairs(dfn)
summary(dfn)
dfn$rWC<-ifelse(dfn$rWC==0,0.01,dfn$rWC)
dfn$rWC<-ifelse(dfn$rWC==1,0.99,dfn$rWC)

write.table(dfn,file=file.path(path, "../Jeger_Data_regression_pred_centered.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )
#pairs(dfn)
head(dfn)

### beta distribution
brmfit_b<- brm(bf(rWC~1),data= dfn,
             
              prior = c(#set_prior("student_t(1,-2,1)", class = "b"),
                set_prior("student_t(3, 0, 1)", class = "Intercept")),#,
                #set_prior("gamma(1, 20, 10)", class = "phi") ),
             # 
             # "iter" is the number of iterations
             # sample_prior = "only",
             family = Beta(),
             iter = 4000)

summary(brmfit_b)
brmfit_b$prior
prior_summary(brmfit_b)
pp_check(brmfit_b, prefix = "ppd", ndraws = 100)
pp_check(brmfit_b, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit_b, type = "dens")

# fitted estimates
bfitted_values <- fitted(brmfit_b, summary=F)
head(bfitted_values)
head(bfitted_values[,1])


### beta distribution
brmfit_b<- brm(bf(rWC~total),data= dfn,
               
               prior = c(#set_prior("student_t(1,-2,1)", class = "b"),
                 set_prior("student_t(3, 0, 1)", class = "Intercept")),#,
               #set_prior("gamma(1, 20, 10)", class = "phi") ),
               # 
               # "iter" is the number of iterations
               # sample_prior = "only",
               family = Beta(),
               iter = 4000)

summary(brmfit_b)
brmfit_b$prior
prior_summary(brmfit_b)
pp_check(brmfit_b, prefix = "ppd", ndraws = 100)
pp_check(brmfit_b, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit_b, type = "dens")
plot(brmfit_b)
