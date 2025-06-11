### start analysis
### question: can we predict the rWC by ICD-10? 
### question: can we use bayesian to get an idea of an rWC estimate?

# source("00_prepare_data.R")
source("01_prepare_libraries.R")
library(data.table)
library("bayesplot")
library("ggplot2")
library("rstanarm")
###############################################################################
### read data

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

df<-read.csv(file=file.path(path, "../Jeger_Data_clean.csv"),#sep="\t", 
             sep=";",fileEncoding = "UTF-16LE")
head(df) 
df$Sex_f<- ifelse(df$Sex=="F", 1,0)
table(df$main_ICD_sub)
df$main_ICD_sub<-as.factor(df$main_ICD_sub)
df<-within(df, main_ICD_sub<-relevel(main_ICD_sub, ref="F45"))
df$main_ICD<-as.factor(df$main_ICD)
df<-within(df, main_ICD<-relevel(main_ICD, ref="F4"))


table(df$main_ICD_sub)
table(df$main_ICD)

# ind<-which(df$main_ICD=="F3"|df$main_ICD=="F4")
# df$main_ICD[ind]<-df$main_ICD_sub[ind]
# table(df$main_ICD)
# ##################################################################################
# ### hot encoding of main ICD
# # dummify the data
# dmy_main_ICD <- dummyVars(" AF.angepasst~ main_ICD", data = df,sep = "_", drop2nd=T)
# 
# trsf_main_ICD <- data.frame(predict(dmy_main_ICD, newdata = df))
# head(trsf_main_ICD)

dfn<-cbind.data.frame(main_ICD=df$main_ICD,
                      main_ICD_sub=df$main_ICD_sub,#trsf_main_ICD, 
                      fem_sex=df$Sex_f, 
                      age=scale(df$Alter,T,T),
                      total=scale(df$Gesamtpunktzahl, T,T),
                      total_cat=cut(df$Gesamtpunktzahl, 6),
                      scale(df[,grep("Regeln.und.Routinen", colnames(df)):
                                 grep("Mobilitat", colnames(df))],T,T),
                      rWC=df$AF.angepasst)
#pairs(dfn)
summary(dfn)
table(dfn$main_ICD_sub)

write.table(dfn,file=file.path(path, "../Jeger_Data_regression_pred_centered.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )
#pairs(dfn)
head(dfn)


summary(head(dfn))
dfn<-dfn[complete.cases(dfn),]
dim(dfn)
################################################################################
### student 
# https://solomonkurz.netlify.app/blog/2019-02-10-bayesian-robust-correlations-with-brms-and-why-you-should-love-student-s-t/

# prior belief: 
mu<-30
mu_sd<-10
sigma<-20
sigma_sd<-10

brmfit<- brm(bf(rWC~1),data= dfn,
             
             prior = #c(#set_prior("student_t(1,-2,1)", class = "b"),
               c(prior(gamma(2, .1), class = nu),
                 prior(normal(mu, mu_sd), class = Intercept),### how confident about the peak
                 prior(normal(sigma, sigma_sd), class = sigma) ),
             
             # "iter" is the number of iterations
             # sample_prior = "only",
             family = student(),
             iter = 4000)

summary(brmfit)
brmfit$prior
prior_summary(brmfit)
posterior_summary(brmfit)
pp_check(brmfit, prefix = "ppd", ndraws = 100)
pp_check(brmfit, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit, type = "dens")
plot(brmfit)



## compute expected predictions
ppe<-posterior_epred(brmfit)
dim(ppe)

# fitted estimates
bfitted_values <- fitted(brmfit, summary=F)
head(bfitted_values[,1])

# predicted estimates
bpredict_values <- predict(brmfit,cores=1,summary=F)
head(bpredict_values[,1])

### adapt for no values <0 and >100
bpredict_values_cut<-ifelse(bpredict_values<0,0,bpredict_values)
bpredict_values_cut<-ifelse(bpredict_values_cut>100,100,bpredict_values_cut)


ppc_error_hist(y=brmfit$data$rWC, yrep=bpredict_values_cut[1:10,])
ppc_scatter(y=brmfit$data$rWC, yrep=bpredict_values_cut[1:10,])

data <- data.frame(x = rnorm(10,mu,mu_sd))
p<-ppc_dens_overlay(y=brmfit$data$rWC, yrep=bpredict_values_cut[1:100,])
p+  stat_function(fun = dnorm,
                args = list(mean = mean(data$x),
                            sd = sd(data$x)),
                col = "red",
                linewidth = 2) +
  annotate("text", x=mu, y=0.03, label= paste("Prior\nM=",mu, 
                                              "\nsd=",mu_sd))+
annotate("text", x=50, y=0.04, 
         label= paste("Posterior\nM=",round(summary(brmfit)$fixed$Estimate), 
                      "\nsd=",round(sumary_brm$spec_pars$Estimate[1])))


