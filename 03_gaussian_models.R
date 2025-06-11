### check different gaussian models 


# Ideally pre-fit and loaded with `readRDS()` in real app
library("shiny")
# install.packages("shinythemes")
library("shinythemes")
library(brms)

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
                      df[,c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX")) ],
                      df[,paste0("F",c(0:9))])
head(dfn)
dfn$n_diag<-rowSums(dfn[, paste0("F",c(0:9))], na.rm = T)

### make it comparable to beta distribution:
dfn$y<-dfn$rWC/100
dfn$y[which(dfn$y==0)]<-0.0001
dfn$y[which(dfn$y==1)]<-0.9999
hist(dfn$y)

# https://mspeekenbrink.github.io/sdam-r-companion/bayesian-estimation-with-brms.html
mod_priors <- c(  prior(normal(100, 30), class = Intercept, lb = 0, ub=100), # intercept
                  # slopes
                  prior(normal(-30, 30), class = b),
                  # random effects
                  prior(normal(0, 40), class = sigma,lb = 0)
                  # error SD
                  #prior(normal(0, 20), class = sd, lb = 0),
                  )


## fixed effect for MIodel
fit1 = brms::brm(
  y ~ MICF_mean ,  data  = dfn, 
  prior = mod_priors,
  family=gaussian(), 
  chains = 4,
  iter =4000
  #,  cores = 4
)
summary(fit1)
fit1 <- add_criterion(fit1, "waic")
fit1 <- add_criterion(fit1, "loo")

## add a group-level effect with random intercept
fit2 <- update(fit1, formula. = ~ . +(1|psy),
               prior = prior(normal(0, 20), class = sd, lb = 0))
summary(fit2)

## add a group-level effect with random intercept and slope
fit2 <- update(fit1, formula. = ~ . +(MICF_mean|psy),
               prior = prior(normal(0, 20), class = sd, lb = 0))
summary(fit2)
plogis(fixef(fit1))*100
plot(fit1)
(r1<-bayes_R2(fit1, robust=T)) # Returns mean and CI for Bayesian R²)
# R2  0.5064147 0.01422931 0.4767822 0.5324965

# widely applicable information criterion (WAIC) 
fit1 <- add_criterion(fit1, "waic")
fit1 <- add_criterion(fit1, "loo")


## add another fixed effect for number of different main diagnosis
fit2 <- update(fit1, formula. = ~ . +n_diag, newdata=dfn)
summary(fit2)
(r2<-bayes_R2(fit2, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.5172982 0.01332634 0.4900963 0.5417921
fit2 <- add_criterion(fit2, "waic")
fit2 <- add_criterion(fit2, "loo")

# compare both models
# widely applicable information criterion (WAIC) 
loo_compare(fit1, fit2, criterion = "waic")
loo_compare(fit1, fit2, criterion = "loo")

## add a group-level effect with random intercept and slope
fit3 <- update(fit2, formula. = ~ . +(1|psy), newdata=dfn,
                prior = prior(normal(0, 20), class = sd, lb = 0))
summary(fit3)
# widely applicable information criterion (WAIC) 
fit3 <- add_criterion(fit3, "waic")
fit3 <- add_criterion(fit3, "loo")

loo_compare(fit1, fit2,fit3, criterion = "waic")
(r3<-bayes_R2(fit3, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.592499 0.01119564 0.5690517 0.6125379
plot(fit3)
pp_check(fit3, ndraws=100)


## add a group-level effect with random intercept and slope
fit4 <- update(fit3, formula. = ~ . +  (1|F0XXX)+(1|F1XXX)+
                  (1|F2XXX)+(1|F3XXX)+
                  (1|F4XXX)+(1|F5XXX)+
                  (1|F6XXX)+(1|F7XX)+
                  (1|F8XXX)+(1|F9XX),
                newdata=dfn)
summary(fit4)
# widely applicable information criterion (WAIC) 
fit4 <- add_criterion(fit4, "waic")
fit4 <- add_criterion(fit4, "loo")
loo_compare(fit1, fit2,fit3,fit4, criterion = "waic")
loo_compare(fit1, fit2,fit3,fit4, criterion = "loo")

(r4<-bayes_R2(fit4, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6243801 0.01082707 0.60115 0.6441842
plot(fit4)
pp_check(fit4, ndraws=100)



### add single items:
## add a group-level effect with random intercept and slope
fit5 <- update(fit4, formula. = ~ .-MICF_mean +
                  Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
                  fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
                  Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
                  Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
                  Mobilitat,
                newdata=dfn)
summary(fit5)
# widely applicable information criterion (WAIC) 
fit5 <- add_criterion(fit5, "waic")
fit5 <- add_criterion(fit5, "loo")

loo_compare(fit1, fit2,fit3,fit4,fit5, criterion = "waic")
(r5<-bayes_R2(fit5, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6531508 0.009789274 0.6318401 0.6706634
plot(fit5)
pp_check(fit5, ndraws=100)


### add some random slopes
## add a group-level effect with random intercept and slope
fit6 <- update(fit5, formula. = ~ .  -(1|psy) -(1|F0XXX)-(1|F1XXX)-
                  (1|F2XXX)-(1|F3XXX)-
                  (1|F4XXX)-(1|F5XXX)-
                  (1|F6XXX)-(1|F7XX)-
                  (1|F8XXX)-(1|F9XX) +
                  (MICF_mean|psy)+
                  (MICF_mean|F0XXX)+(MICF_mean|F1XXX)+
                  (MICF_mean|F2XXX)+(MICF_mean|F3XXX)+
                  (MICF_mean|F4XXX)+(MICF_mean|F5XXX)+
                  (MICF_mean|F6XXX)+(MICF_mean|F7XX)+
                  (MICF_mean|F8XXX)+(MICF_mean|F9XX),
                newdata=dfn)
summary(fit6)
# widely applicable information criterion (WAIC) 
fit6 <- add_criterion(fit6, "waic")
fit6 <- add_criterion(fit6, "loo",save_psis = TRUE)

loo_compare(fit6,fit5, criterion = "loo")
# Model Ranking
# The top model (with elpd_diff = 0) is best supported by the data.
# Other models are worse by the elpd_diff amount.
# Practical Significance
# If the elpd_diff is larger than 2–4 times the se_diff, it suggests a meaningful difference between models.
# Sign of elpd_diff
# Negative elpd_diff means the model is worse than the top model.
# Positive (only if you reverse the comparison) means better.

(r6<-bayes_R2(fit6, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6629431 0.009293493 0.6435565 0.6800591
plot(fit6)
pp_check(fit6, ndraws=100)

yrep <- posterior_predict(fit6)
ppc_loo_pit_qq(fit6$data$y, yrep, lw = weights(loo(fit6)$psis_object))



+
  Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
  fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
  Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
  Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
  Mobilitat+
  (MICF_mean|F0/F0XXX)+(MICF_mean|F1/F1XXX)+
  (MICF_mean|F2/F2XXX)+(MICF_mean|F3/F3XXX)+
  (MICF_mean|F4/F4XXX)+(MICF_mean|F5/F5XXX)+
  (MICF_mean|F6/F6XXX)+(MICF_mean|F7/F7XX)+
  (MICF_mean|F8/F8XXX)+(MICF_mean|F9/F9XX)

