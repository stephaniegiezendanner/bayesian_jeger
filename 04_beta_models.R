# Load or bfit model nested with random intercepts and random slopes
# Ideally pre-bfit and loaded with `readRDS()` in real app
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
table(dfn$n_diag)

### beta distribution:
dfn$y<-dfn$rWC/100
dfn$y[which(dfn$y==0)]<-0.0001
dfn$y[which(dfn$y==1)]<-0.9999
hist(dfn$y)



# https://mspeekenbrink.github.io/sdam-r-companion/bayesian-estimation-with-brms.html
mod_priors <- c(   prior(student_t(3,3, 1), class = Intercept), # intercept
                   ## slopes
                   prior(student_t(3,-1, 1), class = b, coef = "MICF_mean"))



## fixed effect for MICF_mean
bfit1 = brms::brm(
  y ~ MICF_mean ,  
  data  = dfn, 
  prior = mod_priors,
  #sample_prior = "only",
  family=Beta(), 
  chains = 4,
  iter =4000
  #,  cores = 4
)
summary(bfit1)
plogis(fixef(bfit1))*100
plot(bfit1)
(r1<-bayes_R2(bfit1, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.5248984 0.01140006 0.4999811 0.5455464

# widely applicable information criterion (WAIC) 
bfit1 <- add_criterion(bfit1, "waic")
bfit1 <- add_criterion(bfit1, "loo")
loo_compare(fit1, bfit1, criterion = "loo")


## add another fixed effect for number of different main diagnosis
bfit2 <- update(bfit1, formula. = ~ . +n_diag, newdata=dfn)
summary(bfit2)
(r2<-bayes_R2(bfit2, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.526084 0.01163902 0.5013509 0.5463428
bfit2 <- add_criterion(bfit2, "waic")
bfit2 <- add_criterion(bfit2, "loo")

loo_compare(bfit2, bfit1, criterion = "loo")


# compare both models
# widely applicable information criterion (WAIC) 
# loo_compare(bfit1, bfit2, criterion = "waic")
loo_compare(bfit1, bfit2, criterion = "loo")

## add a group-level effect with random intercept and slope
bfit3 <- update(bfit2, formula. = ~ . +(1|psy), newdata=dfn,
               prior = prior(normal(0, 20), class = sd, lb = 0))
summary(bfit3)
# widely applicable information criterion (WAIC) 
# bfit3 <- add_criterion(bfit3, "waic")
bfit3 <- add_criterion(bfit3, "loo")

loo_compare(bfit1, bfit2,bfit3, criterion = "loo")
(r3<-bayes_R2(bfit3, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.5931352 0.01068921 0.5703062 0.6119766
plot(bfit3)
pp_check(bfit3, ndraws=100)


## add a group-level effect with random intercept and slope
bfit4 <- update(bfit3, formula. = ~ . +  (1|F0XXX)+(1|F1XXX)+
                 (1|F2XXX)+(1|F3XXX)+
                 (1|F4XXX)+(1|F5XXX)+
                 (1|F6XXX)+(1|F7XX)+
                 (1|F8XXX)+(1|F9XX),
               newdata=dfn)
summary(bfit4)
# widely applicable information criterion (WAIC) 
# bfit4 <- add_criterion(bfit4, "waic")
bfit4 <- add_criterion(bfit4, "loo")
# loo_compare(bfit1, bfit2,bfit3,bfit4, criterion = "waic")
loo_compare(bfit1, bfit2,bfit3,bfit4, criterion = "loo")

(r4<-bayes_R2(bfit4, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6319064 0.01004782 0.6108332 0.6501864
plot(bfit4)
pp_check(bfit4, ndraws=100)



### add single items:
## add a group-level effect with random intercept and slope
bfit5 <- update(bfit4, formula. = ~ .-MICF_mean +
                 Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
                 fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
                 Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
                 Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
                 Mobilitat,
               newdata=dfn)
summary(bfit5)
# widely applicable information criterion (WAIC) 
# bfit5 <- add_criterion(bfit5, "waic")
bfit5 <- add_criterion(bfit5, "loo")

(r5<-bayes_R2(bfit5, robust=T)) # Returns mean and CI for Bayesian R²)
# R2  0.6556947 0.009192507 0.637295 0.6727111 
plot(bfit5)
pp_check(bfit5, ndraws=100)


### add some random slopes
## add a group-level effect with random intercept and slope
bfit6 <- update(bfit5, formula. = ~ .  -(1|psy) -(1|F0XXX)-(1|F1XXX)-
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
summary(bfit6)
# widely applicable information criterion (WAIC) 
# bfit6 <- add_criterion(bfit6, "waic")
bfit6 <- add_criterion(bfit6, "loo",save_psis = TRUE)

loo_compare(bfit6,bfit5, criterion = "loo")
loo_compare(bfit3, bfit4, criterion = "loo")

# Model Ranking
# The top model (with elpd_diff = 0) is best supported by the data.
# Other models are worse by the elpd_diff amount.
# Practical Significance
# If the elpd_diff is larger than 2–4 times the se_diff, it suggests a meaningful difference between models.
# Sign of elpd_diff
# Negative elpd_diff means the model is worse than the top model.
# Positive (only if you reverse the comparison) means better.

(r6<-bayes_R2(bfit6, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6656763 0.009010808 0.6464091 0.6819507
plot(bfit6)
pp_check(bfit6, ndraws=100)
plogis(fixef(bfit6))*100
yrep <- posterior_predict(bfit6)
ppc_loo_pit_qq(bfit6$data$y, yrep, lw = weights(loo(bfit6)$psis_object))

dim(yrep)
plot(colMeans(yrep), bfit6$data$y)

