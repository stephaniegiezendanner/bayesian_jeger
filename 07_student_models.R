# Load or tfit model nested with random intercepts and random slopes
# Ideally pre-tfit and loaded with `readRDS()` in real app
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
dfn<-dfn[complete.cases(dfn),]

### make it comparable to beta distribution:
dfn$y<-dfn$rWC/100
dfn$y[which(dfn$y==0)]<-0.0001
dfn$y[which(dfn$y==1)]<-0.9999
hist(dfn$y)


# https://mspeekenbrink.github.io/sdam-r-companion/bayesian-estimation-with-brms.html
mod_priors <- c(  prior(normal(1, 0.30), class = Intercept, lb = 0, ub=1), # intercept
                  # slopes
                  prior(student_t(3,-0.30, 0.20), class = b),
                  # nu= the degrees of freedom from the t-distribution
                  # prior(constant(2), class = "nu"),
                  prior(normal(2, 0.2), class = "nu" ,lb = 1),                  
                  
                  # # random effects
                  # prior(normal(0, 20), class = sd,lb = 0))
                  
                  # model error SD
                  prior(normal(0, 0.40), class = sigma, lb = 0))
               


## fixed effect for MICF_mean
tfit1 = brms::brm(
  y ~ MICF_mean ,  
  data  = dfn, 
  prior = mod_priors,
  family=student(), 
  chains = 4,
  iter =4000
  #,  cores = 4
)

summary(tfit1)
plot(tfit1)


pp_check(tfit1, ndraws=100)
pp_check(tfit1, ndraws=100, prefix='ppd')

(r1<-bayes_R2(tfit1, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.5156077 0.01364525 0.4874558 0.5407049

# leave one out
tfit1 <- add_criterion(tfit1, "loo")


## add another fixed effect for number of different main diagnosis
tfit2 <- update(tfit1, formula. = ~ . +age, newdata=dfn)
summary(tfit2)
(r2<-bayes_R2(tfit2, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.52634 0.01243645 0.4996718 0.5495339
tfit2 <- add_criterion(tfit2, "loo")

# compare both models
# widely applicable information criterion (WAIC) 
loo_compare(tfit1, tfit2, criterion = "loo")

## add a group-level effect with random intercept and slope
tfit3 <- update(tfit2, formula. = ~ . +(1|psy), newdata=dfn,
               prior = prior(normal(0, 0.20), class = sd, lb = 0))
summary(tfit3)
# widely applicable information criterion (WAIC) 
tfit3 <- add_criterion(tfit3, "loo")
loo_compare(tfit1, tfit2,tfit3, criterion = "loo")

(r3<-bayes_R2(tfit3, robust=T)) # Returns mean and CI for Bayesian R²)
# R2  0.6035177 0.01063939 0.5813507 0.6228109
plot(tfit3)
pp_check(tfit3, ndraws=100)


## add a group-level effect with random intercept and slope
tfit4 <- update(tfit3, formula. = ~ . +  (1|F0XXX)+(1|F1XXX)+
                 (1|F2XXX)+(1|F3XXX)+
                 (1|F4XXX)+(1|F5XXX)+
                 (1|F6XXX)+(1|F7XX)+
                 (1|F8XXX)+(1|F9XX),
               newdata=dfn)
summary(tfit4)
tfit4 <- add_criterion(tfit4, "loo")
loo_compare(tfit1, tfit2,tfit3,tfit4, criterion = "loo")

(r4<-bayes_R2(tfit4, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6302324 0.01043672 0.6087596 0.6498997
plot(tfit4)
pp_check(tfit4, ndraws=100)



### add single items:
## add a group-level effect with random intercept and slope
tfit5 <- update(tfit4, formula. = ~ . +
                 Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
                 fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
                 Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
                 Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
                 Mobilitat,
               newdata=dfn)
summary(tfit5)
tfit5 <- add_criterion(tfit5, "loo")

loo_compare(tfit1, tfit2,tfit3,tfit4,tfit5, criterion = "loo")
(r5<-bayes_R2(tfit5, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6600374 0.009274383 0.6400053 0.6769515
plot(tfit5)
pp_check(tfit5, ndraws=100)


### add some random slopes
## add a group-level effect with random intercept and slope
tfit6 <- update(tfit5, formula. = ~ .  -(1|psy) -(1|F0XXX)-(1|F1XXX)-
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
summary(tfit6)

tfit6 <- add_criterion(tfit6, "loo",save_psis = TRUE)

loo_compare(tfit1, tfit2,tfit3,tfit4,tfit5,tfit6, criterion = "loo")
# Model Ranking
# The top model (with elpd_diff = 0) is best supported by the data.
# Other models are worse by the elpd_diff amount.
# Practical Significance
# If the elpd_diff is larger than 2–4 times the se_diff, it suggests a meaningful difference between models.
# Sign of elpd_diff
# Negative elpd_diff means the model is worse than the top model.
# Positive (only if you reverse the comparison) means better.

(r6<-bayes_R2(tfit6, robust=T)) # Returns mean and CI for Bayesian R²)
# R2 0.6673857 0.008775741 0.6488953 0.6831664
plot(tfit6)
pp_check(tfit6, ndraws=100)

yrep <- posterior_predict(tfit6)
ppc_loo_pit_qq(tfit6$data$rWC, yrep, lw = weights(loo(tfit6)$psis_object))


tfit7 <- update(tfit6, formula. = ~ . -MICF_mean,
                newdata=dfn)
summary(tfit7)
tfit7 <- add_criterion(tfit7, "loo",save_psis = TRUE)
loo_compare(tfit1, tfit2,tfit3,tfit4,tfit5,tfit6,tfit7, criterion = "loo")


tfit8 <- update(tfit6, formula. = ~ . -Regeln.und.Routinen - Aufgabenplanung -Flexibilitat -
                  fachliche.Kompetenzen -Entscheidungsfahigkeit -Spontanaktivitaten -
                  Durchhaltefahigkeit -Selbstbehauptungsfahigkeit -Kontaktfahigkeit -
                  Gruppenfahigkeit -dyadische.Beziehungen -Selbstpflege -
                  Mobilitat,
                newdata=dfn)
summary(tfit8)
tfit8 <- add_criterion(tfit8, "loo")
loo_compare(tfit1, tfit2,tfit3,tfit4,tfit5,tfit6,tfit7,tfit8, criterion = "loo")

summary(tfit7)


# elpd_diff se_diff
# tfit7    0.0       0.0 
# tfit6    0.0       0.3 
# tfit5  -11.6       6.5 
# tfit8  -32.3      10.4 
# tfit4  -46.5      12.1 
# tfit3  -68.0      15.8 
# tfit2 -167.5      20.4 
# tfit1 -176.4      20.7 


# loo_compare(tfit6,tfit7,bfit6,bfit5, criterion = "loo")
# elpd_diff se_diff
# bfit6    0.0       0.0 
# bfit5   -9.8       8.4 
# tfit7 -310.8      51.7 
# tfit6 -310.9      51.8 

#################################################################################
###
### plot the model fit for a simple fit with MINI ICF-APP
###
group<-c("psy",c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX")))
for (i in group){
  dfn$group<-dfn[,i]
  
  newdata <- dfn %>%
    dplyr::group_by(group ) %>%
    tidyr::expand(MICF_mean = seq(0, 3, length.out = 100)) %>%
    ungroup()
  head(newdata)
  newdata$age<-0
  if(i!="psy"){
    newdata$psy<-"H"
  }
  
  newdata[,c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX"))]<-"none"
  newdata[, i]<-NULL
  colnames(newdata)[1]<-i
  
  newdata[,c("Regeln.und.Routinen","Aufgabenplanung","Flexibilitat",
             "fachliche.Kompetenzen","Entscheidungsfahigkeit","Spontanaktivitaten",
             "Durchhaltefahigkeit","Selbstbehauptungsfahigkeit","Kontaktfahigkeit",
             "Gruppenfahigkeit","dyadische.Beziehungen","Selbstpflege","Mobilitat")]<-0
  
  head(newdata)
  
  
  newdata_preds <- fitted(tfit4, newdata = newdata, re_formula = NULL,
                          allow_new_levels=T,probs = c(0.05,0.25,0.5,0.75, 0.95))
  # newdata_preds <- predict(tfit4, newdata = newdata, re_formula = NULL,
  #                         allow_new_levels=T,probs = c(0.05,0.25,0.5,0.75, 0.95))

  head(newdata_preds)
  
  
  newdata_preds[which(newdata_preds<0)]<-0
  newdata_preds[which(newdata_preds>1)]<-1
  
  newdata_preds<-newdata_preds*100
  
  newdata <- cbind(newdata, newdata_preds)
  head(newdata)
  
  hist(newdata$Estimate)
  
  if (length(grep("F",i))>0){
    title<-substr(i,1,2)
  } else{
    title<-i
  }
  ### plot 
  p1<-ggplot(newdata, aes(x = MICF_mean, y = Q50)) +
    geom_line(aes(color = .data[[i]])) +
    geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = .data[[i]]), alpha = 0.2) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = .data[[i]]), alpha = 0.5) +
    labs(y = "Predicted rWC", title = paste("Posterior Predictive Lines by", title),
         subtitle = "50% and 90% credible intervals as ribbons") +
    #theme_minimal() +
    ylim(c(0,100))+
    facet_wrap(vars(.data[[i]]))+
    scale_x_continuous(breaks = seq(0, 3, by = 1))+
  theme(panel.grid.major.x = element_line(color = "lightgrey",
                                         size = 0.25,linetype = 2))
  print(p1)
  
  p2<-ggplot(newdata, aes(x = MICF_mean, y = Q50)) +
    geom_line(aes(color = .data[[i]])) +
    #geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = .data[[i]]), alpha = 0.2) +
    labs(y = "Predicted rWC", title = paste("Posterior Predictive Lines by", title)) +
    #theme_minimal() +
    ylim(c(0,100))+
    #facet_wrap(vars(.data[[i]]))+
    scale_x_continuous(breaks = seq(0, 3, by = 1))+
    theme(panel.grid.major.x = element_line(color = "lightgrey",
                                            size = 0.25,linetype = 2))
 print(p2)
 p3<-ggarrange(p1,p2,ncol = 2)
 print(p3)
 ggsave(filename = file.path("Figures/Bayesian/Student/", paste0("Posterior_predictions_",i,".tiff" )),
        plot=p3,device="tiff", width=20, height=10, units="in", dpi=300)
}

#### plot fit with subdomains
### plot the model fit
group<-c("psy",c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX")))
domain<-c("Regeln.und.Routinen",  "Aufgabenplanung","Flexibilitat",
          "fachliche.Kompetenzen","Entscheidungsfahigkeit","Spontanaktivitaten",
          "Durchhaltefahigkeit","Selbstbehauptungsfahigkeit","Kontaktfahigkeit",
          "Gruppenfahigkeit","dyadische.Beziehungen","Selbstpflege","Mobilitat")

for (i in group){
  p2<-list()
  for (j in domain){
  dfn$group<-dfn[,i]
  
  newdata <- dfn %>%
    dplyr::group_by(group ) %>%
    tidyr::expand(dom = seq(0, 4,1)) %>%
    ungroup()

  newdata$age<-0
  if(i!="psy"){
    newdata$psy<-"H"
  }
  
  newdata[,c(paste0("F",c(0:6,8), "XXX"),paste0("F",c(7,9), "XX"))]<-"none"
  newdata[, i]<-NULL
  
  colnames(newdata)[1]<-i
  colnames(newdata)[2]<-j
  newdata[, domain[!domain %in% j] ]<-0
  newdata$MICF_mean<- rowMeans(newdata[,domain])
  newdata_preds <- fitted(tfit6, newdata = newdata, re_formula = NULL,
                          allow_new_levels=T,probs = c(0.05,0.25,0.5,0.75, 0.95))
  # newdata_preds <- predict(tfit4, newdata = newdata, re_formula = NULL,
  #                         allow_new_levels=T,probs = c(0.05,0.25,0.5,0.75, 0.95))
  
  newdata_preds[which(newdata_preds<0)]<-0
  newdata_preds[which(newdata_preds>1)]<-1
  
  newdata_preds<-newdata_preds*100
  newdata <- cbind(newdata, newdata_preds)
  if (length(grep("F",i))>0){
    title<-substr(i,1,2)
  } else{
    title<-i
  }
  ### plot 
  # p1[[j]]<-ggplot(newdata, aes(x = .data[[j]], y = Q50)) +
  #   geom_line(aes(color = .data[[i]])) +
  #   geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = .data[[i]]), alpha = 0.2) +
  #   geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = .data[[i]]), alpha = 0.5) +
  #   labs(y = "Predicted rWC", title = paste("Posterior Predictive Lines by", title),
  #        subtitle = "50% and 90% credible intervals as ribbons") +
  #   #theme_minimal() +
  #   ylim(c(0,100))+
  #   facet_wrap(vars(.data[[i]]))+
  #   scale_x_continuous(breaks = seq(0, 3, by = 1))+
  #   theme(panel.grid.major.x = element_line(color = "lightgrey",
  #                                           size = 0.25,linetype = 2))
  # print(p1 [[j]])
  
  p2[[j]]<-ggplot(newdata, aes(x = .data[[j]], y = Q50)) +
    geom_line(aes(color = .data[[i]]),size=1.5) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = .data[[i]]), alpha = 0.2) +
    #geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = .data[[i]]), alpha = 0.2) +
    labs(y = "Predicted rWC", title = paste("Posterior Predictive Lines by", title),
         subtitle = "50% credible intervals as ribbons") +
    #theme_minimal() +
    ylim(c(0,100))+
    #facet_wrap(vars(.data[[i]]))+
    #scale_x_continuous(breaks = seq(0, 3, by = 1))+
    theme(panel.grid.major.x = element_line(color = "lightgrey",
                                            size = 0.25,linetype = 2))
    
  print(p2[[j]])
  }
 
  p3<-ggarrange( plotlist =p2,common.legend = T)
  print(p3)
  
  ggsave(filename = file.path("Figures/Bayesian/Student/", paste0("Posterior_predictions_by_domain_and_",i,".tiff" )),
         plot=p3,device="tiff", width=30, height=30, units="in", dpi=300,bg = "white")
}

