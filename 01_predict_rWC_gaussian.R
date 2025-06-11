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
                      rWC=df$AF.angepasst)
#pairs(dfn)
summary(dfn)

write.table(dfn,file=file.path(path, "../Jeger_Data_regression_pred_centered.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )
#pairs(dfn)
head(dfn)

summary(head(dfn))
dfn<-dfn[complete.cases(dfn),]

dfn$main_ICD_subZ7<-NULL

dfn %>%
  ggplot(aes(y = total_cat, x = rWC)) +
  geom_point()


##############################################################################
### linear frequentist models
### predictor: Gesamtpunktzahl
fit<-lm(AF.angepasst~Gesamtpunktzahl,df)
summary(fit)
confint(fit)
plot(df$AF.angepasst~df$Gesamtpunktzahl)
abline(fit)

### predictor:main_ICD
table(df$main_ICD)
fit<-lm(AF.angepasst~main_ICD,df)
summary(fit)
confint(fit)

############################################################################
### full freuquentist model 
fit<-lm(rWC~1,dfn)
summary(fit)

fit<-lm(rWC~total,dfn)
summary(fit)

new<-cbind.data.frame(predicted=predict(fit,newdata = dfn),
                      observed=dfn$rWC)
ggplot(new,aes(x=predicted,y=observed))+geom_point()+geom_smooth()+ 
  stat_poly_eq() +
  xlab("Predicted rWC")+ylab("Observed rWC")
###############################################################################
##############################################################################
### gaussian distribution
brmfit<- brm(bf(rWC~1),data= dfn,
               
              prior = c(#set_prior("student_t(1,-2,1)", class = "b"),
                        set_prior("student_t(3, 50, 20)", class = "Intercept"),
                        set_prior("student_t(1, 20, 10)", class = "sigma") ),

              # "iter" is the number of iterations
              # sample_prior = "only",
              family = gaussian(),
              iter = 4000)
          
summary(brmfit)
brmfit$prior
prior_summary(brmfit)
posterior_summary(brmfit)
pp_check(brmfit, prefix = "ppd", ndraws = 100)
pp_check(brmfit, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit, type = "dens")



## compute expected predictions
ppe<-posterior_epred(brmfit)
dim(ppe)

# fitted estimates
bfitted_values <- fitted(brmfit, summary=F)
head(bfitted_values)
head(bfitted_values[,1])

# predicted estimates
bpredict_values <- predict(brmfit,cores=1,summary=F)
head(bpredict_values[,1])
dim(bpredict_values)
hist(bpredict_values[,1])
bpredict_values_cut<-ifelse(bpredict_values<0,0,bpredict_values)
bpredict_values_cut<-ifelse(bpredict_values_cut>100,100,bpredict_values_cut)

hist(bpredict_values_cut[,2])



### with a spread around a categorical gesamtpunktzahl
### gaussian distribution
brmfit<- brm(bf(rWC~total),data= dfn,
             
             prior = c(set_prior("student_t(1,-20,10)", class = "b"),
               set_prior("student_t(3, 50, 20)", class = "Intercept"),
               set_prior("student_t(1, 20, 10)", class = "sigma") ),
             
             # "iter" is the number of iterations
             # sample_prior = "only",
             family = gaussian(),
             iter = 4000)

summary(brmfit)
brmfit$prior
prior_summary(brmfit)
posterior_summary(brmfit)

pp_check(brmfit, prefix = "ppd", ndraws = 100)
pp_check(brmfit, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit, type = "dens")

## predicted responses
pp <- predict(brmfit,robust=T, summary=T, 
              probs=c(0.025,0.33,0.5,0.66,0.975))
dim(pp)
head(pp)

### check if median of the predictions relates to outcome
plot(brmfit$data[,1]~pp[,1])
summary(lm(brmfit$data[,1]~pp[,1]))

### show how predictor affects outcome
newdata<-data.frame(total =seq(-2,2,1))
pred <- add_predicted_draws(newdata,brmfit)
head(pred)
aggregate(pred$.prediction,list(pred$total),quantile,c(0.1,0.5,0.9))


pred$total<-as.factor(pred$total)
# APPROACH 1: calculate mean value from predicted values
predM <- group_by(pred, total) %>% summarise(mP = mean(.prediction))
predM$total<-as.factor(predM$total)
aggregate(pred$.prediction,list(pred$total),mean)

ggplot(data = pred, aes(x = .prediction, col = total)) + geom_density() +
  geom_vline(data = predM, aes(xintercept = mP, col = total), linetype = "dashed") 
  #+ geom_vline(data = calcM, aes(xintercept = mC, col = total), linetype = "solid") 
  #+ facet_wrap(~total)


conditional_effects(brmfit)

################################################################################
### include more predictors:
### with a spread around a categorical gesamtpunktzahl
### gaussian distribution
brmfit_full<- brm(bf(rWC~.-total_cat -total),data= dfn,
             prior = c(set_prior("student_t(1,-10,20)", class = "b"),
                       set_prior("student_t(0.5, 50, 20)", class = "Intercept"),
                       set_prior("student_t(0.5, 20, 20)", class = "sigma") ),
             
             # "iter" is the number of iterations
             # sample_prior = "only",
             family = gaussian(),
             iter = 4000)

summary(brmfit_full)
brmfit_full$prior
prior_summary(brmfit_full)
posterior_summary(brmfit_full,probs = c(0.025,0.1,0.5,0.9,0.975))

pp_check(brmfit_full, prefix = "ppd", ndraws = 100)
pp_check(brmfit_full, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit_full, type = "dens")

## predicted responses
pp <- predict(brmfit_full,robust=T, summary=T, 
              probs=c(0.025,0.33,0.5,0.66,0.975))
dim(pp)
head(pp)

### check if median of the predictions relates to outcome
plot(brmfit_full$data[,1]~pp[,1])
summary(lm(brmfit_full$data[,1]~pp[,1]))

### show how predictor affects outcome
newdata<-dfn[1,]
newdata[1,]<-0
newdata$b_main_ICDF44 =1
newdata$age=2
newdata$b_Regeln.und.Routinen=-2
pred <- add_predicted_draws(newdata,brmfit_full)
head(pred)
dim(pred)
predM<-data.frame(q=quantile(pred$.prediction,c(0.05,0.5,0.95)))

ggplot(data = pred, aes(x = .prediction)) + geom_density() +
  geom_vline(data = predM, aes(xintercept = q), linetype = "dashed") 

aggregate(pred$.prediction,list(pred$total),quantile,c(0.1,0.5,0.9))



pred$total<-as.factor(pred$total)
# APPROACH 1: calculate mean value from predicted values
predM <- group_by(pred, total) %>% summarise(mP = mean(.prediction))
predM$total<-as.factor(predM$total)
aggregate(pred$.prediction,list(pred$total),mean)

ggplot(data = pred, aes(x = .prediction, col = total)) + geom_density() +
  geom_vline(data = predM, aes(xintercept = mP, col = total), linetype = "dashed") 
#+ geom_vline(data = calcM, aes(xintercept = mC, col = total), linetype = "solid") 
#+ facet_wrap(~total)


conditional_effects(brmfit_full)







#############################################################################
#### mixture
mu1<-10
mu2<-50
sigma1<-5
sigma2<-10
fit <- brm(
  bf(rWC~.),data= dfn,
  family = mixture(gaussian, gaussian),        # Two-component Gaussian mixture
  prior = c(
    prior(normal(50, 10), class="Intercept", dpar = mu1),  # Prior for mean of first Gaussian
    prior(normal(10, 10), class="Intercept", dpar = mu2),  # Prior for mean of second Gaussian
    prior("student_t(1,-2,3)", class = "b"),               # Priors for predictors
    prior("gamma(1, 1)", dpar = sigma1),            # Prior for sd of first Gaussian
    prior("gamma(2, 0.5)", dpar = sigma2),            # Prior for sd of second Gaussian
    prior(dirichlet(1, 1), class = "theta")         # Prior for mixing proportions
  ),
  chains = 4, iter = 2000, cores = 4
)

summary(brmfit)
prior_summary(brmfit)

tidybayes::summarise_draws(brmfit)
brms::rhat(brmfit) |> head()
get_variables(brmfit)

##############################################################################
## get predictions:
samples_post_pred_temperature <- brms::posterior_predict(brmfit)
posterior_long <- as.data.frame(samples_post_pred_temperature)
posterior_long<-t(posterior_long)
head(posterior_long[,1:10])
# Aggregate for plotting
posterior_summary <- posterior_long %>%
  pivot_longer(-observation, names_to = "sample", values_to = "prediction") %>%
  group_by(observation) %>%
  summarize(mean = mean(prediction), lower = quantile(prediction, 0.025), upper = quantile(prediction, 0.975))

posterior_summary<-cbind.data.frame(mean=apply(posterior_long,1, mean), 
                 lower=apply(posterior_long,1, quantile, 0.025),
                   upper=apply(posterior_long,1, quantile, 0.975))
posterior_summary$observed <- dfn$AF.angepasst[complete.cases(dfn)]
# Add observed data
#posterior_summary$observed <- dfn$AF.angepasst
posterior_summary$observation<-1:nrow(posterior_summary)

# Plot
ggplot(posterior_summary, aes(x = observation, y = mean)) +
  geom_point(aes(y = observed), color = "red") +
  geom_linerange(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(x = "Observation", y = "Prediction", title = "Posterior Predictions vs Observed Data") +
  theme_minimal()

ggplot(posterior_summary, aes(x = mean, y =observed )) +
  geom_point() 

bayesplot::pp_check(brmfit, ndraws = 30)
mcmc_plot(brmfit, variable =  get_variables(brmfit)[1:26] , type = "intervals") +
  geom_vline(xintercept = 0, color = "darkorange") # add line at 0  + xlim(-0.1, 0.7)
