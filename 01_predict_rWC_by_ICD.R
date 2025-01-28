### start analysis
### question: can we predict the rWC by ICD-10? 
### question: can we use bayesian to get an idea of an rWC estimate?

# source("00_prepare_data.R")
library(ggplot2)
library(mltools)
library(data.table)
library(caret)
#install.packages("tidybayes")
library("tidybayes")
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(distributional)
library("bayesplot")

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean.csv"),sep="\t", fileEncoding = "UTF-16LE")
head(df) 
df$Sex_f<- ifelse(df$Sex=="F", 1,0)

##################################################################################
### hot encoding of main ICD
# dummify the data
dmy_main_ICD <- dummyVars(" AF.angepasst~ main_ICD_sub", data = df,sep = "_", drop2nd=T)

trsf_main_ICD <- data.frame(predict(dmy_main_ICD, newdata = df))
head(trsf_main_ICD)

dfn<-cbind.data.frame(trsf_main_ICD, fem_sex=df$Sex_f, 
                      age_per_10=df$Alter/10-5,
                      rWC=df$AF.angepasst/100,
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))])
#pairs(dfn)
head(dfn)
summary(head(dfn))
dfn$main_ICD_subZ7<-NULL


################################################################################
## how to predict rWC`?
boxplot(df$AF.angepasst~df$main_ICD,las=2)
boxplot(df$AF.angepasst~df$main_ICD_sub,las=2)


# Basic violin plot
p <- ggplot(df, aes(x=main_ICD, y=AF.angepasst)) + 
  geom_violin()
p


fit<-lm(AF.angepasst~I(AF.angestammt/10),df)
summary(fit)
confint(fit)


table(df$main_ICD)
fit<-lm(AF.angepasst~main_ICD,df)
summary(fit)
confint(fit)

library(cowplot)
x_lab<-"main ICD"
y_lab<-"residual WC"
p <- ggplot(df) + theme_minimal()
p1 <- p + geom_boxplot(aes(x = main_ICD, y = AF.angepasst, color = main_ICD,
                           fill=main_ICD),
                       outlier.shape = NA, alpha = 0.3) +
  geom_jitter(aes(x = main_ICD, y = AF.angepasst, color = main_ICD), alpha = 0.3) +
  scale_y_continuous(y_lab) + scale_x_discrete("") +
  scale_color_discrete(x_lab) + scale_fill_discrete(x_lab)
p2 <- p + geom_histogram(aes(x = AF.angepasst, color = main_ICD, fill = main_ICD),
                         alpha = 0.3, bins = 10) +
  scale_y_continuous("") + scale_x_continuous(y_lab) +
  scale_color_discrete(x_lab) + scale_fill_discrete(x_lab) +
  facet_grid(main_ICD ~ .)
p3 <- p + stat_qq(aes(sample = AF.angepasst, color = main_ICD)) +
  stat_qq_line(aes(sample = AF.angepasst, color = main_ICD)) +
  scale_color_discrete(x_lab) +
  scale_x_continuous("Theoretical Quantile") + scale_y_continuous("Empirical Quantile")
plot_grid(p1, p2, p3,
          align = "vh", ncol = 3, labels = c("A", "B", "C"))


##############################################################################
###test  bayesian
library(tidyverse); set.seed(42)

fit<-lm(AF.angepasst~.,dfn)
summary(fit)
predictions<-predict(fit,newdata = dfn)
plot(predictions, dfn$AF.angepasst)

fit<-lm(AF.angepasst~main_ICDF3,dfn)
summary(fit)

criteria<-which(dfn$main_ICDF3==1&(dfn$age_per_10>5))
dfn[criteria,]
hist(dfn$AF.angepasst[criteria])

brmfit<- brm(bf(AF.angepasst~.),data= dfn,
              family = gaussian(), 
              # prior = c(set_prior("normal(0,5)", class = "b"),
              #           set_prior("cauchy(0,2)", class = "sd"),
              #           set_prior("lkj(2)", class = "cor")),
              # "iter" is the number of iterations
              iter = 4000)
          
summary(brmfit)
prior_summary(brmfit)

brmfit<- brm(bf(AF.angepasst~.),data= dfn,
             family = gaussian(), 
             prior = set_prior("student_t(1,-2,3)", class = "b"),
             
                       set_prior("cauchy(0,2)", class = "Intercept",  ),
                       #set_prior("lkj(2)", class = "cor")),
            #"iter" is the number of iterations
             iter = 4000)
mu1<-10
mu2<-50
sigma1<-5
sigma2<-10
fit <- brm(
  bf(AF.angepasst~.),data= dfn,
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
