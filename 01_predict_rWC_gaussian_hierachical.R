### hierarchical model:
# https://cu-psych-computing.github.io/cu-psych-comp-tutorial/tutorials/r-extra/brms/multilevel-models-with-brms/
################################################################################

# source("00_prepare_data.R")
source("01_prepare_libraries.R")
library(data.table)
library("bayesplot")
library("ggplot2")
library("rstanarm")
library("broom")
library("tidyverse")
# https://bjsmith.github.io/post/geom_hdi_for_ggplot2/
source("plot_geom_hdi.R")
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

dfn<-cbind.data.frame(main_ICD=df$main_ICD,
                      main_ICD_sub=df$main_ICD_sub,#trsf_main_ICD, 
                      fem_sex=df$Sex_f, 
                      age_cent_by_10=scale(df$Alter/10,T,F),
                      total_mean_cent=scale(df$Gesamtpunktzahl/13, T,F),
                      total_cat=cut(df$Gesamtpunktzahl/13, breaks=0:4,include.lowest = T),
                      scale(df[,grep("Regeln.und.Routinen", colnames(df)):
                                 grep("Mobilitat", colnames(df))],T,F),
                      rWC=df$AF.angepasst)
#pairs(dfn)
summary(dfn)
table(dfn$main_ICD_sub)
mini_icf<-grep("Regeln.und.Routinen", colnames(df)):grep("Mobilitat", colnames(df))
dfn<-cbind.data.frame(main_ICD=df$main_ICD,
                      main_ICD_sub=df$main_ICD_sub,#trsf_main_ICD, 
                      ceiling(df[,mini_icf])-2,
                      rWC=df$AF.angepasst)
summary(dfn)
mini_icf<-grep("Regeln.und.Routinen", colnames(dfn)):grep("Mobilitat", colnames(dfn))
dfn$total <-rowSums(dfn[,mini_icf],na.rm = T)/rowSums(!is.na(dfn[,mini_icf]))
plot(dfn$total,df$Gesamtpunktzahl)
dfn$total_cat<-cut(dfn$total,breaks=-2:3,include.lowest = T,right = F)
summary(dfn)
write.table(dfn,file=file.path(path, "../Jeger_Data_regression_pred_centered.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )



###################################################################################
dfn$id<-1:nrow(dfn)
nested <- dfn %>%
  group_by( main_ICD) %>%
  group_nest(.key = "trials")
nested


nested_with_models <- nested %>%
  mutate(models = map(trials, ~lm(rWC ~ total_mean_cent, data = .)),
         coefs = map(models, ~tidy(.)))

nested_with_models[['coefs']]

# Plot the estimated effect of is old for each subject

indivModelSummaries = nested_with_models %>%
  dplyr::select(-models, -trials) %>%
  unnest(coefs) %>%
  filter(., term == 'total_mean_cent')
indivModelSummaries

ggplot(indivModelSummaries, aes(x = main_ICD, y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error, width = 0)) +
  geom_point(color = 'purple') + 
  labs(x = 'Subject ICD-10', y = 'Estimated rWC', title = '1 Model Per Diagnosis') +
  geom_hline(yintercept = 0, color = 'red', lty = 3)# + ylim(-5, 1)

# individual Summary (no pooling) and multilevel model comparison!
rawMeans <- dfn %>%
  dplyr::group_by(main_ICD,total_mean_cent   , fem_sex) %>%
  summarise(rWC = mean(rWC,na.rm=T))
rawMeans

noPoolModel <- lm(data = rawMeans, rWC ~ total_mean_cent   *fem_sex)
summary(noPoolModel, digits = 4)



### test hierarchical model
# prior belief: 
mu<-50
mu_sd<-20
sigma<-20
sigma_sd<-10

brmfit_hier<- brm(bf(rWC~total_mean_cent  +
                       total_mean_cent*main_ICD + (1|main_ICD)+(1|total_cat)),data= dfn,
                  prior = c(set_prior("student_t(1, 0, 10)", class = "b"),
                            set_prior("normal(50, 20)", class = "Intercept"),
                            set_prior("normal(20, 10)", class = "sigma") ),
                  
                  
                  # prior = #c(#set_prior("student_t(1,-2,1)", class = "b"),
                  #   c(prior(gamma(2, 1), class = nu),
                  #     prior(normal(mu, mu_sd), class = Intercept),### how confident about the peak
                  #     prior(normal(sigma, sigma_sd), class = sigma) ,
                  #     prior(normal(0,10), class = b)),
                  
                  # "iter" is the number of iterations
                  # sample_prior = "only",
                  family = gaussian(),
                  iter = 4000)

summary(brmfit_hier)
brmfit_hier$prior
prior_summary(brmfit_hier)
posterior_summary(brmfit_hier)
pp_check(brmfit_hier, prefix = "ppd", ndraws = 100)
pp_check(brmfit_hier, prefix = "ppc", ndraws = 100)
mcmc_plot(brmfit_hier, type = "dens")
plot(brmfit_hier)

(a <- summary(brmfit_hier))
(fixedEffect <- a$fixed[2,1])

(ranef(brmfit_hier)[[1]])
subjectEffectEstimates <- data.frame(ranef(brmfit_hier)[[1]]) %>%
  cbind(indivModelSummaries) %>%
  mutate(Estimate.Intercept = Estimate.Intercept + fixedEffect)


ggplot(subjectEffectEstimates , aes(x = main_ICD, y = Estimate.Intercept)) +
  geom_errorbar(aes(ymin = Estimate.Intercept - 2*Est.Error.Intercept, ymax = Estimate.Intercept + 2*Est.Error.Intercept, width = 0)) +
  geom_point(color = 'red') + 
  labs(x = 'ICD-10', y = 'estimated Effect of total MINI-ICF App score (centered/scaled) on rWC', title = 'Partial Pooling (red) vs. No Pooling (purple)') +
  geom_hline(yintercept = 0, color = 'red', lty = 3) +
  #ylim(-5, 1) +
  geom_point(data = indivModelSummaries, aes(x = main_ICD, y = estimate), color = 'purple', position = position_nudge(.2)) +
  geom_errorbar(data = indivModelSummaries, aes(x = main_ICD, y = estimate, ymin = estimate - 2*std.error, 
                                                ymax = estimate + 2*std.error, width = 0),
                position= position_nudge(.2))

# Multilevel model predictions for the age effect
grid <- expand.grid(total_mean_cent = seq(-1.5,2,0.5),fem_sex=c(0,1),  
                    main_ICD=paste0("F",0:9),
                    age_cent_by_10=0,total_cat=levels(dfn$total_cat) )
grid
partialPoolPredicts <- fitted(brmfit_hier, newdata = grid, re_formula = NA) %>% #,probs=c(0.33,0.66)) %>%
  cbind(grid, .)
partialPoolPredicts$main_ICD<-as.factor(partialPoolPredicts$main_ICD)
partialPoolPredicts$fem_sex<-as.factor(partialPoolPredicts$fem_sex)
#rawMeans<-rawMeans[which(rawMeans$main_ICD=="F3"|rawMeans$main_ICD=="F4"|rawMeans$main_ICD=="F6"),]
partialPoolPredicts$Estimate<-ifelse(partialPoolPredicts$Estimate>100,100,partialPoolPredicts$Estimate)
partialPoolPredicts$Estimate<-ifelse(partialPoolPredicts$Estimate<0,0,partialPoolPredicts$Estimate)


ggplot(partialPoolPredicts) +
  geom_ribbon(aes(x = total_mean_cent, y = Estimate, ymin = Q2.5, ymax = Q97.5, group = main_ICD), alpha = .3) +
  geom_line(aes(x = total_mean_cent, y = Estimate, group = main_ICD, color = main_ICD), lwd = 1) +
  labs(y = 'Estimated rWC', title = 'Multilevel Model Age Effect Estimates\nPoints Are Raw Means') +
  geom_point(data = rawMeans, aes(x = total_mean_cent, y = rWC, color = factor(main_ICD))) +
  ylim(c(0,100))
  #+  scale_color_brewer(palette = 'Set1')

### try to compare to real values
grid <-dfn[, c("total_mean_cent", "fem_sex" , "main_ICD", "age_cent_by_10", "total_cat")]
partialPoolPredicts <- fitted(brmfit_hier, newdata = grid, re_formula = NA,probs=c(0.1,0.33,0.5,0.66,0.9)) %>% #,probs=c(0.33,0.66)) %>%
  cbind(grid, .)
partialPoolPredicts$rWC<-dfn$rWC
summary(lm(partialPoolPredicts$rWC~partialPoolPredicts$Estimate))

ggplot(partialPoolPredicts)+
  #geom_ribbon(aes(x =rWC  , y = Estimate, ymin = Q10, ymax = Q90, group = main_ICD), alpha = .3) +
  geom_point(aes(x =rWC  , y = Estimate,group = main_ICD, color = main_ICD), lwd = 1) +
  labs(x = 'Observed rWC', title = 'Multilevel Model') +
  ylim(c(0,100))

## compute expected predictions
ppe<-posterior_epred(brmfit_hier)
dim(ppe)

# fitted estimates
bfitted_values <- fitted(brmfit_hier, summary=F)
head(bfitted_values[,1])

# predicted estimates
bpredict_values <- predict(brmfit_hier,cores=1,summary=F)
head(bpredict_values[,1])

### adapt for no values <0 and >100
bpredict_values_cut<-ifelse(bpredict_values<0,0,bpredict_values)
bpredict_values_cut<-ifelse(bpredict_values_cut>100,100,bpredict_values_cut)


ppc_error_hist(y=brmfit_hier$data$rWC, yrep=bpredict_values_cut[1:10,])
ppc_scatter(y=brmfit_hier$data$rWC, yrep=bpredict_values_cut[1:10,])

data <- data.frame(x = rnorm(10,mu,mu_sd))
p<-ppc_dens_overlay(y=brmfit_hier$data$rWC, yrep=bpredict_values_cut[1:100,])
p+  stat_function(fun = dnorm,
                  args = list(mean = mean(data$x),
                              sd = sd(data$x)),
                  col = "red",
                  linewidth = 2) +
  annotate("text", x=mu, y=0.03, label= paste("Prior\nM=",mu, 
                                              "\nsd=",mu_sd))+
  annotate("text", x=50, y=0.04, 
           label= paste("Posterior\nM=",round(summary(brmfit_hier)$fixed$Estimate), 
                        "\nsd=",round(sumary_brm$spec_pars$Estimate[1])))


###################################################################################################
### new model incorporating more info
brmfit_sing<- brm(bf(rWC~ main_ICD+
                       Regeln.und.Routinen + 
                       Aufgabenplanung+ 
                     Flexibilitat+ 
                     fachliche.Kompetenzen + 
                     Entscheidungsfahigkeit+ 
                     Spontanaktivitaten+ 
                     Durchhaltefahigkeit+ 
                     Selbstbehauptungsfahigkeit+ 
                     Kontaktfahigkeit+ 
                     Gruppenfahigkeit+ 
                     dyadische.Beziehungen+ 
                     Selbstpflege + 
                     Mobilitat +  (1|main_ICD)+(1|total_cat)),data= dfn,
                  prior = c(set_prior("student_t(1, 0, 10)", class = "b"),
                            set_prior("normal(50, 20)", class = "Intercept"),
                            set_prior("normal(20, 10)", class = "sigma") ),

                  family = gaussian(),
                  iter = 4000)

summary(brmfit_sing)
plot (brmfit_sing)
prior_summary(brmfit_sing)

# Plot posterior distributions with 95% credible intervals
mcmc_areas(posterior_samples, pars = c("b_Regeln.und.Routinen"), prob = 0.95)

posterior_preds <- posterior_predict(brmfit_sing)
dim(posterior_preds)


### make predictions for a particular case
grid <-expand.grid(Regeln.und.Routinen=c(0),
                     Aufgabenplanung=0,
                     Flexibilitat=0,
                     fachliche.Kompetenzen=0,
                     Entscheidungsfahigkeit=0,
                     Spontanaktivitaten=0,
                     Durchhaltefahigkeit=c(-2,2),
                     Selbstbehauptungsfahigkeit=0,
                     Kontaktfahigkeit=0,
                     Gruppenfahigkeit=0,
                     dyadische.Beziehungen=0,
                     Selbstpflege=0,
                     Mobilitat =0,
                     main_ICD="F3")


head(grid)                     
grid$total_cat<-cut(rowSums(grid[, 1:13])/13,breaks =-2:3,include.lowest = T,right = F )          
table(grid$total_cat)
dim(grid)

posterior_preds <- posterior_predict(brmfit_sing,newdata =grid,allow_new_levels=T)
dim(posterior_preds)
hist(posterior_preds)
posterior_preds_long <- pivot_longer(as.data.frame(posterior_preds), 
                                     cols =1:ncol(posterior_preds) ,  # Columns to pivot
                                     names_to = "Patient",              # New column for years
                                     values_to = "PosteriorPredictive"  )
# New column for values
probs<-c(0.05,0.33, 0.5,0.66, 0.95)
interval<-diff(probs[rev(end(probs))])
pred_summary <- apply(posterior_preds, 2, quantile, probs = probs)
pred_summary <-aggregate(posterior_preds_long$PosteriorPredictive, by=list(posterior_preds_long$Patient), quantile,probs)
pred_summary
p<-ggplot(posterior_preds_long, aes(x=PosteriorPredictive, group=Patient , fill=factor(Patient ),color=factor(Patient))) +
  geom_freqpoly(binwidth=10)+
  geom_hdi(aes(color=Patient ),lineend="round",size=2,credible_mass=diff(probs[rev(end(probs))]),alpha=0.8)+
  labs(title="Posterior Predictive",x="rWC")
print(p)
# Add text annotation at x=4, y=25
p + annotate("text", x = as.numeric(as.data.frame(pred_summary$x)[2]), y = 0, 
                   label = paste(interval, "HDI"), size = 3, color =1 )


partialPoolPredicts <- fitted(brmfit_sing, newdata = grid, re_formula = NA,probs=c(0.1,0.9)) %>% #,probs=c(0.33,0.66)) %>%
  cbind(grid, .)
# Plot posterior distributions with 95% credible intervals


mcmc_areas(posterior_samples, pars = c("b_wt"), prob = 0.95)

