# Load or fit model nested with random intercepts and random slopes
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
table(dfn$n_diag)



# https://mspeekenbrink.github.io/sdam-r-companion/bayesian-estimation-with-brms.html
mod_priors <- c(  prior(normal(100, 30), class = Intercept, lb = 0, ub=100), # intercept
                  # slopes
                  prior(student_t(3,-1, 20), class = b),
                  # nu= the degrees of freedom from the t-distribution
                  # prior(constant(2), class = "nu"),
                  prior(normal(2, 2), class = "nu" ,lb = 0),                  
                  
                  # error SD
                  prior(normal(0, 40), class = sigma, lb = 0),
                  # random effects
                  prior(normal(0, 20), class = sd,lb = 0))



mod_reg_stud_d_rs = brms::brm(
  rWC ~ MICF_mean +
    Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
    fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
    Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
    Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
    Mobilitat+(MICF_mean|psy)+
    (MICF_mean|F0XXX)+(MICF_mean|F1XXX)+
    (MICF_mean|F2XXX)+(MICF_mean|F3XXX)+
    (MICF_mean|F4XXX)+(MICF_mean|F5XXX)+
    (MICF_mean|F6XXX)+(MICF_mean|F7XX)+
    (MICF_mean|F8XXX)+(MICF_mean|F9XX),  data  = dfn, 
  prior = mod_priors,
  family=student(), 
  chains = 4,
  iter =2000
  #,  cores = 4
)

summary(mod_reg_stud_d_rs)
### check the model fit
pp_check(mod_reg_stud_d_rs, ndraws=10)
plot(mod_reg_stud_d_rs)

plot(conditional_effects(mod_reg_stud_d_rs, effects = "MICF_mean"))

posterior_summary(mod_reg_stud_d_rs) 
posterior_samples(mod_reg_stud_d_rs) 
brms::prior_summary(mod_reg_stud_d_rs)
pred <- as.data.frame(fitted(mod_reg_stud_d_rs, robust=T, re_formula=NULL))  # returns mean and 95% CI
head(pred)
pred$obs<-mod_reg_stud_d_rs$data$rWC
pred$Estimate_adj<-pred$Estimate
pred$Estimate_adj[pred$Estimate_adj<0]<-0
pred$Estimate_adj[pred$Estimate_adj>100]<-100

hist(pred$obs, col='red')
par(new=T)
hist(pred$Estimate_adj, col='blue', add=TRUE)

plot(pred$obs~ pred$Estimate_adj)
(res<-summary(lm(pred$obs~ pred$Estimate_adj)))
res$adj.r.squared
# r^2 = 0.6728 

(r2<-bayes_R2(mod_reg_stud_d_rs, robust=T)) # Returns mean and CI for Bayesian R²)
# bayes_R2(mod_reg_stud_d_rs) 
#      Estimate  Est.Error     Q2.5     Q97.5
# R2 0.6640837 0.008953635 0.6454826 0.6801589
loo(mod_reg_stud_d_rs)    # Leave-one-out cross-validation
# Fitted (predicted) values

results <- tibble(
  observed = pred$obs,
  predicted = pred[, "Estimate_adj"]
)
head(results)

p<-ggplot(results, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.6) +
  xlim(c(0,100))+
  ylim(c(0,100))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Predicted values rWC",
    y = "Observed values rWC",
    title = "Observed vs Predicted values",
    subtitle =paste("R^2:",round(res$adj.r.squared,2))
  ) +
  theme_minimal()
print(p)
ggsave(filename = file.path("Figures/Bayesian","LMER_student_domains_psy_FXXX_random_slopes.tiff" ),
       plot=p,device="tiff", width=7, height=0, units="in", dpi=300)

# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(mod_reg_stud_d_rs)

mod_reg_stud_d_rs %>%
  spread_draws(r_F3XXX[condition,]) %>%
  summarise_draws()
mod_reg_stud_d_rs %>%
  spread_draws(r_F5:F5XXX[condition,]) %>%
  summarise_draws()

mod_reg_stud_d_rs %>%
  spread_draws(r_F3XXX[condition,]) %>%
  summarise_draws()
mod_reg_stud_d_rs %>%
  spread_draws(b_Intercept,r_F3XXX[condition,b_MICF_mean]) %>%
  median_qi(condition_mean = b_Intercept + r_F3XXX, .width = c(.95, .66))

mod_reg_stud_d_rs %>%
  spread_draws(b_Intercept, sigma) %>%
  head(10)
mod_reg_stud_d_rs %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)
mod_reg_stud_d_rs %>%
  spread_draws(b_Intercept,  r_F3XXX[condition,], b_) %>%
  median_qi(condition_mean = b_Intercept + r_F3XXX , .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() 


ui <- fluidPage(
  titlePanel("Bayesian Model Prediction for Residual Work Capacity based on diagnosis and MINI-ICF-APP"),
  sidebarLayout(
    sidebarPanel(
      numericInput("MICF_mean", "MICF_mean:", value = 3),
      selectInput("psy", "psy:", choices = unique(dfn$psy)),
      selectInput("F0XXX", "Select F0 diagnosis:", choices = unique(dfn$F0XXX)),
      selectInput("F1XXX", "Select F1 diagnosis:", choices = unique(dfn$F1XXX)),
      selectInput("F2XXX", "Select F2 diagnosis:", choices = unique(dfn$F2XXX)),
      selectInput("F3XXX", "Select F3 diagnosis:", choices = unique(dfn$F3XXX)),
      selectInput("F4XXX", "Select F4 diagnosis:", choices = unique(dfn$F4XXX)),
      selectInput("F5XXX", "Select F5 diagnosis:", choices = unique(dfn$F5XXX)),
      selectInput("F6XXX", "Select F6 diagnosis:", choices = unique(dfn$F6XXX)),
      selectInput("F7XX", "Select F7 diagnosis:", choices = unique(dfn$F7XX)),
      selectInput("F8XXX", "Select F8 diagnosis:", choices = unique(dfn$F8XXX)),
      selectInput("F9XX", "Select F9 diagnosis:", choices = unique(dfn$F9XX)),
      
      actionButton("predict", "Predict")
    ),
    mainPanel(
      tableOutput("dataTable"),
      plotOutput("dataPlot")        # Plot output
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Create new data for prediction
    newdata <- data.frame(MICF_mean = input$MICF_mean, psy = input$psy, 
                          # F0 = ifelse(input$F0XXX=="none",0,1),
                          # F1 = ifelse(input$F1XXX=="none",0,1),
                          # F2 = ifelse(input$F2XXX=="none",0,1),
                          # F3 = ifelse(input$F3XXX=="none",0,1),
                          # F4 = ifelse(input$F4XXX=="none",0,1),
                          # F5 = ifelse(input$F5XXX=="none",0,1),
                          # F6 = ifelse(input$F6XXX=="none",0,1),
                          # F7 = ifelse(input$F7XX=="none",0,1),
                          # F8 = ifelse(input$F8XXX=="none",0,1),
                          # F9 = ifelse(input$F9XX=="none",0,1),
                          # 
                          F0XXX = input$F0XXX,
                          F1XXX = input$F1XXX,
                          F2XXX = input$F2XXX,
                          F3XXX = input$F3XXX,
                          F4XXX = input$F4XXX,
                          F5XXX = input$F5XXX,
                          F6XXX = input$F6XXX,
                          F7XX = input$F7XX,
                          F8XXX = input$F8XXX,
                          F9XX = input$F9XX)
    
    pp<-add_predicted_draws(newdata=newdata, mod_reg_stud_d_rs) 
    head(pp)
    pp$.prediction[which(pp$.prediction>100)]<-100
    pp$.prediction[which(pp$.prediction<0)]<-0
    output_tabl<-median_qi(pp$.prediction, .width=c(.50,0.66, .80, .95))
    output_tabl
    colnames(output_tabl )[ 1:4] <-(c("Predicted rWC [median]", 
                                      "Predicted rWC [lower interval]", 
                                      "Predicted rWC [upper interval]", 
                                      "Probability [to determine the widths of the intervals]"))
    
    # Render the table
    output$dataTable <- renderTable({
      output_tabl[, 1:4]  
    })
    # Render the plot
    output$dataPlot <- renderPlot({
      
      ggplot(pp,aes(y = "" , x = .prediction)) +
        stat_interval(.width = c(.50,0.66, .80, .95)) +
        scale_color_brewer()+ylab("")+ 
        scale_x_continuous(breaks=seq(0,100,by=10),
                           limits =c(0,100))+
        ylab("")+xlab("Predicted residual work capacity[%]")+
        annotate("text", x=output_tabl[1, 1], 
                 y="", label=paste("Median: \n",round(output_tabl[1, 1]), "\n\n"))
      # + geom_point(aes(y = "", x = rWC), data = dfn[]) 
      
      
      
    })
  })
}

shinyApp(ui = ui, server = server)

