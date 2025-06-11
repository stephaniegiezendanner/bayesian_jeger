### bayesian analysis
### beta distribution
### fixed effects: MINI_icf_mean and 13 single domains
### random intercepts for FXXX and random slope for MICF_mean


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

mini_items<-colnames(df)[grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))]

dfn<-cbind.data.frame(id=df$id,
                      sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      age=scale(df$Alter,T, T),
                      psy=df$Arzt,
                      MICF_mean=df$MICF_mean,
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))],
                      rWC=   df$rWC, 
                      df[,paste0("F",0:9 )])

head(dfn)
dfn<-cbind.data.frame(dfn,df[,grep("F*.XXX",colnames(df),value=T )])
dfn$F7XX<-df$F7XX
dfn$F9XX<-df$F9XX
head(dfn)

### beta distribution:
dfn$y<-dfn$rWC/100
dfn$y[which(dfn$y==0)]<-0.00001
dfn$y[which(dfn$y==1)]<-0.99999
hist(dfn$y)
mod_priors <- c(  prior(student_t(3,2.2, 1), class = Intercept) # intercept
                  # slopes
                  # prior(student_t(3,-1, 1), class = b, coef = "MICF_mean")
                  # # error SD
                  # prior(normal(20, 20), class = sigma),
                  # # random effects
                  # prior(normal(0, 20), class = sd )
)
model<-bf(y ~ Regeln.und.Routinen+ Aufgabenplanung+Flexibilitat+
            fachliche.Kompetenzen+Entscheidungsfahigkeit+Spontanaktivitaten+
            Durchhaltefahigkeit+Selbstbehauptungsfahigkeit+Kontaktfahigkeit+
            Gruppenfahigkeit+dyadische.Beziehungen+Selbstpflege+
            Mobilitat+
            (1|psy) + (1|F0XXX)+(1|F1XXX)+
            (1|F2XXX)+(1|F3XXX)+(1|F4XXX)+(1|F5XXX)+
            (1|F6XXX)+(1|F7XX)+(1|F8XXX)+(1|F9XX))

mod_b = brms::brm(
  model,  data  = dfn,
  prior = mod_priors,
  family=Beta(),
  chains = 4,
  iter = 4000
  #,  cores = 4
)
make_stancode(model,
              data = dfn,
              family = Beta())
summary(mod_b)
# These coefficients are on the logit scale by default.
# For interpretation:
# A 1-unit increase in x1 is associated with a log-odds increase of 0.85 in the mean of the response.
# Use plogis(estimate) to transform back to the proportion scale (inverse logit).
plogis ( -1.34) # 0.2075 → the mean response decreases toward 18%

#The Intercept is the expected logit of the mean outcome when all predictors are 0.
# Transform with plogis(Intercept) to get the expected baseline proportion.
plogis( 1.57   ) #0.8277836

# Random Effects
# These show group-level variability
# sd(Intercept) tells you how much variation there is in intercepts across groups.
# Larger values suggest greater heterogeneity between groups.
# 
# Distributional Parameters (Optional)
# If you're modeling phi (the precision), there may be extra coefficients:
# Higher phi means less variance in the outcome (more concentrated around the mean).
# You can model phi ~ predictors if needed.
plot(mod_b)
pp_check(mod_b,ndraws =  100)  # Posterior predictive check

newdata <- data.frame(MICF_mean = 0:4, psy = "H",
                      F0XXX="none",
                      F1XXX="none",
                      F2XXX="none",
                      F3XXX="F322",
                      F4XXX="none",
                      F5XXX="none",
                      F6XXX="none",
                      F7XX="none",
                      F8XXX="none",
                      F9XX="none")
round(predict(mod_b,newdata=newdata, re_formula =NULL,summary=T, robust=T),2)

pp<-predict(mod_b,newdata=newdata, re_formula =NULL,summary=F, robust=T)
boxplot(pp)
brms::prior_summary(mod_b)
inverse_logit = function(x) {
  1 / (1 + exp(-x))
}
hist(inverse_logit(rnorm(1000, 0, 2.5)))
### good prior for intercept 
hist(inverse_logit(rnorm(100, 2, 2.5)),100)
hist(inverse_logit(rnorm(1000, 2.2, 1)),100)

### good prior for MICF slope 
hist(inverse_logit(rnorm(1000,-1, 1)),100)


### check the model fit
pp_check(mod_b, ndraws=100)  
r2<-bayes_R2(mod_b) # Returns mean and CI for Bayesian R²
r2
# bayes_R2(mod_b) 
#      Estimate  Est.Error     Q2.5     Q97.5
# R2 0.6262266 0.01118739 0.6031802 0.6470253
loo(mod_b)    # Leave-one-out cross-validation
# Fitted (predicted) values
pred <- fitted(mod_b, summary = TRUE,robust=T, re_formula=NULL)  # returns mean and 95% CI
head(pred)
# Combine with observed data

results <- tibble(
  observed = mod_b$data$y,
  predicted = pred[, "Estimate"]
)
head(results)

p<-ggplot(results, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method="lm") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Predicted values rWC",
    y = "Observed values rWC",
    title = "Observed vs Predicted values",
    subtitle =paste("R^2:",round(r2[1],2))
  ) +
  theme_minimal()
print(p)
ggsave(filename = file.path("Figures/Bayesian","LMER_beta_predictions_psy_FXXX_random_slope.tiff" ),
       plot=p,device="tiff", width=7, height=0, units="in", dpi=300)

# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(mod_b)

mod_b %>%
  spread_draws(r_psy[condition,]) %>%
  summarise_draws()


mod_b %>%
  spread_draws(b_Intercept,r_psy[condition,b_MICF_mean]) %>%
  median_qi(condition_mean = plogis(b_Intercept + r_psy), .width = c(.95, .66))

mod_b %>%
  spread_draws(b_Intercept,  r_psy[condition,]) %>%
  median_qi(condition_mean =plogis( b_Intercept + r_psy) , .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() 


ui <- fluidPage(
  titlePanel("Bayesian Model Prediction for Residual Work Capacity based on diagnosis and MINI-ICF-APP (Beta distribution)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("MICF_mean", "MICF_mean:", value = 3),
      selectInput("psy", "psy:", choices = c(NA,unique(dfn$psy))),
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
      
      tableOutput("dataTable1"),
      plotOutput("dataPlot"),  # Plot output
      textOutput("text"),
      tableOutput("dataTable2"),
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Create new data for prediction
    # newdata <- data.frame(MICF_mean = input$MICF_mean, psy = input$psy)
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
    
    pp<-add_predicted_draws(newdata=newdata, mod_b,allow_new_levels=TRUE,re_formula = NULL) 
    head(pp)
    
    
    
    # pp$.prediction[which(pp$.prediction>100)]<-100
    # pp$.prediction[which(pp$.prediction<0)]<-0
    output_tabl<-median_qi(pp$.prediction, .width=c(.50,0.66, .80, .95))
    output_tabl[,1:3]<-round(100*output_tabl[,1:3],2)
    colnames(output_tabl )[ 1:4] <-(c("Predicted rWC [median]", 
                                      "Predicted rWC [lower interval]", 
                                      "Predicted rWC [upper interval]", 
                                      "Probability [to determine the widths of the intervals]"))
    
    ### percentage of draws which lie within +-10% from median
    perc_5<-length(which(pp$.prediction>(median(pp$.prediction)-0.05) &
                           pp$.prediction<(median(pp$.prediction)+0.05)) )/length(pp$.prediction)
    perc_10<-length(which(pp$.prediction>(median(pp$.prediction)-0.10) &
                            pp$.prediction<(median(pp$.prediction)+0.10)) )/length(pp$.prediction)
    perc_15<-length(which(pp$.prediction>(median(pp$.prediction)-0.15) &
                            pp$.prediction<(median(pp$.prediction)+0.15)) )/length(pp$.prediction)
    max_diff<-round(100*cbind.data.frame(perc_5,
                                         perc_10,
                                         perc_15))
    
    max_diff<-cbind.data.frame(paste("+/-", c(5, 10, 15),"%"),t(max_diff))
    colnames(max_diff)<-c("Interval around median", "% of data within this range")
    max_diff
    # Render the table
    output$dataTable1 <- renderTable({
      output_tabl[, 1:4]  
    })
    output$text <- renderText({  
      paste("Probability of data for different maximum acceptable intervals in WC ratings")
    })
    output$dataTable2 <- renderTable({
      max_diff 
    })
    # Render the plot
    output$dataPlot <- renderPlot({
      
      ggplot(pp,aes(y = "" , x =100* .prediction)) +
        stat_interval(.width = c(.50,0.66, .80, .95)) +
        scale_color_brewer()+ylab("")+ 
        scale_x_continuous(breaks=seq(0,100,by=10),
                           limits =c(0,100))+
        ylab("")+xlab("Predicted residual work capacity [%]")+
        annotate("text", x=output_tabl[1, 1], 
                 y="", label=paste("Median: \n",round(output_tabl[1, 1]), "%\n\n"))
      # + geom_point(aes(y = "", x = rWC), data = dfn[]) 
      
    })
  })
}

shinyApp(ui = ui, server = server)

