# Load or fit model nested 
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

# https://mspeekenbrink.github.io/sdam-r-companion/bayesian-estimation-with-brms.html
mod_priors <- c(# intercept
  prior(normal(95, 30), class = Intercept), 
  # slopes
  prior(normal(-30, 20), class = b, coef = "MICF_mean"),
  
  # error SD
  prior(normal(20, 20), class = sigma),
  # random effects
  prior(normal(0, 20), class = sd)
)
mod_regression = brms::brm(
  rWC ~ MICF_mean  +(1|psy) +(1|psy)+ (1|F0/F0XXX)+(1|F1/F1XXX)+(1|F2/F2XXX)+(1|F3/F3XXX)+(1|F4/F4XXX)+(1|F5/F5XXX)+(1|F6/F6XXX)+(1|F7/F7XX)+(1|F8/F8XXX)+(1|F9/F9XX),  data  = dfn,
  prior = mod_priors,
  family=gaussian()
  #,  cores = 4
)


summary(mod_regression)
plot(mod_regression)

brms::prior_summary(mod_regression)

newdata <- data.frame(MICF_mean = 0:4, psy = "H")
predict(mod_regression,newdata=newdata, re_formula =NULL,summary=T, robust=T)

pp<-predict(mod_regression,newdata=newdata, re_formula =NULL,summary=F, robust=T)
boxplot(pp)

### check the model fit
pp_check(mod_regression, ndraws=100)  
r2<-bayes_R2(mod_regression) # Returns mean and CI for Bayesian R²
# bayes_R2(mod_regression) 
#      Estimate  Est.Error     Q2.5     Q97.5
# R2 = 0.6171532 0.01179845 0.592848 0.6383107
loo(mod_regression)    # Leave-one-out cross-validation
# Fitted (predicted) values
pred <- fitted(mod_regression, summary = TRUE,robust=T, re_formula=NULL)  # returns mean and 95% CI
head(pred)
# Combine with observed data

results <- tibble(
  observed = mod_regression$data$rWC,
  predicted = pred[, "Estimate"]
)
head(results)

p<-ggplot(results, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Predicted values rWC",
    y = "Observed values rWC",
    title = "Observed vs Predicted values",
    subtitle =paste("R^2:",round(r2[1],2))
  ) +
  theme_minimal()
print(p)
ggsave(filename = file.path("Figures/Bayesian","LMER_predictions_psy_FX_nested_FXXX.tiff" ),
        plot=p,device="tiff", width=7, height=0, units="in", dpi=300)

# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(mod_regression)

mod_regression %>%
  spread_draws(r_F5[condition,]) %>%
  summarise_draws()
mod_regression %>%
  spread_draws(r_F5:F5XXX[condition,]) %>%
  summarise_draws()

mod_regression %>%
  spread_draws(r_F3XXX[condition,]) %>%
  summarise_draws()
mod_regression %>%
  spread_draws(b_Intercept,r_F3XXX[condition,b_MICF_mean]) %>%
  median_qi(condition_mean = b_Intercept + r_F3XXX, .width = c(.95, .66))

mod_regression %>%
  spread_draws(b_Intercept, sigma) %>%
  head(10)
mod_regression %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)
mod_regression %>%
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
                          F0 = ifelse(input$F0XXX=="none",0,1),
                          F1 = ifelse(input$F1XXX=="none",0,1),
                          F2 = ifelse(input$F2XXX=="none",0,1),
                          F3 = ifelse(input$F3XXX=="none",0,1),
                          F4 = ifelse(input$F4XXX=="none",0,1),
                          F5 = ifelse(input$F5XXX=="none",0,1),
                          F6 = ifelse(input$F6XXX=="none",0,1),
                          F7 = ifelse(input$F7XX=="none",0,1),
                          F8 = ifelse(input$F8XXX=="none",0,1),
                          F9 = ifelse(input$F9XX=="none",0,1),
                          
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
    
    pp<-add_predicted_draws(newdata=newdata, mod_regression) 
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
                           limits =c(0,100))
      +
        ylab("")+xlab("Predicted residual work capacity [%]")+
        annotate("text", x=output_tabl[1, 1], 
                 y="", label=paste("Median: \n",round(output_tabl[1, 1]), "%\n\n"))
      # + geom_point(aes(y = "", x = rWC), data = dfn[]) 
    
      
    })
  })
}

shinyApp(ui = ui, server = server)
