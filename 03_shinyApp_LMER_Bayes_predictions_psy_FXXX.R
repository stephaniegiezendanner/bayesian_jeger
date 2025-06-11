
# Load or fit Bayesian model with random intercepts for FXXX
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
  prior(normal(0, 20), class = sigma),
  # random effects
  prior(normal(0, 20), class = sd, coef = "Intercept", group="psy"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F0XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F1XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F2XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F3XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F4XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F5XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F6XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F7XX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F8XXX"),
  prior(normal(0, 20), class = sd, coef = "Intercept", group="F9XX")
  )
mod_regression = brms::brm(
  rWC ~ MICF_mean  +(1|psy) +(1|psy)+ (1|F0XXX)+(1|F1XXX)+(1|F2XXX)+(1|F3XXX)+(1|F4XXX)+(1|F5XXX)+(1|F6XXX)+(1|F7XX)+(1|F8XXX)+(1|F9XX),  data  = dfn,
  #prior = mod_priors,
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


# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(mod_regression)

mod_regression %>%
  spread_draws(r_F4XXX[condition,]) %>%
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
       # + geom_point(aes(y = F3XXX, x = rWC), data = dfn[,c("rWC","F3XXX" )]) 
      
    })
  })
}

shinyApp(ui = ui, server = server)
