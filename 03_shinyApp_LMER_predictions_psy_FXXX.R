
# Load or fit model
# Ideally pre-fit and loaded with `readRDS()` in real app
library("shiny")
# install.packages("shinythemes")
library("shinythemes")

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
                      rWC=df$rWC, 
                      df[,paste0("F",0:9 )])

head(dfn)
dfn<-cbind.data.frame(dfn,df[,grep("F*.XXX",colnames(df),value=T )])
dfn$F7XX<-df$F7XX
dfn$F9XX<-df$F9XX
head(dfn)
table(dfn$F9XX)
model = lmer(rWC ~ MICF_mean  +(1|psy)+ (1|F0XXX)+(1|F1XXX)+(1|F2XXX)+(1|F3XXX)+(1|F4XXX)+(1|F5XXX)+(1|F6XXX)+(1|F7XX)+(1|F8XXX)+(1|F9XX),
               REML=T, data=dfn)
summary(model)
tab_model(model)


ui <- fluidPage(
  titlePanel(" Model Prediction for Residual Work Capacity based on diagnosis and MINI-ICF-APP"),
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
    
    #(pred_rand<-predictInterval( model,  newdata = newdata,  level = 0.95,  n.sims = 1000,  stat = "mean",  type = "linear.prediction"   ))       # could also be "median"  # or "response" 
    
    # Predict using re.form=NA to include random effects
    fit.m<-predict(model,newdata = newdata,re.form = NA,allow.new.levels=T, se.fit=T)
    (pred_fixed<-cbind.data.frame(rWC_prediction=fit.m$fit,
                                  lwr=fit.m$fit+qnorm(c(0.025))*fit.m$se.fit,
                                  upr=fit.m$fit+qnorm(c(0.975))*fit.m$se.fit))
    (fit.c<-predict(model,newdata = newdata,re.form = NULL,allow.new.levels=T))
    
    
    # Try the parametric bootstrap method, and make prediction at the population level
    (pred_rand<- predictInterval( model,  newdata = newdata,  level = 0.95,  
                                  n.sims = 1000,  stat = "median",  type = "linear.prediction"   ))       # could also be "median"  # or "response" 
    
    pred_rand<-cbind.data.frame(rWC_prediction=pred_rand$fit , lwr=pred_rand$lwr, upr=pred_rand$upr)
    (pred<-rbind(pred_fixed,pred_rand))
    pred<-round(pred)
    pred$Type<-c("Marginal (across all diagnosis on average)", "Conditional (selected diagnosis)")
    
    row.names(pred)<-c("fixed effect excluding random effects ", "fixed effect including random effects")
    
        # Render the table
    output$dataTable <- renderTable({
      pred
    })
    # Render the plot
    output$dataPlot <- renderPlot({
      
      ggplot(data=pred, aes(x=Type, y=rWC_prediction , ymin=0, ymax=100,col=Type)) +
        geom_point(size=3)+ # Makes range for ggplot values based on the data and AES specified in first line
        geom_hline(aes(yintercept=rWC_prediction,col=Type), lty=2, linewidth =1) +  # add a dotted line at x=0 after flip
        geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, cex=1)+
        ylab("rWC [%]")+ xlab(" ")+ 
        coord_flip() 
    })
  })
}

shinyApp(ui = ui, server = server)
