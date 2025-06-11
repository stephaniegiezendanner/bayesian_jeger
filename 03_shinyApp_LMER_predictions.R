
# Load or fit model
# Ideally pre-fit and loaded with `readRDS()` in real app
library("shiny")
# install.packages("shinythemes")
library("shinythemes")

model <- lmer(rWC ~ MICF_mean  +(1|psy),
              REML=T, data=dfn)
summary(model)
tab_model(model)


ui <- fluidPage(
  titlePanel("Mixed Effects Model Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("MICF_mean", "MICF_mean:", value = 3),
      selectInput("psy", "psy:", choices = unique(dfn$psy)),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      verbatimTextOutput("prediction")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Create new data for prediction
    newdata <- data.frame(MICF_mean = input$MICF_mean, psy = input$psy)
    
    #(pred_rand<-predictInterval( model,  newdata = newdata,  level = 0.95,  n.sims = 1000,  stat = "mean",  type = "linear.prediction"   ))       # could also be "median"  # or "response" 
    
    # Predict using re.form=NA to include random effects
    fit.m<-predict(model,newdata = newdata,re.form = NA,allow.new.levels=T, se.fit=T)
    (pred_fixed<-cbind.data.frame(fit=fit.m$fit,
                                  lwr=fit.m$fit+qnorm(c(0.025))*fit.m$se.fit,
                                  upr=fit.m$fit+qnorm(c(0.975))*fit.m$se.fit))
    
  
    # Try the parametric bootstrap method, and make prediction at the population level
    boot <- bootMer(model,  nsim = 1000, function(x) predict (x, newdata=newdata, re.form = NULL))
    (pred_rand<-quantile(boot$t, c(0.5,0.025,0.975)))
    (pred<-rbind(pred_fixed,pred_rand))
    row.names(pred)<-c("fixed effect excluded random effects ", "fixed effect included random effects")
    pred<-round(pred)
    
    output$prediction <- renderPrint({
      # paste0("Population level prediction= ",pred[1],"% (95% CI: ", pred[2],"-", pred[3],
      #                        "%), \nAverage effect, considering random effects= ",
      #        pred[4],"% (95% CI: ",pred[5],
      #        "-", pred[6],"%)")
      pred
    })
  })
}

shinyApp(ui = ui, server = server)
