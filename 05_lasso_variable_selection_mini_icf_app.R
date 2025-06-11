
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

dfn<-cbind.data.frame(#sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      #age=scale(df$Alter,T, T),
                      #psy=df$Arzt,
                      MICF_mean=df$MICF_mean,
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                           grep("Mobilitat", colnames(df))],
                      rWC=df$rWC, 
                      df[,paste0("F",0:9 )])

head(dfn)
# Define predictors and response
X <- model.matrix(rWC ~ ., data = dfn[complete.cases(dfn),])[, -1]  # remove intercept column
y <- dfn$rWC[complete.cases(dfn)]
# Fit Lasso model (alpha = 1 means Lasso; alpha = 0 is Ridge)
lasso_fit <- glmnet(X, y, alpha = 1)
plot(lasso_fit, xvar = "lambda", label = TRUE)
cv_fit <- cv.glmnet(X, y, alpha = 1)
plot(cv_fit)
best_lambda <- cv_fit$lambda.min
coef(cv_fit, s = "lambda.min")
coef(cv_fit, s = "lambda.1se")

coefs<-coef(cv_fit, s = "lambda.min")

