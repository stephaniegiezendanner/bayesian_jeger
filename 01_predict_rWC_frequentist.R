
# source("00_prepare_data.R")
source("01_prepare_libraries.R")

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean_f_diag.csv"),sep=";", 
             fileEncoding = "UTF-16LE")
head(df) 
data_fxxx<-list()
for (num in c(3,4,6)){
  data<-df[,grep(paste0("^F",num,".*.."), colnames(df))]
  data$id<-df$id
  ind<-which(rowSums(data[, 1:(length(data)-1)])!=0)
  data[ind,]
  data[,paste0("F",num,"XXX")]<-"none"
  data[,paste0("F",num,"XXX")][ind] <- toupper(names(data)[max.col(data[ind,1:(length(data)-2)])])
  head(data)
  data_fxxx[[paste(num)]]<-data[,c("id",paste0("F",num,"XXX"))]
}
data_fxxx

data_fxx<-list()
for (num in  c(3,4,6)){
  data<-df[grepl(paste0("^F",num,".*.$"), colnames(df)) & nchar(colnames(df))==3]
  data$id<-df$id
  ind<-which(rowSums(data[, 1:(length(data)-1)])!=0)
  data[ind,]
  data[,paste0("F",num,"XX")]<-"none"
  data[,paste0("F",num,"XX")][ind] <- toupper(names(data)[max.col(data[ind,1:(length(data)-2)])])
  head(data)
  data_fxx[[paste(num)]]<-data[,c("id",paste0("F",num,"XX"))]
}
data_fxx
fxx_diagnoses<-Reduce(function(x, y) merge(x, y,all=TRUE), data_fxx)
head(fxx_diagnoses)
fxxx_diagnoses<-Reduce(function(x, y) merge(x, y,all=TRUE), data_fxxx)
head(fxxx_diagnoses)


dfn<-cbind.data.frame(id=df$id,
                      sex_f=(ifelse(df$Sex=="F", 1,0)), 
                      age=scale(df$Alter,T, T),
                      psy=df$Arzt,
                      MICF_mean=df$Gesamtpunktzahl/13,
                      #MICF_mean_cat=cut(df$Gesamtpunktzahl/13,breaks=c(0:4), include.lowest = T, right = F),
                      df[,grep("Regeln.und.Routinen", colnames(df)):
                                 grep("Mobilitat", colnames(df))],
                      rWC=df$AF.angepasst, 
                      df[,paste0("F",0:9 )])

head(dfn)


# correlation
cor(dfn$rWC, dfn$MICF_mean, 
    method = "spearman",use="pairwise.complete.obs" )
cor(dfn$rWC, dfn$MICF_mean, 
    method = "pearson",use="pairwise.complete.obs" )

plot(rWC ~ MICF_mean, data=dfn)
plot(rWC ~ age, data=dfn)


# simple linear regression
model_sim = lm(rWC ~ MICF_mean, data=dfn)
summary(model_sim)
# Adjusted R-squared:  0.4916
confint(model_sim)
newdata<-cbind.data.frame(MICF_mean=c(0:4))
predict(model_sim,newdata = newdata,interval = "prediction",level = 0.95)
predict(model_sim,newdata = newdata,interval = "confidence",level = 0.95)


# multiple linear regression
model_multi = lm(rWC ~ .-id -psy, data=dfn)
summary(model_multi)
# Adjusted R-squared:  0.582
confint(model_multi)
newdata<-cbind.data.frame(MICF_mean=c(0:4), 
                          sex_f=c(0),
                          age=0, 
                          Regeln.und.Routinen=c(0:4),
                          Aufgabenplanung=c(0:4),
                          Flexibilitat=c(0:4),
                          fachliche.Kompetenzen=c(0:4),
                          Entscheidungsfahigkeit=c(0:4),
                          Spontanaktivitaten=c(0:4),        
                          Durchhaltefahigkeit=c(0:4),
                          Selbstbehauptungsfahigkeit=c(0:4),
                          Kontaktfahigkeit=c(0:4),
                          Gruppenfahigkeit=c(0:4),
                          dyadische.Beziehungen=c(0:4),     
                          Selbstpflege=c(0:4),
                          Mobilitat=c(0:4),
                          F0=0,
                          F1=0,
                          F2=0,
                          F3=0,
                          F4=0,
                          F5=0, 
                          F6=0,
                          F7=0,                        
                          F8=0,
                          F9=0, 
                          psy="H",
                          id=1)
newdata
predict(model_multi,newdata = newdata,interval = "prediction",level = 0.95)
predict(model_multi,newdata = newdata,interval = "confidence",level = 0.95)

dfn<-merge(dfn,fxx_diagnoses, by='id')
dfn<-merge(dfn,fxxx_diagnoses, by='id')
head(dfn)

# Model A - unconditional means
modelal = lmer(rWC ~ MICF_mean + (1|psy)+ (1|F3)  , #+ (1|F3) + (1|F3XX) + (1|F3XXX) 
               REML=T, data=dfn)
summary(modelal)
#  93.54 the mean rWC across all MICF_mean and psy:
#  369.45 the variance within patients
#  70.99 the variance between psy
# only 19.2% of total variability is due to difference between psychiatrists.