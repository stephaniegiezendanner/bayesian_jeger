
# prepare
source("01_prepare_libraries.R")
library("xlsx")

path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

## get data
df<-read.xlsx("../Jeger_Data.xlsx",sheetIndex=1, header=F)#, encoding="utf-8"

head(df)
colnames<-gsub(" |  ", ".",trimws(gsub("[[:digit:]]*.\\.", "", trimws(df[1,]))))
colnames<-iconv(colnames, from = 'UTF-8', to = 'ASCII//TRANSLIT')
colnames<-str_replace_all(colnames, "[^[:alnum:]]", " ")
colnames<-gsub(" ", ".",trimws(colnames))

colnames(df)<- colnames
df<-df[-1,]
head(df)
str(df)

### get numerics
# numerics<-c("AF.angestammt", 
            # "AF.angepasst", 
            # "Alter", 
            # "Regeln.und.Routinen", 
            # "Aufgabenplanung", 
            # "Flexibilität", 
            # "fachliche.Kompetenzen",
            # "Entscheidungsfähigkeit", 
            # "Spontanaktivitäten", 
            # "Durchhaltefähigkeit", 
            # "Selbstbehauptungsfähigkeit", 
            # "Kontaktfähigkeit", 
            # "Gruppenfähigkeit", 
            # "dyadische.Beziehungen",
            # "Selbstpflege", 
            # "Mobilität", 
            # "Gesamtpunktzahl")
numerics<-colnames(df[,!is.na(as.numeric(df[1,]))])

for (i in numerics){
  df[,i]<-as.numeric(df[,i])
}
str(df)

df[,grep("Entscheidungs", colnames(df))]<-as.numeric(df[,grep("Entscheidungs", colnames(df))])

### create new variables
df$main_ICD<-substr(df$ICD.10, 1,2)
ind<-which(df$main_ICD=="F3"|df$main_ICD=="F4")
df$main_ICD_sub<-df$main_ICD
df$main_ICD_sub[ind]<-substr(df$ICD.10[ind],1,3)  



### corrections
ind<-which(df$Sex=="W")
df$Sex[ind]<-"F"

ind<-which(df$main_ICD==43)
df[ind ,'ICD.10'] <-paste0("F",df[ind ,'ICD.10'])
df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)

ind<-which(df$main_ICD=="F.")
df[ind ,'ICD.10'] <-gsub("F.", "F", df[ind ,'ICD.10'])
df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)

ind<-which(df$main_ICD=="FF")
df[ind ,'ICD.10'] <-gsub("FF", "F", df[ind ,'ICD.10'])
df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)

ind<-which(df$main_ICD=="DS")
df[ind ,'ICD.10']<-"F43.25"
df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)

table(df$main_ICD)
df$MEDAS.N<-NULL
df$Explorand.in<-NULL
df$Ge.Datum<-NULL
df$Bermerkungen<-NULL
df$Untersuchung<-NULL
df$letzte.Tatigkeit<-NULL
str(df)
write.table(df,file=file.path(path, "../Jeger_Data_clean.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )


