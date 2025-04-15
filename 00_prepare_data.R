
# prepare
source("01_prepare_libraries.R")
library("xlsx")
# install.packages("ICD10gm")
library(ICD10gm)
# install.packages("stringdist")
library(stringdist)


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
df$id<-1:nrow(df)
diagnosen<-df$Diagnosen
diagnosen<-sapply(strsplit(diagnosen, ","), paste, collapse = " ") 
diagnosen<-cbind.data.frame(id=df$id,diagnosen)
write.table(diagnosen,file=file.path(path, "../Diagnoses.csv"),sep=",", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )

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

### correct wrong information:
df$fachliche.Kompetenzen[which(df$fachliche.Kompetenzen>4)]<-2
table(df$fachliche.Kompetenzen)

### create new variables
df$main_ICD<-substr(df$ICD.10, 1,2)
table(df$main_ICD)
df$ICD.10[which(df$main_ICD==43)]<-"F43.1"
df$main_ICD[which(df$main_ICD==43)]<-paste0("F",substr(df$main_ICD[which(df$main_ICD==43)],1,1))
df$Diagnosen[which(df$main_ICD=="DS")]

df$ICD.10[which(df$main_ICD=="DS")]
df$ICD.10[which(df$main_ICD=="DS")]<-"F43.25"
df$main_ICD[which(df$main_ICD=="DS")]<-"F4"
df$ICD.10[which(df$main_ICD=="F.")]
df$ICD.10[which(df$main_ICD=="F.")]<-"F33.0"
df$main_ICD[which(df$main_ICD=="F.")]<-"F3"
df$ICD.10[which(df$main_ICD=="FF")]
df$ICD.10[which(df$main_ICD=="FF")]<-"F45.41"
df$main_ICD[which(df$main_ICD=="FF")]<-"F4"
table(df$main_ICD)

ind<-which(df$main_ICD=="F3"|df$main_ICD=="F4")
df$main_ICD_sub<-df$main_ICD
df$main_ICD_sub[ind]<-substr(df$ICD.10[ind],1,3)  

table(df$main_ICD_sub)


### corrections
ind<-which(df$Sex=="W")
df$Sex[ind]<-"F"

# ind<-which(df$main_ICD==43)
# df[ind ,'ICD.10'] <-paste0("F",df[ind ,'ICD.10'])
# df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)
# 
# ind<-which(df$main_ICD=="F.")
# df[ind ,'ICD.10'] <-gsub("F.", "F", df[ind ,'ICD.10'])
# df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)
# 
# ind<-which(df$main_ICD=="FF")
# df[ind ,'ICD.10'] <-gsub("FF", "F", df[ind ,'ICD.10'])
# df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)
# 
# ind<-which(df$main_ICD=="DS")
# df[ind ,'ICD.10']<-"F43.25"
# df[ind ,'main_ICD'] <-substr(df$`ICD.10`[ind], 1,2)

table(df$main_ICD)
df$MEDAS.N<-NULL
df$Explorand.in<-NULL
df$Ge.Datum<-NULL
df$Bermerkungen<-NULL
df$Untersuchung<-NULL
df$letzte.Tatigkeit<-NULL
summary(df)
# 
# #### icd 10 
# df$F0<-0 # Organische, einschließlich symptomatischer psychischer Störungen
# df$F1<-0 # Psychische und Verhaltensstörungen durch psychotrope Substanzen
# df$F2<-0 # Schizophrenie, schizotype und wahnhafte Störungen
# df$F3<-0 # Affektive Störungen
# df$F4<-0 # Neurotische, Belastungs- und somatoforme Störungen
# df$F5<-0 # Verhaltensauffälligkeiten mit körperlichen Störungen und Faktoren
# df$F6<-0 # Persönlichkeits- und Verhaltensstörungen
# df$F7<-0 # Intelligenzstörung
# df$F8<-0 # Entwicklungsstörungen
# df$F9<-0 # Verhaltens- und emotionale Störungen mit Beginn in der Kindheit und Jugend
# df$F99<-0 # Nicht näher bezeichnete psychische Störungen

#################################################################################
### german alternative
# Example: Search for codes related to 'Diabetes'
df$Diagnosen<-gsub(", gegenwärtig", " gegenwärtig",df$Diagnosen)

codes_per_claimant<-list()


source("dictionary.R")
for (i in 355:length(df$Diagnosen)){
  search_terms<-unlist(strsplit(df$Diagnosen[i],","))
  search_terms<-trimws(search_terms)
  if(length(grep("somatoforme Schmerzstörung mit Persönlichkeitsänderung",search_terms))>=1){
   search_terms<- unlist(strsplit(search_terms ,"mit"))  }
  if(length(grep("andauernde Persönlichkeitsänderung mit rezidivierenden Panikattacken",search_terms))>=1){
    search_terms<- unlist(strsplit(search_terms ,"mit"))}
  
  search_terms<-search_terms[!search_terms%in% "HIV-Infektion mit V.a. unerwünschte Arzenimittelwirkungen"]
  search_terms<-search_terms[!search_terms%in% "aktuell remittiert"]
  search_terms<-search_terms[!search_terms%in% "anhaltende kognitive Beeinträchtigungen"]
  search_terms<-search_terms[!search_terms%in% "unerwünschte Arzneimittelwirkungen"]
  
  code_per_search_term<-list()
  for (s in search_terms){
    s_dash<-clean_icd_codes(s, search_terms)
    search_term <- trimws(s_dash)
    icd_codes <- icd_search(search_term)
    # Display results
    (icd_codes$label)
    
    # Calculate string distances (lower = more similar)
    distances <- stringdist(search_term, icd_codes$label, method = "jw")  # Jaro-Winkler is good for typos
    # Get best match
    best_match_index <- which.min(distances)
    best_match <- icd_codes$label[best_match_index]
    best_match_code <- icd_codes$icd_sub[best_match_index]
    print(paste(s,":",s_dash,":",best_match, ":", best_match_code ))
    code_per_search_term[[s_dash]]<-best_match_code
    }
    codes_per_claimant[[i]]<-code_per_search_term
    if (any(sapply(code_per_search_term,function(x) length(x))==0)) {
        break 
      }
    }
codes_per_claimant
empty_entries <- codes_per_claimant[sapply(codes_per_claimant, function(x) length(x) == 0)]
print(empty_entries)

write.table(df,file=file.path(path, "../Jeger_Data_clean.csv"),sep=";", fileEncoding = "UTF-16LE",
            row.names = F, col.names=T )


