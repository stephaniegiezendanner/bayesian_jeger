
############# test brms package


path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)


# read in data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean.csv"),sep="\t", 
             fileEncoding = "UTF-16LE")

df_sub<-df[which(df$main_ICD_sub=="F32"),]
hist(df_sub$AF.angepasst)

df_sub$group<-ifelse (df_sub$AF.angepasst<20,"<20",">20")


numerics<-c("AF.angestammt", 
            "AF.angepasst",
            "Alter",
            "Regeln.und.Routinen",
            "Aufgabenplanung",
            "Flexibilitat",
            "fachliche.Kompetenzen",
            "Entscheidungsfahigkeit",
            "Spontanaktivitaten",
            "Durchhaltefahigkeit",
            "Selbstbehauptungsfahigkeit",
            "Kontaktfahigkeit",
            "Gruppenfahigkeit",
            "dyadische.Beziehungen",
            "Selbstpflege",
            "Mobilitat",
            "Gesamtpunktzahl")
t<-list()
for (n in numerics){
  t[[n]]<-t.test(df_sub[,n]~df_sub$group)$p.value
  boxplot(df_sub[,n]~df_sub$group)
}
numerics[which(do.call(rbind,t)<0.05)]

chisq.test(table(df_sub$Sex,df_sub$group))
chisq.test(table(,df_sub$group))
