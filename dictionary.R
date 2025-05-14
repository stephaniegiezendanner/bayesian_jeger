clean_icd_codes <- function(s, search_terms) {
  s<-trimws(s)
  s<-tolower(s)
  
  ### typos/ unneccessary details
  (s<-gsub("\\(.*?\\)", "", s))
  (s<-gsub("\\[.*?\\]", "", s))
  (s<-gsub("seit mindestens 2015", "",s))
  (s<-gsub("nach autounfall am 10.01.2010", "",s))
  (s<-gsub("nach psych traumatisierung durch unfall", "",s))
  (s<-gsub("chron ","chronische ",s)   )
  (s<-gsub("somat ","somatischen ",s)   )
  (s<-gsub("som ","somatischen ",s)   )
  (s<-gsub("psych ","psychischen ",s) )
  (s<-gsub("nach verkehrsunfall","",s) )
  (s<-gsub("längere","",s) )
  (s<-gsub("atyp, ", "",s))
  (s<-gsub("atypische ", "",s))
  (s<-gsub("bzw", "",s))
  (s<-gsub("z.z.", "",s))
  (s<-gsub("schwerer verdacht auf", "",s))
  (s<-gsub("abhänglgikeit", "abhängigkeit",s))
  (s<-gsub("tranezustände", "trancezustände",s))
  (s<-gsub("gegenäwrtig", "gegenwärtig",s))
  (s<-gsub("aktuell", "",s))   
  (s<-gsub("somatichem", "somatischem",s))   
  (s<-gsub("dringender Verdacht auf", "",s))   
  (s<-gsub("mittelgrqadig", "mittelgradig",s))   
  (s<-gsub("z.zt.", "",s))   
  (s<-gsub("zZt", "",s))
  (s<-gsub("\\biatrogen\\w*\\b", "",s))   
  (s<-gsub("opat", "opiat",s))   
  (s<-gsub("abhöngigkeit", "abhängigkeit",s))   
  (s<-gsub("abhängikeit", "abhängigkeit",s))   
  (s<-gsub("entsicklung", "entwicklung",s))   
  (s<-gsub("  ", " ",s))
  (s<-gsub("persönlichektisstörung", "persönlichkeitsstörung",s))
  (s<-gsub("persistierende", "",s))
  (s<-gsub("schweregrad unklar", "",s))
  (s<-gsub("redizidivierende", "rezidivierende",s))
  (s<-gsub("akzentzuierte", "akzentuierte",s))
  (s<-gsub("akzent ", "akzentuierte ",s))
  (s<-gsub("aktenmässig ", "",s))
  #(s<-gsub("wechslenden ausmasses", "",s))  
  (s<-gsub("instablien", "instabilen",s))  
  (s<-gsub("rezidiverende", "rezidivierende ",s))  
  (s<-gsub("persönlochkeitsstörung", "persönlichkeitsstörung",s))  
  s<-gsub("schwierigikeiten","schwierigkeiten",s)
  if(length(grep("wahnhaft*.",s))==0 &length(grep("depressi*.",s))==0 &length(grep("affektiv*.",s))==0 & length(grep("somatoforme schmerzstörung",s))==0  ){
    (s<-gsub("anhaltende", "",s))   
  }
  if(length(grep("subsyndromal*", s))>=1 &length(grep("depressive Störung", s))>=1 & length(grep("leicht", s))==0 ){
    (s<-gsub("subsyndromale","leichte",s))
  }else{
    (s<-gsub("subsyndromale","",s))
  }
  s<-gsub("spzifische","",s)
   s<-gsub("unter therapie","",s)
  s<-gsub("depr. ","depressive",s)
  s<-gsub("aktzentuierte","akzentuierte",s)
  s<-gsub("akzentzierte","akzentuierte",s)
  s<-gsub("abhänhikgkeit","abhängigkeit",s)
  s<-gsub("persönlichkeitsszügen","persönlichkeitszügen",s)
  s<-gsub("mittelschwre","mittelschwere",s)
   
  s<-gsub("mit erheblicher resiudalsymptomatik","",s)
  s<-gsub("cancer related fatigue/","",s)
  #s<-gsub("autounfall 1997","",s)
  
  
  (s<-gsub("mit inadäquater krankheitsverarbeitung", "",s))   
  (s<-gsub("episodemn", "episode",s))  
  (s<-gsub("aktenanamnestisch ", "",s))   
  (s<-gsub("anamnestisch", "",s))   
  (s<-gsub("bekannte", "",s))   
  (s<-gsub("moprhin", "morphin",s))   
  (s<-gsub("depreswsive", "depressive",s))   
   
  if(length(grep("somaotoforme",s))>=1){
    (s<-gsub("somaotoforme","somatoforme",s))
  }
  (s<-gsub("rez |rez\\.","rezidivierende ",s))
  (s<-gsub("st.n. ","",s))
  (s<-gsub("st. n. ","",s))
  (s<-gsub("v. a. ","",s))
  (s<-gsub("austonome","autonome",s))
  (s<-gsub("des urogenitaltrakts","",s))
  (s<-gsub("bei myotonia congenita curshmann-steinert","",s))
  (s<-gsub("nach herzstillstand mit reanimation","",s))
  #(s<-gsub("unter methadon-behandlung","",s))
  (s<-gsub("aktuell unter therapie leichten grades","",s))
  (s<-gsub("grosser verdacht auf","",s))
  (s<-gsub("agitierte ","",s))
  (s<-gsub("mit pseudodemenz","",s))
  (s<-gsub("am 3.5.2010 nach sturz vom balkon in suizidaler absicht","",s))
  (s<-gsub("uaw durch interaktion","",s))
  (s<-gsub("nach multiplen traumatischen ereignissen und","",s))
  (s<-gsub("des gi-traktes","",s))
  (s<-gsub("nach jahrelangem distress","",s))
  (s<-gsub("chronifzierte angstbetonte","",s))
  (s<-gsub("nach unfall und ","",s))
  (s<-gsub("persönlichekitsänderung","persönlichkeitsänderungen",s))
  (s<-gsub("sekundäres","",s))
  (s<-gsub("sekundäre","",s))
  (s<-gsub("auf dem boden einer ","",s))
  (s<-gsub("des gi-trakts","",s))
  (s<-gsub("saisonale","",s))
  (s<-gsub("opioid-abhänglgiekti","opioidabhängigkeit",s))
  (s<-gsub("mit einer somatischen dissoziation erklärbar","",s))
  (s<-gsub("anakastische","anankastische",s))
  (s<-gsub("langdauernde ","chronische",s))
  (s<-gsub("zur zeit schweren grades","schwere",s))
  (s<-gsub("ads","sonstige hyperkinetische störungen",s))
  (s<-gsub("des erwachsenen","",s))
  (s<-gsub("im erwachsenenalter","",s))
  (s<-gsub("\\bagitiert\\w*\\b", "",s))
  (s<-gsub("nach vulva-operation", "",s))
  (s<-gsub("stlörung", "störung",s))
  (s<-gsub("leiche", "leichte",s))
  (s<-gsub("mittelgradie", "mittelgradige",s))
  (s<-gsub("kurze", "",s))
  (s<-gsub("saisonal verstärkt", "",s))
  (s<-gsub("psychotischemSyndrom", "psychotischem syndrom",s))
  (s<-gsub("im schnitt", "",s))
  (s<-gsub("nher", "näher",s))
  (s<-gsub("weitgehend","",s))
  (s<-gsub("persönlichkeitsänederung","persönlichkeitsänderung",s))
  (s<-gsub("anngst","angst",s))
  (s<-gsub("dringender verdacht auf","",s))
  (s<-gsub("psychotischem syndrom", "psychotischen symptomen",s))
  
  
  (s<-gsub("im rahmen einer multiplen sklerose","",s))
  if (length(grep("persönlichkeitsstörung",s))==0){
    (s<-gsub("kombinierte","",s))
  }
  (s<-gsub("nach jahrelanger traumatisierung","",s))
  (s<-gsub("mehrfache schwere traumatisierung","Posttraumatische Belastungsstörung",s))
  (s<-gsub("im nachgang zu einem gewaltverbrechen","",s))
  (s<-gsub("v\\.a\\.","",s))
  (s<-gsub("\\biatrogene\\b*w","",s))
  (s<-gsub("gemsicht","gemischt",s))
  (s<-gsub("weitgehend remittiert","gegenwärtig remittiert",s))
  (s<-gsub("entsprechend einer","",s))
  (s<-gsub("\\bepisode\\w*\\b","episode",s))
  (s<-gsub("  "," ",s))
  
  

    #### schweregrad  
  (s<-gsub("leicht mit mittelgradig","leicht bis mittelgradig",s))
  if(length(grep("depressive episode",s))==1&length(grep("leicht*",s))==1){
  (s<-"leichte depressive episode")
  }
   (s<-gsub("\\bmittelschwer\\w*\\b bis \\bschwer\\w*\\b","mittelschwer bis schwere",s))
  if(length(grep("mittelschwer bis schwere",s))==1&length(grep("\\bdeperssive\\w*\\b episode*.",s))==1){
    (s<-"Mittelgradige depressive Episode")
  }
  (s<-gsub("mittelschwere ","mittelgradige ",s) )
  (s<-gsub("leichte bis mittelgradige","leichte",s))
  (s<-gsub("leicht bis \\bmittelgradig\\w*\\b","leichte episode",s))
  if(length(grep("mittelgradig*.",s))==1&length(grep("episode*.",s))==1){
    (s<-gsub("einer mittelgradigen episode entsprechend","gegenwärtig mittelgradige episode",s))
  }
  
  (s<-gsub("leichten ausmasses","leichte episode",s))
  (s<-gsub("leicht bis mittelschweren grades","leichte episode",s))
  (s<-gsub("leichten bis mittelschweren Ausmasses","leichte episode",s))
  (s<-gsub("leichten bis mittelgradigen","leichte",s))
  (s<-gsub("mittelgradig bis schwergradig","mittelgradig",s))
  (s<-gsub("mittelgradig bis schwer","mittelgradig",s))
  (s<-gsub("mittelgradig bis schwer","mittelgradig",s))
   
  (s<-gsub("schweregrad mittel bis schwer","gegenwärtig mittelgradige episode",s)   )
  (s<-gsub("mittel- bis schwergradige","mittelgradige",s)   )
  (s<-gsub("mittelgradige bis schwere", "mittelgradige",s))
  (s<-gsub("schweregrad mittelschwer bis schwer", "gegenwärtig mittelgradige episode",s))
  (s<-gsub("schweregrad mittelschwer","gegenwärtig mittelgradige episode",s)   )
  if(length(grep("depressive*. episode",s))==1){
    (s<-gsub("mittelschwer bis schwere", "mittelgradige",s))
    (s<-gsub("\\bdepressiv\\w*\\b","depressive",s))
  }
  (s<-gsub("einer leichten bis mittelgradigen depressiven episode", "",s))
  if(length(grep("chronifizierte depressive entwicklung leichten bis mittleren grades",s))==1){
    (s<-"Rezidivierende depressive Störung, gegenwärtig leichte Episode")
  }
  if(length(grep("chronifizierte depressive entwicklung mittelschwer ausgeprägt",s))==1){
    (s<-"Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")
  }
  
  ### affektive
  (s<-gsub("anhaltende depressive störung wechslenden ausmasses", "Sonstige depressive Episoden",s))
  
  if(length(grep("anhaltende|\bchron\\w*\\b",s))==1&length(grep("depression",s))==1 &(length(grep("leicht*.|mittel*.|schwer*.",s))==0)){
      (s<-"Dysthymia")   
  }
  if(length(grep("anhaltende|\\bchron\\w*\\b",s))==1&length(grep("depression",s))==1 &(length(grep("leicht*.|mittel*.|schwer*.",s))==1)){
    if(length(grep("leicht*.",s))==1){
      (s<-"Rezidivierende depressive Störung, gegenwärtig leichte Episode")   
    } else if(length(grep("mittel*.",s))==1){
      (s<-"Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")   
    } else if(length(grep("schwer*.",s))==1){
      (s<-"Rezidivierende depressive Störung, gegenwärtig schwere Episode")   
    }
   
  }
  
  if(length(grep("depressive*. episode",s))==1){
    (s<-gsub("andauernde", "",s))
    (s<-gsub("mit somatischem syndrom", "",s))
  }
  (s<-gsub("leichte episode depressivität", "Leichte depressive Episode",s))
  if(length(grep("angst und depression",s))==0){
    (s<-gsub("depression","depressive episode ",s))  
  }

  (s<-gsub("depressive reaktion","depressive episode",s))   
  (s<-gsub("remittierte depressive reaktion",
           "rezidivierende depressive störung, gegenwärtig remittiert",s))
  #(s<-gsub("depressive Störung","depressive Episode",s)  ) 
  if (length(grep("atypisch",s))>=1& length(grep("depressive störung",s))>=1){
    (s<-"Sonstige rezidivierende depressive Störungen")
  }

  (s<-gsub("anhaltende mitelgradige depressive störung",
           "rezidivierende depressive störung, gegenwärtig mittelgradige episode",s))
  
  (s<-gsub("rezidivierende gegenwärtig mittelgradige bis schwere episode",
           "rezidivierende depressive störung, gegenwärtig mittelgradige episode",s))
  
  (s<-gsub("leichtgradige depressive episode","rezidivierende depressive störung, gegenwärtig leichte episode",s))
  (s<-gsub("leichte depressive störung","rezidivierende depressive störung, gegenwärtig leichte episode",s))
  
  s<-gsub("agitierte depressive episode","depressive episode",s)
  (s<-gsub("depressiven episoden","depressive episode",s))
  (s<-gsub("mit rezidivierenden","rezidivierende",s))
   (s<-gsub("rezidivierende depressive episode","rezidivierende depressive störung",s))
  (s<-gsub("rezidivierende depressive störung  gegenwärtig leichtgradig",
           "rezidivierende depressive störung gegenwärtig leichte episode" ,s))
  (s<-gsub("rezidivierende depression mittelgradig",
           "rezidivierende depressive störung, gegenwärtig mittelgradige episode" ,s))  
  
  if(length(grep("rezidivierende \\bdepression\\w*\\b",s))==1) {
    (s<-gsub("rezidivierende depression",
             "rezidivierende depressive störung" ,s))  
  }
  
  if(length(grep("depressive entwicklung",s))==1 & length(grep("chronische",s))==0 & length(grep("episode",s))==0  ) {
    (s<-gsub("depressive entwicklung","depressive episode",s))   
  }else if (length(grep("depressive entwicklung",s))==1 & length(grep("chronisch*.|rezidivierend*.",s))==1){
    (s<-gsub("depressive entwicklung","depressive störung",s))   
  }else if (length(grep("depressive entwicklung",s))==1 & length(grep("episode",s))==1){
    (s<-gsub("depressive entwicklung","depressive störung",s))   
  }

  if(length(grep("depressive störung",s))==1 & 
     length(grep("rezidivierende",s))==0  ){
    (s<-gsub("depressive störung",
             "rezidivierende depressive störung" ,s))  
  } 
  
  if(length(grep("rezidivierende depressive störung",s))==1 & 
     length(grep("depressive episode",s))==1  ){
    (s<-gsub("depressive episode",
             "episode" ,s))  
  } 
  if(length(grep("leichte episode",s))==1 &
     length(grep("gegenwärtig",s))==0 & length(grep("störung",s))==1 ){
    (s<-"rezidivierende depressive Störung, gegenwärtig leichte Episode")
  }
  
  if(length(grep("mittelgradig",s))==1 &length(grep("gegenwärtig",s))==0 & length(grep("depressiv*. störung",s))==1){
    (s<-"rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")
  }
  if(length(grep("mittleren",s))==1 &length(grep("gegenwärtig",s))==0 & length(grep("depressiv*. störung",s))==1 ){
    (s<-"rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")
  }
  
  if(length(grep("depressiver episode",s))==1){
  }
  if(length(grep("depressive*. episode",s))==1 &length(grep("\\bsomatisch\\w*\\b",s))>=1){
  (s<-gsub("\\bsomatisch\\w*\\b syndrom","",s)) 
  (s<-gsub(" mit | ohne ","",s)) 
  }
  if(length(grep("mittelgradige depressive episode",s))==1 ){
    (s<-"Mittelgradige depressive Episode") 
  }
  if(length(grep("depressive störung",s))>=1){
    (s<-gsub("ängstliche","",s)   )
  }

  if(length(grep("mittel- bis schwergradig",s)>=1)  ){
    (s<-gsub(" mittel- bis schwergradig"," mittelgradige episode",s)   )
    if (length(grep("gegenwärtig",s))==0){
      (s<-gsub("störung","störung, gegenwärtig",s)   )
    }
  }
  (s<-gsub("mittelgradige depressive störung","rezidivierende depressive störung, gegenwärtig mittelgradige episode",s)   )
  if (length(grep("mittelgradige depressive episode mit somatischem syndrom",s))==1 ){
    (s<-"mittelgradige depressive episode")
  }
  (s<-gsub("remittierte depressive episode","rezidivierende depressive störung, gegenwärtig remittiert", s))
  if(length(grep("remittiert",s))>=1 &length(grep("depressiv",s))>=1){
    (s<-"rezidivierende depressive störung, gegenwärtig remittiert")
  }
  if (length(grep("leichte depressive episode",search_terms))==1&length(grep("begleitende angststörung",s))==1 ){
    (s<-gsub("begleitende angststörung", "leichte depressive episode",s))
  }
  
  if (length(grep("gegenwärtig",s))==1&length(grep(",",s))==0 ){
    (s<-gsub(" gegenwärtig", ", gegenwärtig",s))
  }
  if (length(grep("rezidivierende depressive störung",s))==1){
    (s<-gsub("leichte depressive episode",
             "leichte episode",s))  
  }
  if(length(grep("remittierte",s))>=1 & length(grep("gegenwärtig",s))==0 ){
    s<-gsub("remittierte","",s)   
    s<-paste0(s,", gegenwärtig remittiert")
  }
  #(s<-gsub("schwere depressive Störung","Rezidivierende depressive Störung, gegenwärtig schwere Episode",s))   
  (s<-gsub("schwere .*. depressive störung",
           "rezidivierende depressive störung, gegenwärtig schwere episode",s))   
  if (str_count(s, "depressive störung")>1&length(grep("leichtgradige",s))>=1){
    (s<-"rezidivierende depressive störung, gegenwärtig leichte episode")
  }
  
  (s<-gsub("ausgeprägte erschöpfungsdepression","schwere depressive episode",s))   
   
  if (length(grep("rezidivierende depressive störung, gegenwärtig leichte episode",s))>=1){
    s<-gsub("mit somatischem syndrom","",s)  
    
  }
  if(length(grep("anhaltende affektive störung",s))==1 &length(grep("dysthymen",s))==1){
    (s<-"dysthymia")
  }else if (length(grep("dysthymia",s))==1){
    (s<-"dysthymia")
  }
  (s<-gsub("gegenwärtig rezidivierende","rezidivierende",  s))
  
  if(length(grep("depressive episode mittelgradig",s))==1){
    (s<-"mittelgradige depressive episode")
  }

  if (length(grep("chron*.",s))>=1&length(grep("depressiv*. störung",s))>=1&
      length(grep("rezidivierende",s))==0){
    (s<-gsub("chronische", "rezidivierende",s))
  }else if (length(grep("chron*.",s))>=1&length(grep("depressiv*. störung",s))>=1&
            length(grep("rezidivierende",s))==1){
    (s<-gsub("\\bchro\\w*\\b", "",s))
  }else if (length(grep("chron*",s))>=1&length(grep("depressiv*. episode",s))>=1){
    (s<-gsub("\\bchro\\w*\\b", "",s))
  }
  s<-gsub(", gegenwärtig mittelgradige rezidivierende depressive störung",
       "rezidivierende depressive störung, gegenwärtig mittelgradige Episode" ,s)
  s<-gsub(", gegenwärtig mittelgradige depressive episode",
          "mittelgradige depressive episode" ,s)
  if(length(grep("leichtgradig",s))>=1 & length(grep("depressive störung",s))>=1 ){
    (s<-"rezidivierende depressive Störung, gegenwärtig leichte Episode")
  }
  if(length(grep("mittelgradige",s))>=1 & length(grep("rezidivierende depressive störung",s))>=1 ){
    (s<-"Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")
  }
  s<-gsub(", gegenwärtig  rezidivierende depressive störung",
          "rezidivierende depressive störung" ,s)
  
  
  if (length(grep("\\bängstlich\\w*\\b",s))>=1&length(grep("depressiv*",s))>=1&length(grep("störung*|episode",s))>=1){
    (s<-gsub("ängstlich", "",s))
  }
  
  if (length(grep("depressiv*",s))>=1&length(grep("episode*",s))>=1&length(grep("gegenwärtig*",s))==0 &
      length(grep("störung*",s))>=1){
      if (length(grep("leichte",s))>=1){
        (s<-gsub(" leichte", ", gegenwärtig leichte",s))
      }
      if(length(grep("mittelgradige",s))>=1 ){
        (s<-gsub(" mittelgradige", ", gegenwärtig mittelgradige",s))
      } 
      if(length(grep("schwere",s))>=1 ){
        (s<-gsub(" schwere", ", gegenwärtig schwere",s))
      }
  }
  
  if(length(grep("depressive episode*",s))>=1 & length(grep("gegenwärtig mittelgradige episode",s))>=1 ){
    (s<-"mittelgradige depressive episode")
  } 
  if(length(grep("depressive episode",s))>=1 & length(grep("schwere",s))>=1 ){
    (s<-"schwere depressive episode")
  } 
  (s<-gsub("bipolare affektive störung typ II", "sonstige bipolare affektive störungen",s))
  if(length(grep("rezidivierende depressive störung",s))==1 & 
     length(grep("depressive episode",s))==1  ){
    (s<-gsub("depressive episode",
             "episode" ,s))  
  } 
  if(length(grep("rezidivierende depressive störung",s))==1){
    (s<-gsub("mit somatischem syndrom",
             "" ,s))  
  } 
  if(length(grep("rezidivierende depressive störung",s))==1 & 
     length(grep("mittelschwer",s))==1){
    (s<-"rezidivierende depressive störung, gegenwärtig mittelgradige episode")  
  } 
  if(length(grep("depressive episode",s))==1&length(grep("mittelgradig*",s))==1){
    (s<-"Mittelgradige depressive episode")
  }
  if(length(grep("bipolar*.",s))==1&length(grep("affektive störung",s))==1&length(grep("depressiv*.",s))==1 
     &length(grep("schwer*.|leicht*.|mittel*.",s))==0 ){
    (s<-"Bipolare affektive Störung, gegenwärtig leichte oder mittelgradige depressive Episode")
  }
  if(length(grep("rezidivierende",s))==1&length(grep("episode",s))==1&length(grep("depressive Störung",s))==0 ){
    if(length(grep("leicht*.",s))>=1){
      s<-"Rezidivierende depressive Störung, gegenwärtig leichte Episode"
    }else if(length(grep("mittel*.",s))>=1){
      s<-"Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode"
    }else if(length(grep("schwer*.",s))>=1){
      s<-"Rezidivierende depressive Störung, gegenwärtig schwere Episode"
    }
  }
  if(length(grep("rezidivierende",s))==1&length(grep("anhaltende",s))==1&length(grep("depressive störung",s))==1 ){
    s<-gsub("anhaltende", "",s)
  }
  
  
  #### sucht
  (s<-gsub("psychische und verhaltensstörung durch psychotrope substanzen",
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen" ,s))  
  if(length(grep("opiat",s))>=1 &length(grep("benzo",s))>=1 &length(grep("abhängigkeit",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Abhängigkeitssyndrom")
  }
  if(length(grep("benzo",s))>=1 &length(grep("abhängigkeit",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika : Abhängigkeitssyndrom")
  }
  if(length(grep("benzo",s))>=1 &length(grep("gebrauch",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika : Schädlicher Gebrauch")
  }
  if(length(grep("benzo*.",s))>=1 &length(grep("gebrauch|anwendung",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika : Schädlicher Gebrauch")
  }
  if(length(grep("sedativa",s))>=1 &length(grep("gebrauch",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika : Schädlicher Gebrauch")
  }
  if(length(grep("cannabis",s))>=1 &length(grep("abhängigkeit",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Cannabinoide : Abhängigkeitssyndrom")
  }
  if(length(grep("cannabis",s))>=1 &length(grep("gebrauch",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Cannabinoide : Schädlicher Gebrauch")
  }
  if(length(grep("heroin",s))>=1 &length(grep("abhängigkeit",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Opioide : Abhängigkeitssyndrom")
  }
  
  if(length(grep("morphium",s))>=1 &length(grep("abhängigkeit",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch Opioide : Abhängigkeitssyndrom")
  }
  (s<-gsub("akute opiatintoxikation","Psychische und Verhaltensstörungen durch Opioide : Akute Intoxikation [akuter Rausch]",s))
  if(length(grep("persönlichkeitsstörung residualaffektives zustandsbild mit depressiven und manischen komponenten benzodiazepinabhängigkeit",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika: Restzustand und verzögert auftretende psychotische Störung")
  }
  if(length(grep("opioid- und morphinabhängigkeit",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Opioide : Abhängigkeitssyndrom")
  }
  if(length(grep("opiat*.",s))>=1 & length(grep("*.abhängigkeit",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Opioide : Abhängigkeitssyndrom")
  }
  if(length(grep("morphin*.",s))>=1 & length(grep("*.abhängigkeit",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Opioide : Abhängigkeitssyndrom")
  }
  if(length(grep("schädlicher gebrauch",s))>=1 &length(grep("alkohol",s))>=1 ){
    (s<-"Psychische und Verhaltensstörungen durch Alkohol : Schädlicher Gebrauch")
  }
  if(length(grep("alkohol",s))>=1 &length(grep("konsum",s))>=1 ){
    (s<-"Psychische und Verhaltensstörungen durch Alkohol : Schädlicher Gebrauch")
  }
   if(length(grep("medikament*.",s))>=1 &length(grep("gebrauch",s))>=1 ){
    (s<-"Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen : Schädlicher Gebrauch")
  }
  if(length(grep("*abhängigkeit*",s))>=1 &length(grep("alkohol",s))>=1 ){
    (s<-"Psychische und Verhaltensstörungen durch Alkohol : Abhängigkeitssyndrom")
  }
  if(length(grep("abhängigkeit*.",s))>=1 &length(grep("sedativa",s))>=1 ){
    (s<-"Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika : Abhängigkeitssyndrom")
  }
  if(length(grep("konsum*.",s))>=1 &length(grep("drogen",s))>=1 &length(grep("multipler",s))>=1  ){
    (s<-"Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen : Schädlicher Gebrauch")
  }
  
  (s<-gsub("anhaltende kognitive beeinträchtigung bei polytoxikomanie",
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Restzustand und verzögert auftretende psychotische Störung",s))  
  if(length(grep("polytoxikomanie",s))>=1 ){
    if(length(grep("residuale störung der persönlichkeit",s))>=1){
      (s<-"Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen : Restzustand und verzögert auftretende psychotische Störung")
    }else{
      (s<-"multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen : Abhängigkeitssyndrom" )  
    }
   
  }
   (s<-gsub("diverse substanzabhängigkeiten","multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen",s) )  

  (s<-gsub("residualaffektives zustandsbild","Restzustand und verzögert auftretende psychotische Störung",s) )  
  
  (s<-gsub("benzodiazepinabhängigkeit", 
           "Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika, Abhängigkeitssyndrom",s))
  (s<-gsub("alkoholbdedingte wesensveränderung", 
           "Psychische und Verhaltensstörungen durch Alkohol",s))
  (s<-gsub("aethylkonsum", 
           "Psychische und Verhaltensstörungen durch Alkohol: Schädlicher Gebrauch",s))
  (s<-gsub("schädlicher alkoholgebrauch", 
           "Psychische und Verhaltensstörungen durch Alkohol: Schädlicher Gebrauch",s))
  
  (s<-gsub("alkoholabhängigkeitssyndrom gegenwärtiger substanzgebrauch", 
           "Psychische und Verhaltensstörungen durch Alkohol: Schädlicher Gebrauch",s))
  
   (s<-gsub("alkoholabhängigkeit gegenwärtig abstinent", 
           "Psychische und Verhaltensstörungen durch Alkohol: Abhängigkeitssyndrom",s))
  (s<-gsub("alkoholabhängigkeitssyndrom", 
           "Psychische und Verhaltensstörungen durch Alkohol: Abhängigkeitssyndrom",s))
  
  
  (s<-gsub("psychische und verhaltensstörungen durch sedativa oder hypnotika, abhängigkeitssyndrom", 
           "Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika: Abhängigkeitssyndrom",s))
  (s<-gsub("langjähriger drogen- und alkoholabusus", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Abhängigkeitssyndrom",s))
  
  if (length(grep("opiatabhängigkeit ",s))>=1){
  (s<-"Psychische und Verhaltensstörungen durch Opioide: Abhängigkeitssyndrom")
  }
  if (length(grep("opioidsubstitution ",s))>=1){
    (s<-"	Psychische und Verhaltensstörungen durch Opioide: Abhängigkeitssyndrom")
  }
  if (length(grep("cannabisabhängigkiet",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Cannabinoide : Abhängigkeitssyndrom")
  }
  if (length(grep("multiplen substanzgebrauch",s))>=1 &length(grep("abhängigkeit",s))>=1){
    (s<-"	Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen : Abhängigkeitssyndrom")
  }
  
  (s<-gsub("psychotische störung aufgrund von multiplen substanzgebrauch und konsum anderer psychotroper substanzen", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Psychotische Störung",s))
  (s<-gsub("psychischen störung durch multiple substanzen", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Psychotische Störung",s))
  (s<-gsub("psychischen abhängigkeit", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Abhängigkeitssyndrom",s))
  (s<-gsub("multiplen substanzgebrauch und konsum anderer psychotroper substanzen restzustand mit persönlichkeitsstörung", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Restzustand und verzögert auftretende psychotische Störung",s))
    if (length(grep("bipolare affektive störung mit neuropsychologischen defiziten",s))>=1){
        (s<-"Bipolare affektive Störung, gegenwärtig remittiert")
      }
  
  
    if (length(grep("opiatabhängigkeit|opioidabhängigkeit",s))>=1){
      (s<-"Psychische und Verhaltensstörungen durch Opioide: Abhängigkeitssyndrom")
    }
   (s<-gsub("schädlicher cannabisgebrauch","Psychische und Verhaltensstörungen durch Cannabinoide: Schädlicher Gebrauch",s))
   (s<-gsub("schädlicher gebrauch von cannabis","Psychische und Verhaltensstörungen durch Cannabinoide: Schädlicher Gebrauch",s))
   (s<-gsub("cannabisabhängigkeitssyndrom","Psychische und Verhaltensstörungen durch Cannabinoide: Abhängigkeitssyndrom",s))
  
  if(length(grep("lsd",s))>=1 & length(grep("abhängigkeit",s))>=1){
   (s<-"Psychische und Verhaltensstörungen durch Halluzinogene: Restzustand und verzögert auftretende psychotische Störung")
  }
  
   (s<-gsub("restzustand mit persönlichkeitsstörung nach multiplen substanzgebrauch und konsum anderer psychotroper substanzen",
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Restzustand und verzögert auftretende psychotische Störung",s))

  if(length(grep("psychologische faktoren bei andernorts klassifizierten *.",s))>=1){
    s<-"Psychologische Faktoren oder Verhaltensfaktoren bei anderenorts klassifizierten Krankheiten"
  }
  ### organische ursache
  if(length(grep("organisch*.",s))>=1 & length(grep("depressiv*.",s))>=1){
    s<-"Organische affektive Störungen"
  }
  (s<-gsub("frontalhirnatrophie", 
           "Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns",s))
  (s<-gsub("organisches psychosyndrom nach tramatischer hirnverletzung", 
           "Organisches Psychosyndrom nach Schädelhirntrauma",s))
  if(length(grep("organische persönlichkeitsstörung",s))>=1){
    s<-"Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  if(length(grep("organisch*.",s))>=1 &length(grep("persönlichkeits*.*änderung",s))>=1 ){
    s<-"Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  if(length(grep("organisch*.",s))>=1 & length(grep("schädelhirntrauma|sht",s))>=1 & length(grep("psychosyndrom",s))>=1){
    s<-"Organisches Psychosyndrom nach Schädelhirntrauma"
  }
  (s<-gsub("organische emotional labile störung", 
           "Organische emotional labile [asthenische] Störung",s))
  (s<-gsub("organische emotional instabile störung", 
           "Organische emotional labile [asthenische] Störung",s))
   (s<-gsub("organische depressive störung", 
           "Organische affektive Störungen",s))
  (s<-gsub("organische depressive störung", 
           "Organische affektive Störungen",s))
  (s<-gsub("sht", 
           "schädelhirntrauma",s))
  if (length(grep("psychoorganisches syndrom",s))>=1 & length(grep("schädelhirntrauma",s))>=1){
    (s<-"Organisches Psychosyndrom nach Schädelhirntrauma")
  }
  if (length(grep("organische",s))>=1 & length(grep("affektive störung",s))>=1){
    (s<-"Organische affektive Störungen")
  }
 
  #### personality
  (s<-gsub("ängstliche persönlichkeitsstörung", 
           "ängstliche (vermeidende) persönlichkeitsstörung",s))
  
  if (s=="persönlichkeitsänderung"){
    (s<-"Sonstige andauernde Persönlichkeitsänderungen")
  }
  if (length(grep("persönlichkeitsstörung", s))>=1&length(grep("komb*.", s))>=1) {
    (s<-"Kombinierte und andere Persönlichkeitsstörungen")
  }
  if (length(grep("anankastische",s))>=1){
    (s<-"Anankastische [zwanghafte] Persönlichkeitsstörung")
  }
  if (length(grep("kombinierte persönlichkeitsstörung mit narzisstischen und dissoziativen zügen",s))>=1){
    (s<-"Sonstige spezifische Persönlichkeitsstörungen")
  }
  if (length(grep("kombinierte persönlichkeitsstörung",s))>=1){
    (s<-"kombinierte und andere persönlichkeitsstörungen")
  }
  if (length(grep("kombinierte",s))>=1& length(grep("persönlichkeitsstörung",s))>=1){
    (s<-"Kombinierte und andere Persönlichkeitsstörungen")
  }
  if (length(grep("selbstunsichere",s))>=1& length(grep("persönlichkeitsstörung",s))>=1){
    (s<-"Ängstliche (vermeidende) Persönlichkeitsstörung")
  }
  if(length(grep("persönlichkeitsänderung",s))>=1){
    (s<-gsub("beginnende","",s)   )
    
  }
  if(length(grep("sonstige organische persönlichkeits- und verhaltensstörung aufgrund einer krankheit",s))>=1){
    s<-"Sonstige organische Persönlichkeits- und Verhaltensstörungen aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  if(length(grep("andauernde persönlichkeitsänderung bei chronische herzmuskelerkrankung und chronischem schmerzsyndrom",s))>=1){
    (s<-"Andauernde Persönlichkeitsänderung bei chronischem Schmerzsyndrom")
  }
 
  if(length(grep("andauernde persönlichkeitsänderung",s))>=1& length(grep("schweren progredienten somatischen erkrankung",s))>=1){
    (s<-"Sonstige andauernde Persönlichkeitsänderungen")
  }
  if(length(grep("andauernde persönlichkeitsänderung nach langjähriger psychischer erkrankung",s))>=1){
    (s<-"Andauernde Persönlichkeitsänderung nach psychischer Krankheit")
  }
  if(length(grep("andauernde persönlichkeitsveränderung",s))>=1 &length(grep("psychischen erkrankung",s))>=1){
    (s<-"Andauernde Persönlichkeitsänderung nach psychischer Krankheit")
  }
  
  
  if(length(grep("andauernde persönlichkeitsänderung",s))>=1&length(grep("dissoziat*.",s))>=1){
    (s<-"Andauernde Persönlichkeitsänderung nach psychischer Krankheit")
  }
  if(length(grep("andauernde persönlichkeitsänderung",s))>=1&length(grep("hirninfarkt",s))>=1){
    (s<-"Sonstige organische Persönlichkeits- und Verhaltensstörungen aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns")
  }
  (s<-gsub("andauernde persönlichkeitsänderung nach folter und anderen extrembelastungen", 
           "Andauernde Persönlichkeitsänderung nach Extrembelastung",s))
  (s<-gsub("trauerrekation", "trauerreaktion",s))
  (s<-gsub("tauerreaktion", "trauerreaktion",s))
  
  if(length(grep("protrahierte trauerreaktion",s))>=1){
    (s<-"Anpassungsstörungen")
  }
  if(length(grep("mit emotional instabilen zügen vom borderline-typus",s))==1 ){
    (s<-"Emotional instabile Persönlichkeitsstörung: Borderline-Typ")
  }
  if(length(grep("borderline",s))==1&length(grep("persönlichkeitssörung",s))==1  ){
    (s<-"Emotional instabile Persönlichkeitsstörung: Borderline-Typ")
  }
  if(length(grep("borderline",s))==1&length(grep("persönlichkeitszügen ",s))==1  ){
    (s<-"Emotional instabile Persönlichkeitsstörung: Borderline-Typ")
  }
  
  (s<-gsub("perönlichkeitszüge", "persönlichkeitszüge",s))
  if(length(grep("akzentuierte persönlichkeit*.",s))>=1|length(grep("akzentuierung von persönlichkeitszügen",s))>=1){
    (s<-"Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung")
  }
  if(length(grep("persönlichkeitsstörung",s))>=1&length(grep("narzisstische",s))>=1 ){
    (s<-"Sonstige spezifische Persönlichkeitsstörungen")   
  }
  if(length(grep("persönlichkeit*.",s))>=1&length(grep("neurotisch*.",s))>=1 ){
    (s<-"Sonstige spezifische Persönlichkeitsstörungen")   
  }
  (s<-gsub("\\babhängige persönlichkeit\\w*\\b", "Abhängige (asthenische) Persönlichkeitsstörung",s))
  (s<-gsub("spezifische persönlichkeitsstörung mit selbstunsicheren und schizoiden Zügen", "Ängstliche (vermeidende) Persönlichkeitsstörung",s))
  
   
  
  s<-gsub("akzentuierte persönlichkeitszüge","Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung",s)
   s<-gsub("schwierigkeiten mit der kulturellen eingewöhnung","Kontaktanlässe mit Bezug auf die soziale Umgebung",s)
   s<-gsub("schwierigkeiten bei der kulturellen eingewöhnung","Kontaktanlässe mit Bezug auf die soziale Umgebung",s)
  
  s<-gsub("essattacken bei sonstigen psychischen störungen",
          "Essattacken bei anderen psychischen Störungen",s)
  (s<-gsub("leichte zwangshandlungen","Zwangshandlungen",s))
 
  
  
  
   (s<-gsub("atypische depressive episode ","Sonstige depressive Episoden",s))
  s<-gsub("dd ","",s)
   s<-gsub("borderlinepersönlichkeit","Borderline-Typ",s)
  (s<-gsub("depersonalisationsstörung", "Depersonalisations",s))
  (s<-gsub("depersonalisationssyndrom", "Depersonalisations",s))
  (s<-gsub("dissoziative störung gemischt","Dissoziative Störungen [Konversionsstörungen], gemischt", s))
  (s<-gsub("dissoziative störungen gemischt","Dissoziative Störungen [Konversionsstörungen], gemischt", s))
   (s<-gsub("gemischte dissoziative störung","Dissoziative Störungen [Konversionsstörungen], gemischt", s))
   (s<-gsub("dissoziative zustände","Dissoziative Störungen [Konversionsstörungen]", s))
   (s<-gsub("dissoziative identitätsstöung","Multiple Persönlichkeit(sstörung)", s))
  (s<-gsub("auffällige persönlichkeitszüge","Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung", s))
  (s<-gsub("persönlichkeitsänderung bei epilepsie und depressiver entwicklung",
           "Andauernde Persönlichkeitsänderung, nicht näher bezeichnet",s))
  (s<-gsub("persönlichkeits- und verhaltensstörung aufgrund einer erkrankung des gehirns",
           "Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns",s))
  if(length(grep("persönlichkeitsänderung",s))>=1&length(grep("schmerzen",s))>=1&length(grep("polytrauma ",s))==0 ){
    (s<-"Andauernde Persönlichkeitsänderung bei chronischem Schmerzsyndrom")
  }
   
   if(length(grep("persönlichkeitsänderung",s))>=1&length(grep("epilepsie",s))>=1 ){
     (s<-"Organische Persönlichkeitsstörung")
   }
  #(s<-gsub("kombinierte Persönlichkeitsstörung","Kombinierte und andere Persönlichkeitsstörungen", s))
  (s<-gsub("psychosomatische persönlichkeitsstruktur","Andauernde Persönlichkeitsänderung, nicht näher bezeichnet", s))
  
  
  if(length(grep("persönlichkeitsstörung",s))>=1&length(grep("möglicherweise organisch",s))>=1 ){
    (s<-"Organische Persönlichkeitsstörung")
  }
   
   
   
  ### posttraumatisch 
  if (length(grep("ptbs",s))>=1){
    (s<-"posttraumatische Belastungsstörung")
  }
   if (length(grep("posttraumatische belastungsstörung",s))>=1){
     (s<-"Posttraumatische Belastungsstörung")
   }
   
  
  if (length(grep("andauernde persönlichkeitsänderung *.* posttraumatischen verbitterungsstörung",s))==1 ){
    (s<-"Andauernde Persönlichkeitsänderung, nicht näher bezeichnet")
  }
 if (length(grep("andauernde persönlichkeitsänderung nach traumatisierung",s))==1 ){
    (s<-"Andauernde Persönlichkeitsänderung nach Extrembelastung")
  }    
  if (length(grep("persönlichkeitsstörung",s))==1 &length(grep("emotional instabile*.",s))==1
      &length(grep("impulsive*.",s))==0 &length(grep("borderline*.",s))==0 &length(grep("histrionischen*.",s))==0 ){
    (s<-"Emotional instabile Persönlichkeitsstörung")
  }else if (length(grep("persönlichkeitsstörung mit emotional instabilen und histrionischen",s))==1 ){
    (s<-"Emotional instabile Persönlichkeitsstörung, Impulsiver Typ")
  }else if (length(grep("persönlichkeitsstörung",s))==1 &length(grep("emotional instabile*.",s))==1){
     (s<-"Emotional instabile Persönlichkeitsstörung")
  }
   if (length(grep("persönlichkeit*.",s))==1 &length(grep("emotional instabile*.",s))==1 &length(grep("impulsiv*.",s))==1 ){
     (s<-"Emotional instabile Persönlichkeitsstörung, Impulsiver Typ")
   } 
   
  (s<-gsub("vermeidende zwanghafte züge","Anankastische [zwanghafte] Persönlichkeitsstörung", s))
     
  if (length(grep("passiv-aggressive",s))==1 &length(grep("persönlichkeitsstörung",s))==1 ){
    (s<-"Sonstige spezifische Persönlichkeitsstörungen")
  }
  (s<-gsub("subsyndromal","leichte episode",s))
  (s<-gsub("konversionsneurotische dissoziative störung","Dissoziative Störungen [Konversionsstörungen]",s))
  (s<-gsub("posttraumatische*.* verbitterungsstörung", "Sonstige Reaktionen auf schwere Belastung",s))  
   (s<-gsub("verbitterungssyndrom", "Sonstige Reaktionen auf schwere Belastung",s))  
   
   if (length(grep("borderline",s))==1 & length(grep("persönlichkeitsstörung",s))==1 ){
     (s<-"Emotional instabile Persönlichkeitsstörung: Borderline-Typ")
   }  
   if (length(grep("narzisstische",s))==1 & length(grep("persönlichkeit*.",s))==1 ){
     (s<-"Sonstige spezifische Persönlichkeitsstörungen")
   }  
  #### angst
  s<-gsub("leichte agoraphobie","Agoraphobie",s)
  if (length(grep("tunnel|höhen",s))==1 ){
    (s<-"Spezifische (isolierte) Phobien")
  }
  if (length(grep("photophobie",s))==1 ){
    (s<-"Spezifische (isolierte) Phobien")
  }
  if (length(grep("spezifische phobie",s))==1 ){
    (s<-"Spezifische (isolierte) Phobien")
  }
  
  if (length(grep("soziale phobie",s))==1 ){
    (s<-"Soziale Phobien")
  }
  
  
  (s<-gsub("sepzifische phobien", "Spezifische (isolierte) Phobien",s))  
  (s<-gsub("spezifische phobie", "Spezifische (isolierte) Phobien",s))  
  (s<-gsub("phobie vor geschlossenen und/oder fensterlosen räumen", "Spezifische (isolierte) Phobien",s))  
 
  (s<-gsub("phobie und panikstörung", "Phobische Störung, nicht näher bezeichnet",s))  
  
  (s<-gsub("panikstörung und schwere*. depressive.* episode", "Angst und depressive Störung, gemischt",s))
  
  if (length(grep("angst",s))==1 & length(grep("und",s))==1&length(grep("depressi*.",s))==1 ){
    (s<-"Angst und depressive Störung, gemischt")
  }
  
  (s<-gsub("angst und depressive episode gemischt", "Angst und depressive Störung, gemischt",s))
  (s<-gsub("angst und depression gemischt", "Angst und depressive Störung, gemischt",s))
  (s<-gsub("ängstlich-depressive entwicklung", "Angst und depressive Störung, gemischt",s))
  (s<-gsub("ängstlich-depressive störung", "Angst und depressive Störung, gemischt",s))
  (s<-gsub("angststörung und emotional labile störung", "Angst und depressive Störung, gemischt",s))  
  (s<-gsub("leichte phobie", "Phobische Störungen",s))  
  (s<-gsub("agoraphobie mit schwerer panikstörung", "Agoraphobie mit Panikstörung",s))  
  (s<-gsub("sepzifische phobien", "Spezifische (isolierte) Phobien",s))  
    if (s=="agoraphobie und panikstörung"){
      (s<-"Agoraphobie mit Panikstörung")
    }
  if (length(grep("agoraphobie",s))>=1& length(grep("ohne panikstörung",s))>=1) {
    (s<-"Agoraphobie Ohne Angabe einer Panikstörung")
  }
  
  (s<-gsub(".*schwere panikstörung", "Panikstörung [episodisch paroxysmale Angst]",s))
  (s<-gsub("mittelgradige panikstörung", "Panikstörung [episodisch paroxysmale Angst]",s))
  if(s=="panikanfälle"){
    s<-"Angststörung, nicht näher bezeichnet"
  }
  
    if (s=="agoraphobie ohne panikstörung"){
      (s<-"Agoraphobie ohne Angabe einer Panikstörung")
     }
    (s<-gsub("rezidivierenden panikattacken",
             "Panikstörung [episodisch paroxysmale Angst]",s))
    (s<-gsub("rezidivierende panikattacken",
           "Panikstörung [episodisch paroxysmale Angst]",s))
    (s<-gsub("paniforme angstattacken",
             "Panikstörung [episodisch paroxysmale Angst]",s))
     (s<-gsub("leichtgradige generalisierte angststörung",
             "generalisierte Angststörung",s))
  (s<-gsub("angststörung mit panikattacken",
           "Panikstörung [episodisch paroxysmale Angst]",s))
  (s<-gsub("begleitende angststörung",
           "angststörung, nicht näher bezeichnet",s))
  (s<-gsub("angststörung mit anankastischen zügen",
           "sonstige spezifische angststörungen",s))
  if(s=="ausgeprägte angststörung"){
    (s<-gsub("ausgeprägte angststörung","Andere Angststörungen",s))
  }
  if( length(grep("angststörung*.",s))>=1 &length(grep("panik*.",s))>=1){
    (s<-"Panikstörung [episodisch paroxysmale Angst]")
  }

  ### IQ /schizo /attention
  (s<-gsub( "leichte geistige behinderung", "Leichte Intelligenzminderung",s))
  (s<-gsub( "leichte intellektuelle beeinträchtigung", "Leichte Intelligenzminderung",s))
  
  (s<-gsub("schizoaffektive störung, gegenwärtig remittiert", "Gemischte schizoaffektive Störung", s))
  if (length(grep("verminderte mentale leistungsfähigkeit",s))==1){
    s<-"Leichte kognitive Störung"
  }
  (s<-gsub("neuropsychologische minderleistung", "Leichte kognitive Störung", s))
  if (length(grep("intelligenzminderung",s))==1){
    s<-"Leichte Intelligenzminderung"
  }
  if (length(grep("leichte minderintelligenz",s))==1){
    s<-"Leichte Intelligenzminderung"
  }
  if (length(grep("minderintelligenz",s))==1){
    s<-"Leichte Intelligenzminderung"
  }
  if (length(grep("leichte kognitive beeinträchtigung",s))==1){
    s<-"Leichte kognitive Störung"
  }
  if (length(grep("leichte kognitive störung",s))>=1){
    s<-"Leichte kognitive Störung"
  }
  if (length(grep("kognitive beeinträchtigung",s))==1){
    s<-"Leichte kognitive Störung"
  }
  
  if (length(grep("chronische paranoide schizophrenie",s))==1){
    s<-"paranoide Schizophrenie"
  }
  if (length(grep("\\bchron\\w*\\b \\bparanoid\\w*\\b .*. \\bschizophrenie\\w*\\b",s))==1){
    s<-"paranoide Schizophrenie"
  }
  if (length(grep("kontinuierliche paranoide schizophrenie",s))==1){
    s<-"paranoide Schizophrenie"
  }
  if (length(grep("zönästhetische schizophrenie",s))==1){
    s<-"Sonstige Schizophrenie"
  }
  
  if (length(grep("anpassungsstörung",s))==1){
    s<-"Anpassungsstörungen"
  }
  
  if(length(grep("adhs",s)>=1)  ){
    s<-"Aktivitäts- und Aufmerksamkeitsstörung"
  }
  if(length(grep("aufmerksamkeitsdefizit",s)>=1)  ){
    s<-"Aktivitäts- und Aufmerksamkeitsstörung"
  }
     if(length(grep("dissoziative bewegungsstörung",s)>=1)  ){
    s<-"Dissoziative Bewegungsstörungen"
  }
  if(length(grep("persönlichkeitsänderung nach schweren verletzungen bei polytrauma mit verminderung der körperlichen integrität und chronischen schmerzen",s)>=1)  ){
    s<-"Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  (s<-gsub("\\bdekompensierte\\w*\\b", "", s))
  
  if(length(grep("chronische rezidivierende depressive störung, gegenwärtig leichte episode",s))>=1){
    s<-"Rezidivierende depressive Störung, gegenwärtig leichte Episode"
  }
  if(length(grep("depressive episode",s))>=1 & length(grep("chronisch*.",s))>=1){
  (s<- gsub("\\bchronisch\\w*\\b", "",s))
  }
  if(length(grep("schizoaffektive",s))>=1){
    (s<-gsub("chronische","",s))
  }
  
  (s<-gsub("schizoaffektive psychose", "Schizoaffektive Störung", s))
  (s<-gsub("dekompensierte", "", s))
 
  ### Verhaltens- und emotionale Störungen mit Beginn in der Kindheit und Jugend
  
   if(length(grep("hyperkinetische störung*.",s))>=1){
    (s<-"Hyperkinetische Störungen")
  }
  
  (s<-gsub("unzulängliche soziale fähigkeiten", "Störung des Sozialverhaltens, nicht näher bezeichnet",s))
  
  if(length(grep("analphabet*.",s))>=1){
    (s<-"Kontaktanlässe mit Bezug auf die Ausbildung")
  }
  if(length(grep("emotionale \\bstörung\\w*\\b des kindesalters",s))>=1){
    (s<-"Emotionale Störungen des Kindesalters")
  }
  
  
  
  
  ### somatoform
  (s<-gsub("leichte anhaltende somatoforme schmerzstörung","anhaltende somatoforme Schmerzstörung",s)) 
  if(length(grep("somatoforme autonome funktionsstörung",s)>=1)  ){
    s<-"Somatoforme autonome Funktionsstörung"
  }
  if(length(grep("somatoforme störung",s))>=1 &length(grep("nicht näher*. bezeichnet",s))>=1) {
    s<-"Somatoforme Störung, nicht näher bezeichnet"
  }
  if(length(grep("anhaltende somatoforme schmerzstörung",s))>=1) {
    s<-"Anhaltende somatoforme Schmerzstörung"
  }
  s<-gsub("chronische schmerzstörung mit psychischen und somatischen faktoren",
        "Chronische Schmerzstörung mit somatischen und psychischen Faktoren",s)
  
  #### varia /not F specific
  (s<-gsub("komplexe psychische störung mit depressiven neurasthenischen und hypochondrischen anteilen",
           "Sonstige Reaktionen auf schwere Belastung",s))
  
  
  if(length(grep("neurasthenie",s))>=1) {
    s<-"Neurasthenie"
  }
  (s<-gsub("polytrauma" , "Nicht näher bezeichnete multiple Verletzungen", s))
  (s<-gsub("chronische insomnie" , "Nichtorganische Insomnie", s))
  (s<-gsub("trancezustände" , "Trance- und Besessenheitszustände", s))
  
  if(length(grep("dissoziative störung",s))>=1 &length(grep("bewegung",s))>=1) {
    (s<-"Dissoziative Bewegungsstörungen")
  }
   
  (s<-gsub("pathologischem glückspiel" , "Sonstige Probleme mit Bezug auf die Lebensführung", s))
  (s<-gsub("ausgeprägte frühverwahrlosung" , "Sonstige Probleme mit Bezug auf die Lebensführung", s))
  (s<-gsub("bulimische phasen", "Bulimia nervosa", s))
  (s<-gsub("dissoziative orientierungsstörung", "Orientierungsstörung, nicht näher bezeichnet", s))
  (s<-gsub("komplexe", "",s))
  (s<-gsub("mit beginn in der jugend", "",s))
  (s<-gsub("störung des sozialverhaltens", "Störungen des Sozialverhaltens",s ))
  
  if(length(grep("anorexia",s))>=1){
    (s<-gsub("chronische ", "", s))
  }
  if(length(grep("komplizierte",s))>=1 & length(grep("trauer*.",s))>=1){
    (s<-"Anpassungsstörungen" )
  }
  if(length(grep("lese- und rechtschreib*.",s))>=1){
    (s<-"Lese- und Rechtschreibstörung")
    
  }
  (s<-gsub("demenziellen prozess","Nicht näher bezeichnete Demenz" ,s))
  (s<-gsub("zwangsgedanken und zwangshandlungen gemischt","Zwangsgedanken und -handlungen, gemischt" ,s))
  (s<-gsub("probleme in der primären bezugsgruppe","Andere Kontaktanlässe mit Bezug auf den engeren Familienkreis" ,s))
  (s<-gsub("nachhältige prägung nach sexuellen übergriffen","Posttraumatische Belastungsstörung" ,s))
  
  (s<-gsub("störung sozialer funktionen mit beginn im jugendalter" , "Sonstige Störungen sozialer Funktionen mit Beginn in der Kindheit" ,s))
  if(length(grep("crps",s))>=1){
    (s<-"Komplexes regionales Schmerzsyndrom, sonstiger und nicht näher bezeichneter Typ" )
  }
  (s<-gsub("näher bezeichnete probleme verbunden mit der sozialen umgebung",
           "Kontaktanlässe mit Bezug auf die soziale Umgebung" ,s))
  (s<-gsub("neurotische störung mit ausgeprägtem leistungsanspruch",
           "Sonstige neurotische Störungen" ,s))
  (s<-gsub("psychosoziale belastungssituation",
           "Akute Belastungsreaktion" ,s))
  if(length(grep("probleme mit bezug auf den engeren familienkreis",s))>=1){
    (s<-"Andere Kontaktanlässe mit Bezug auf den engeren Familienkreis" )
  }
  if(length(grep("andere affektive störung",s))>=1){
  (s<-"Andere affektive Störungen")
  }
  
  
  return(s)
}

