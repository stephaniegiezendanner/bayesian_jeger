clean_icd_codes <- function(s, search_terms) {
  s<-trimws(s)
  (s<-gsub("\\(.*?\\)", "", s))
  (s<-gsub("nach Autounfall am 10.01.2010", "",s))
  (s<-gsub("chron ","chronische ",s)   )
  (s<-gsub("somat ","somatischen ",s)   )
  (s<-gsub("som ","somatischen ",s)   )
  (s<-gsub("psych ","psychischen ",s) )
  
  
  if (length(grep("PTBS",s)>=1)){
    (s<-"posttraumatische belastungsstörung")
  }
  if (length(grep("chronisch",s)>=1)&length(grep("depressiv",s)>=1)){
    (s<-gsub("chronische", "",s))
  }
  if(length(grep("somaotoforme",s)>=1)  ){
    (s<-gsub("somaotoforme","somatoforme",s))
  }
  if(s=="Persönlichkeitsänderung"){
  s<-"Andauernde Persönlichkeitsänderungen, nicht Folge einer Schädigung oder Krankheit des Gehirns"
  }
  
  (s<-gsub("rez |rez\\.","rezidivierende ",s))
  (s<-gsub("St.n.","",s))
  (s<-gsub("St. n. ","",s))
  (s<-gsub("austonome","autonome",s))
  (s<-gsub("des Urogenitaltrakts","",s))

   
  
  s<-gsub("agitierte depressive Episode","depressive Episode",s)
  (s<-gsub("im Rahmen einer Multiplen Sklerose","",s))
  (s<-gsub("kombinierte","",s))
  (s<-gsub("im Nachgang zu einem Gewaltverbrechen","",s))
  (s<-gsub("V.a.","",s))
  (s<-gsub("mittelschwere ","mittelgradige ",s) )
  (s<-gsub("leichte bis mittelgradige","leichte",s))
  (s<-gsub("leicht bis mittelgradig","leichte Episode",s))
  (s<-gsub("mittelgradig bis schwer","mittelgradig",s))
  
  
  (s<-gsub("agitierte ","",s))
  
  (s<-gsub("Depression","depressive Episode ",s))   
  (s<-gsub("remittierte depressive Reaktion",
           "Rezidivierende depressive Störung, gegenwärtig remittiert",s))
  #(s<-gsub("depressive Störung","depressive Episode",s)  ) 
  if (length(grep("atypisch",s))>=1& length(grep("depressive Störung",s))>=1){
    (s<-"Sonstige rezidivierende depressive Störungen")
  }
  
  (s<-gsub("anhaltende mitelgradige depressive Störung",
           "Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode",s))
  
  (s<-gsub("rezidivierende gegenwärtig mittelgradige bis schwere Episode",
           "Rezidivierende depressive Störung, gegenwärtig mittelgradige Episode",s))
  
  (s<-gsub("leichtgradige depressive Episode","Rezidivierende depressive Störung, gegenwärtig leichte Episode",s))
  (s<-gsub("leichte depressive Störung","Rezidivierende depressive Störung, gegenwärtig leichte Episode",s))
  
  (s<-gsub("mit Pseudodemenz","",s))
  (s<-gsub("narzisstische","Sonstige spezifische",s))   
  
  (s<-gsub("rezidivierende depressive Episode","rezidivierende depressive Störung",s))
  (s<-gsub("rezidivierende depressive Störung  gegenwärtig leichtgradig",
           "rezidivierende depressive Störung gegenwärtig leichte Episode" ,s))
  (s<-gsub("rezidivierende Depression mittelgradig",
           "rezidivierende depressive Störung, gegenwärtig mittelgradige Episode" ,s))  
  
  if(length(grep("rezidivierende Depression",s)==1) ){
    (s<-gsub("rezidivierende Depression",
             "rezidivierende depressive Störung" ,s))  
  }
  (s<-gsub("mittelgradige bis schwere", "mittelgradige",s))
  
  if(length(grep("mittelgradig",s))==1 &length(grep("gegenwärtig",s))==0 & length(grep("Störung",s))==1 ){
    (s<-"rezidivierende depressive Störung, gegenwärtig mittelgradige Episode")
  }
  
  if(length(grep("depressiver Episode",s))==1){
    (s<-gsub("mittelschwerer", "Mittelgradige",s))
  }
  if(length(grep("mit emotional instabilen Zügen vom Borderline-Typus",s))==1 ){
    (s<-"Emotional instabile Persönlichkeitsstörung")
  }
  
  (s<-gsub("leichte depressive Episode ohne somatisches Syndrom","leichte depressive Episode",s)) 
  (s<-gsub("depressive Entwicklung","depressive Störung",s))   
  (s<-gsub("Schweregrad mittelschwer","gegenwärtig mittelgradige Episode",s)   )
  (s<-gsub("Schweregrad mittel bis schwer","gegenwärtig mittelgradige Episode",s)   )
  (s<-gsub("mittel- bis schwergradige","Mittelgradige",s)   )
  
  if(length(grep("mittel- bis schwergradig",s)>=1)  ){
    (s<-gsub(" mittel- bis schwergradig"," mittelgradige Episode",s)   )
    if (length(grep("gegenwärtig",s))==0){
      (s<-gsub("Störung","Störung, gegenwärtig",s)   )
    }
  }
  
  (s<-gsub("Polytoxikomanie","multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen",s) )  
  (s<-gsub("Benzodiazepinabhängigkeit", 
           "Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika, Abhängigkeitssyndrom",s))
  (s<-gsub("alkoholbdedingte Wesensveränderung", 
           "Psychische und Verhaltensstörungen durch Alkohol",s))
  (s<-gsub("schädlicher Alkoholgebrauch", 
           "Psychische und Verhaltensstörungen durch Alkohol: Schädlicher Gebrauch",s))
 
  (s<-gsub("Alkoholabhängigkeitssyndrom gegenwärtiger Substanzgebrauch", 
           "Psychische und Verhaltensstörungen durch Alkohol: Schädlicher Gebrauch",s))
  
   (s<-gsub("Alkoholabhängigkeit gegenwärtig abstinent", 
           "Psychische und Verhaltensstörungen durch Alkohol: Abhängigkeitssyndrom",s))
  (s<-gsub("iatrogene Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika, Abhängigkeitssyndrom", 
           "Psychische und Verhaltensstörungen durch Sedativa oder Hypnotika: Abhängigkeitssyndrom",s))
  (s<-gsub("langjähriger Drogen- und Alkoholabusus", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Abhängigkeitssyndrom",s))
  
  (s<-gsub("iatrogene Opiatabhängigkeit",
           "Psychische und Verhaltensstörungen durch Opioide: Abhängigkeitssyndrom",s))
  
  (s<-gsub("psychotische Störung aufgrund von multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Psychotische Störung",s))
  (s<-gsub("psychischen Störung durch multiple Substanzen", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Psychotische Störung",s))
  (s<-gsub("psychischen Abhängigkeit", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Abhängigkeitssyndrom",s))
  (s<-gsub("multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen Restzustand mit Persönlichkeitsstörung", 
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Restzustand und verzögert auftretende psychotische Störung",s))
  
  
  
  
  if (length(grep("Opiatabhängigkeit",s))>=1){
    (s<-"Psychische und Verhaltensstörungen durch Opioide: Abhängigkeitssyndrom")
  }
   (s<-gsub("schädlicher Cannabisgebrauch","Psychische und Verhaltensstörungen durch Cannabinoide: Schädlicher Gebrauch",s))
   (s<-gsub("schädlicher Gebrauch von Cannabis","Psychische und Verhaltensstörungen durch Cannabinoide: Schädlicher Gebrauch",s))
  (s<-gsub("Cannabisabhängigkeitssyndrom","Psychische und Verhaltensstörungen durch Cannabinoide: Abhängigkeitssyndrom",s))
  
  
  
   (s<-gsub("Persönlichkeitsänderung und residualaffektives Zustandsbild bei 10-jähriger LSD-Abhängigkeit",
           "Psychische und Verhaltensstörungen durch Halluzinogene: Restzustand und verzögert auftretende psychotische Störung",s))
   (s<-gsub("Restzustand mit Persönlichkeitsstörung nach multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen",
           "Psychische und Verhaltensstörungen durch multiplen Substanzgebrauch und Konsum anderer psychotroper Substanzen: Restzustand und verzögert auftretende psychotische Störung",s))
  if(length(grep("psychologische Faktoren bei andernorts klassifizierten Krankheiten",s))>=1){
    s<-"Psychologische Faktoren oder Verhaltensfaktoren bei anderenorts klassifizierten Krankheiten"
  }
  
  
  (s<-gsub("Frontalhirnatrophie", 
           "Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns",s))
  (s<-gsub("organisches Psychosyndrom nach tramatischer Hirnverletzung", 
           "Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns",s))
  (s<-gsub("organische emotional labile Störung", 
           "Organische emotional labile [asthenische] Störung",s))
  (s<-gsub("organische depressive Störung", 
           "Organische affektive Störungen",s))
  (s<-gsub("organische depressive Störung", 
           "Organische affektive Störungen",s))

  if(length(grep("sonstige organische Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit",s))>=1){
    s<-"Sonstige organische Persönlichkeits- und Verhaltensstörungen aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  
  (s<-gsub("Trauerrekation", "Trauerreaktion",s))
  (s<-gsub("Tauerreaktion", "Trauerreaktion",s))
  
  if(length(grep("protrahierte Trauerreaktion",s))>=1){
    (s<-"Anpassungsstörungen")
  }
  (s<-gsub("Perönlichkeitszüge", "Persönlichkeitszüge",s))
  if(length(grep("akzentuierte Persönlichkeit",s))>=1){
    (s<-"Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung")
  }
  
  s<-gsub("akzentuierte Persönlichkeitszüge","Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung",s)
  s<-gsub("Schwierigikeiten mit der kulturellen Eingewöhnung","Kontaktanlässe mit Bezug auf die soziale Umgebung",s)
  
  
  s<-gsub("Essattacken bei sonstigen psychischen Störungen",
          "Essattacken bei anderen psychischen Störungen",s)
  (s<-gsub("leichte Zwangshandlungen","Zwangshandlungen",s))
  (s<-gsub("subsyndromale","",s))
  
   (s<-gsub("atypische depressive Episode ","Sonstige depressive Episoden",s))
  s<-gsub("leichte Agoraphobie","Agoraphobie",s)
  s<-gsub("DD ","",s)
  s<-gsub("Borderlinepersönlichkeit","Borderline-Typ",s)
  (s<-gsub("Depersonalisationsstörung", "Depersonalisations",s))
  (s<-gsub("sepzifische Phobien", "Spezifische (isolierte) Phobien",s))  
  if (length(grep("Tunnel|Höhen",s))==1 ){
    (s<-"Spezifische (isolierte) Phobien")
  }
  
  
  (s<-gsub("posttraumatische Verbitterungsstörung", "Sonstige Reaktionen auf schwere Belastung",s))  
  
  if (length(grep("andauernde Persönlichkeitsänderung im Sinne einer posttraumatischen Verbitterungsstörung",s))==1 ){
    (s<-"Andauernde Persönlichkeitsänderung, nicht näher bezeichnet")
  }
  if (length(grep("andauernde Persönlichkeitsänderung nach Traumatisierung",s))==1 ){
    (s<-"Andauernde Persönlichkeitsänderung nach Extrembelastung")
  }    
  if (length(grep("mittelgradige depressive Episode mit somatischem Syndrom",s))==1 ){
    (s<-"mittelgradige depressive Episode")
  }
  if (length(grep("Persönlichkeitsstörung mit emotional instabilen und histrionischen",s))==1 ){
    (s<-"Emotional instabile Persönlichkeitsstörung, Impulsiver Typ")
  }
  (s<-gsub("subsyndromal","mittelgradige Episode",s))
  
  (s<-gsub("Panikstörung und schwerer depressiver Episode", "Angst und depressive Störung, gemischt",s))  
  (s<-gsub("Angststörung und emotional labile Störung", "Angst und depressive Störung, gemischt",s))  
  
  
  (s<-gsub("leichte Phobie", "Phobische Störungen",s))  
  
  
  (s<-gsub("remittierte depressive Episode","Rezidivierende depressive Störung, gegenwärtig remittiert", s))
  if(length(grep("remittiert",s))>=1 &length(grep("depressiv",s))>=1){
    (s<-"Rezidivierende depressive Störung, gegenwärtig remittiert")
    
  }
  
  
  (s<-gsub("dissoziative Störung gemischt","Dissoziative Störungen [Konversionsstörungen], gemischt", s))
  (s<-gsub("auffällige Persönlichkeitszüge","Probleme mit Bezug auf Schwierigkeiten bei der Lebensbewältigung", s))
  (s<-gsub("Persönlichkeitsänderung bei Epilepsie und depressiver Entwicklung",
           "Andauernde Persönlichkeitsänderung, nicht näher bezeichnet",s))
  (s<-gsub("Persönlichkeits- und Verhaltensstörung aufgrund einer Erkrankung des Gehirns",
           "Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns",s))
  
  
  if (s=="Agoraphobie und Panikstörung"){
    (s<-"Agoraphobie mit Panikstörung")
  }
  (s<-gsub("rezidivierenden Panikattacken",
           "Panikstörung [episodisch paroxysmale Angst]",s))
  (s<-gsub("paniforme Angstattacken",
           "Panikstörung [episodisch paroxysmale Angst]",s))
  
  
  
  
  if (length(grep("leichte depressive Episode",search_terms))==1&length(grep("begleitende Angststörung",s))==1 ){
    (s<-gsub("begleitende Angststörung", "leichte depressive Episode",s))
  }
  
  if (length(grep("gegenwärtig",s))==1&length(grep(",",s))==0 ){
    (s<-gsub(" gegenwärtig", ", gegenwärtig",s))
  }
  if (length(grep("rezidivierende depressive Störung",s))==1){
    (s<-gsub("leichte depressive Episode",
             "leichte Episode",s))  
  }
  
  
  if (length(grep("verminderte mentale Leistungsfähigkeit",s))==1){
    s<-"Leichte kognitive Störung"
  }
  if (length(grep("chronische paranoide Schizophrenie",s))==1){
    s<-"paranoide Schizophrenie"
  }
  if (length(grep("zönästhetische Schizophrenie",s))==1){
    s<-"Sonstige Schizophrenie"
  }
  
  if (length(grep("Anpassungsstörung",s))==1){
    s<-"Anpassungsstörungen"
  }
  
  
  
  
  if(length(grep("ADHS",s)>=1)  ){
    s<-"Aktivitäts- und Aufmerksamkeitsstörung"
  }
  if(length(grep("remittierte",s))>=1 & length(grep("gegenwärtig",s))==0 ){
    s<-gsub("remittierte","",s)   
    s<-paste0(s,", gegenwärtig remittiert")
  }
  if(length(grep("dissoziative Bewegungsstörung",s)>=1)  ){
    s<-"Dissoziative Bewegungsstörungen"
  }
  if(length(grep("Persönlichkeitsänderung nach schweren Verletzungen bei Polytrauma mit Verminderung der körperlichen Integrität und chronischen Schmerzen",s)>=1)  ){
    s<-"Persönlichkeits- und Verhaltensstörung aufgrund einer Krankheit, Schädigung oder Funktionsstörung des Gehirns"
  }
  (s<-gsub("dekompensierte", "", s))
  
  if(length(grep("Chronische Rezidivierende depressive Störung, gegenwärtig leichte Episode",s))>=1){
    s<-"Rezidivierende depressive Störung, gegenwärtig leichte Episode"
  }
  (s<-gsub("atyp, ", "",s))
  
  return(s)
}