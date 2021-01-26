##### AGR ######
# input = list(choix_ps = "sf",choix_reg="24")



dl_zonage_en_vigueur_agr = function(ps,path,curr_reg){
  print(path)
  my_files = drop_dir(path)
  my_files = data.table(my_files)
  if(nrow(my_files)>0){
    my_files = my_files[grepl(paste0("^en_vigueur_",ps),name)]
    my_files = my_files[!grepl(paste0("^en_vigueur_qpv",ps),name)]
    if (length(my_files$name)>0){
      files = lapply(my_files$name,function(en_vigueur){
        print(en_vigueur)
        drop_path = paste0(path,en_vigueur)
        local_path = paste0("data/",en_vigueur)
        drop_download(drop_path,local_path = "data/",overwrite = T)
        infos = gsub("en_vigueur_","",en_vigueur)
        infos = gsub(".csv","",infos)
        
        ps = strsplit(infos,split = "_")[[1]][1]
        reg = strsplit(infos,split = "_")[[1]][2]
        cbind(fread(local_path,colClasses = c("agr"="character")),reg=reg)
      })
      zonages_en_vigueur = rbindlist(files)
      print("fichier de zonages en vigueur");print(head(zonages_en_vigueur))
      if (nrow(zonages_en_vigueur)>0){
        zonages_en_vigueur = zonages_en_vigueur[reg!=curr_reg]
        zonages_en_vigueur[,reg:=as.numeric(reg)]
        setnames(zonages_en_vigueur,"picked_zonage","en_vigueur_autre_reg")
      } else {
        zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
      }
    } else {
      zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
    }
  } else {
    zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
  }
  # rm(files)
  # message("il faudrait vérifier la région majoritaire pour bien choisir celle \"en vigueur\" à sélectionner, pour l'instant on prend arbitrairement la 1ère du fichier.")
  if (ps == "mg"){
    maj = tvs_reg_majoritaire
  } else if (ps %in% c("sf","inf")){
    maj = bvcv_reg_majoritaire
  }
  base::message("s'il y a plusieurs zonages en vigueur pour un même agr, on privilégie celui de la région majoritaire, sinon arbitraire")
  if(nrow(zonages_en_vigueur)>0){
    zonages_en_vigueur[maj,majoritaire:=1,on=c("agr","reg")]
    zonages_en_vigueur[is.na(majoritaire),majoritaire:=0]
    setorder(zonages_en_vigueur,-majoritaire)#majoritaire en priorité
    zonages_en_vigueur = zonages_en_vigueur[,.SD[1],by="agr"]
  }
  return(zonages_en_vigueur)
}


prepare_zonage_en_vigueur_for_export = function(en_vigueur,ps){
  
  if(nrow(en_vigueur)>0){
    en_vigueur$majoritaire=NULL
    
    if (ps == "mg"){
      maj = tvs_reg_majoritaire
    } else if (ps %in% c("sf","inf")){
      maj = bvcv_reg_majoritaire
    }
    
    en_vigueur[maj,majoritaire:="oui",on=c("agr","reg")]
    en_vigueur[is.na(majoritaire),majoritaire:="non"]
    setorder(en_vigueur,-majoritaire)#majoritaire en priorité
    en_vigueur = en_vigueur[,.SD[1],by="agr"]
    
    if (ps == "mg"){
      en_vigueur <- en_vigueur %>% 
        mutate(en_vigueur_autre_reg=case_when(
          en_vigueur_autre_reg=="Erreur TVS-COM"~"Erreur TVS-COM",
          en_vigueur_autre_reg=="HV"~"4 - Hors-vivier",
          en_vigueur_autre_reg=="Non-spécifié"~"Non-spécifié",
          en_vigueur_autre_reg=="ZV"~"3 - Zone de vigilance",
          en_vigueur_autre_reg=="ZAC"~"2- Zone d'action complémentaire",
          en_vigueur_autre_reg=="ZIP"~"1 -Zone d'intervention prioritaire"))%>%
          # mutate(reg=as.character(reg))%>%
          data.table()
      tvs = unique(TVS()[,.(reg,libreg,agr,libagr)])
      en_vigueur=merge(en_vigueur,tvs,by=c("agr","reg"),all.x=T)
      setnames(en_vigueur,"agr","TVS")
      setnames(en_vigueur,"libagr","TVS_libelle")
      setnames(en_vigueur,"majoritaire","region_majoritaire")
      en_vigueur <- en_vigueur[, c("region", "region_libelle", "TVS", "TVS_libelle", "region_majoritaire", "zonage_regional")]
      
    } else if (ps %in% c("sf","inf")){
      en_vigueur <- en_vigueur %>% mutate(en_vigueur_autre_reg=case_when(
        en_vigueur_autre_reg=="VUD"~"1 - Très sous-doté",
        en_vigueur_autre_reg=="UD"~"2 - Sous-doté",
        en_vigueur_autre_reg=="Int"~"3 - Intermédiaire",
        en_vigueur_autre_reg=="VD"~"4 - Très doté",
        en_vigueur_autre_reg=="OD"~"5 - Sur-doté"))%>%
        # mutate(reg=as.character(reg))%>%
        data.table()
      bvcv = unique(BVCV()[,.(reg,agr,libagr)])
      bvcv[,reg:=as.numeric(reg)]
      
      en_vigueur=merge(en_vigueur,bvcv,by=c("agr","reg"),all.x=T)
      en_vigueur = merge(en_vigueur,unique(TVS()[,.(reg,libreg)]),by="reg")
      setnames(en_vigueur,"agr","BVCV")
      setnames(en_vigueur,"libagr","BVCV_libelle")
    }
    setnames(en_vigueur,"reg","region")
    setnames(en_vigueur,"libreg","region_libelle")
    setnames(en_vigueur,"en_vigueur_autre_reg","zonage_regional")
    setnames(en_vigueur,"majoritaire","region_majoritaire")
    en_vigueur <- en_vigueur[, c("region", "region_libelle", "BVCV", "BVCV_libelle", "region_majoritaire", "zonage_regional")]
    en_vigueur$BVCV = gsub(x=en_vigueur$BVCV,pattern="_",replacement="")
    
    
  }
  return(en_vigueur)
}


prepare_zonage_en_vigueur_com_for_export = function(en_vigueur,ps){
 
  if(nrow(en_vigueur)>0){

    if (ps == "mg"){
      tvs = TVS()[,.(agr,depcom,libcom)]
      setnames(tvs, "agr", "TVS")
      en_vigueur <- merge(en_vigueur, tvs, by = "TVS")
      en_vigueur <- en_vigueur[, c("depcom","libcom","region", "region_libelle", "TVS", "TVS_libelle", "region_majoritaire", "zonage_regional")]

    } else if (ps %in% c("sf","inf")){
      bvcv = BVCV()[,.(agr,depcom,libcom)]
      setnames(bvcv, "agr", "BVCV")
      en_vigueur$BVCV = stringi::stri_pad_right(en_vigueur$BVCV,5,"_")
      en_vigueur <- merge(en_vigueur, bvcv, by = "BVCV")
      en_vigueur <- en_vigueur[, c("depcom","libcom","region", "region_libelle", "BVCV", "BVCV_libelle", "region_majoritaire", "zonage_regional")]
      en_vigueur$BVCV = gsub(x=en_vigueur$BVCV,pattern="_",replacement="")
      
    }
  }
  
  
  return(en_vigueur)
  
}

