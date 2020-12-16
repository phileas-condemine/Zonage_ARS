##### AGR ######
# input = list(choix_ps = "sf",choix_reg="24")



dl_zonage_en_vigueur_agr = function(ps,path,curr_reg){
  my_files = drop_dir(path)
  my_files = data.table(my_files)
  if(nrow(my_files)>0){
    my_files = my_files[grepl(paste0("^en_vigueur_",ps),name)]
    my_files = my_files[!grepl(paste0("^en_vigueur_qpv",ps),name)]
    print("récupération des fichiers de zonage en vigueur")
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
        reg_name=regions_reac()[reg%in%reg]$libreg 
        cbind(fread(local_path,colClasses = c("agr"="character")),reg=reg,reg_name=reg_name)
      })
      zonages_en_vigueur = rbindlist(files)
      print("fichier de zonages en vigueur");print(head(zonages_en_vigueur))
      if (nrow(zonages_en_vigueur)>0){
        zonages_en_vigueur = zonages_en_vigueur[reg!=curr_reg]
        zonages_en_vigueur[,reg:=as.numeric(reg)]
        setnames(zonages_en_vigueur,"picked_zonage","en_vigueur_autre_reg")
      } else {
        zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric(), regname=character())
      }
    } else {
      zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric(), regname=character())
    }
  } else {
    zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric(), regname=character())
  }
  # rm(files)
  # message("il faudrait vérifier la région majoritaire pour bien choisir celle \"en vigueur\" à sélectionner, pour l'instant on prend arbitrairement la 1ère du fichier.")
  
  message("s'il y a plusieurs zonages en vigueur pour un même agr, on privilégie celui de la région majoritaire, sinon arbitraire")
  if(nrow(zonages_en_vigueur)>0){
    
    if (ps == "mg"){
      maj = tvs_reg_majoritaire
    } else if (ps %in% c("sf","inf")){
      maj = bvcv_reg_majoritaire
    }
    
    zonages_en_vigueur[maj,majoritaire:="oui",on=c("agr","reg")]
    zonages_en_vigueur[is.na(majoritaire),majoritaire:="non"]
    setorder(zonages_en_vigueur,-majoritaire)#majoritaire en priorité
    zonages_en_vigueur = zonages_en_vigueur[,.SD[1],by="agr"]
    
    if (ps == "mg"){
      zonages_en_vigueur <- zonages_en_vigueur %>% mutate(en_vigueur_autre_reg=case_when(
        en_vigueur_autre_reg=="VUD"~"1 - Très sous-doté",
        en_vigueur_autre_reg=="UD"~"2 - Sous-doté",
        en_vigueur_autre_reg=="Int"~"3 - Intermédiaire",
        en_vigueur_autre_reg=="VD"~"4 - Très doté",
        en_vigueur_autre_reg=="OD"~"5 - Sur-doté"))
      zonages_en_vigueur=merge(zonages_en_vigueur,setnames(TVS()[,.(tv,libtv),],"tv","agr"),by="agr",all.x=T)
      setnames(zonages_en_vigueur,"agr","TVS")
      setnames(zonages_en_vigueur,"libtv","TVS_libelle")
    } else if (ps %in% c("sf","inf")){
      zonages_en_vigueur <- zonages_en_vigueur %>% mutate(en_vigueur_autre_reg=case_when(
        en_vigueur_autre_reg=="Erreur TVS-COM"~"Erreur TVS-COM",
        en_vigueur_autre_reg=="HV"~"4 - Hors-vivier",
        en_vigueur_autre_reg=="Non-spécifié"~"Non-spécifié",
        en_vigueur_autre_reg=="ZV"~"3 - Zone de vigilance",
        en_vigueur_autre_reg=="ZAC"~"2- Zone d'action complémentaire",
        en_vigueur_autre_reg=="ZIP"~"1 -Zone d'intervention prioritaire"))
      zonages_en_vigueur=merge(zonages_en_vigueur,setnames(BVCV(),"bvcv","agr"),by="agr",all.x=T)
      setnames(zonages_en_vigueur,"agr","BVCV")
      setnames(zonages_en_vigueur,"libbvcv","BVCV_libelle")
    }
    
    setnames(zonages_en_vigueur,"reg","region")
    setnames(zonages_en_vigueur,"libreg","region_libelle")
    setnames(zonages_en_vigueur,"en_vigueur_autre_reg","zonage_regional")
    
    
  }
  zonages_en_vigueur
}
