# my_ps = "sf";my_reg = 24
# my_ps = "inf";my_reg = 24
# load(sprintf("data/%s_preprocessed_BVCV.RData",my_reg))


prep_zonage_hors_mg <- function(
  session,
  my_reg,
  my_ps,
  dropbox_folder,
  dropbox_ps_folder,
  my_dropbox_files,
  choix_mil,
  params,
  VZN_reac,
  pop_femmes,
  communes_AGR,
  zonages_en_vigueur){
  message("func : prep_zonage_hors_mg")
  
  if(my_ps=="sf"){
    filename = params[file=="zonage_sf"]$name
  } else if (my_ps =="inf"){
    filename = params[file=="zonage_inf"]$name
  }
  drop_download(path = paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
  
  zonage_historique=sas7bdat::read.sas7bdat(paste0("data/cadre_nat_",my_ps,".sas7bdat"))
  zonage_historique$zonage_nat=factor(zonage_historique$zonage_nat)
  
  # table(zonage_historique$ZE_UD)
  # table(zonage_historique$ZE_OD)
  
  levels(zonage_historique$zonage_nat) <- c("VUD","UD","Int","VD","OD")
  zonage_historique=zonage_historique%>%
    mutate_if(is.factor,as.character)
  zonage_historique=data.table(zonage_historique)
  zonage_historique=unique(zonage_historique)
  
  if (my_ps=="inf"){
    zonage_historique[,ZE_UD:=gsub("-","0",ZE_UD)]
    zonage_historique[,ZE_OD:=gsub("-","0",ZE_OD)]
    
    zonage_historique[,ZE_UD:=as.numeric(ZE_UD)]
    zonage_historique[,ZE_OD:=as.numeric(ZE_OD)]
  }
  
  setnames(zonage_historique,"apl_2017_bvcv","apl")
  zonage_historique[,apl:=round(apl,1)]
  
  zonage_historique=zonage_historique[,c("bvcv","zonage_nat","ZE_UD","ZE_OD","apl")]
  setnames(zonage_historique,"zonage_nat","CN")
  zonage_historique=unique(zonage_historique)
  zonage_historique$bvcv = stringi::stri_pad_right(zonage_historique$bvcv,5,"_")
  cadre_national = zonage_historique[,c("bvcv","CN","ZE_UD","ZE_OD","apl")]%>%unique
  vals_zonage_historique = zonage_historique[,c("bvcv","CN")]
  
  
  
  bvcv=data.table(communes_AGR)
  # uniqueN(bvcv$agr)
  # fix à insérer dans le handle geo data parce que TVS : reg is numeric vs BVCV : reg is character de taille 2.
  bvcv[,reg:=as.numeric(reg)]
  if(my_ps == "sf"){
    print("hack pop sf")
    bvcv[,population:=NULL]
    print(paste0("merge OK: ",round(100*mean(bvcv$depcom %in% pop_femmes$CODGEO)),"%"))
    bvcv = merge(bvcv,pop_femmes,by.x="depcom",by.y="CODGEO",all.x=T)
  }
  bvcv[,"pop_bvcv_per_reg":=.(sum(population)),by=c("agr","reg")]
  setorder(bvcv,-pop_bvcv_per_reg)
  bvcv[libcom==libagr,`:=`(libcom=paste0("<b>",libcom,"</b>"),main_com=1)]
  bvcv[is.na(main_com),main_com:=0]
  setorder(bvcv,-main_com)
  bvcv=bvcv[,list(departements=paste(unique(dep),collapse=", "),
                  regions=paste(unique(reg),collapse=", "),
                  communes=paste(unique(libcom),collapse=", "),
                  communes_codes=paste(unique(depcom), collapse=", "),
                  proportion_pop=round(100*sum(population[my_reg_BVCV])/sum(population),1),
                  population=sum(population),#toutes les communes du BVCV yc celles des régions voisines
                  nombre_regions=uniqueN(reg),
                  reg_majoritaire=reg[1]),
            by=c("agr","libagr")]
  
  bvcv[,is_majoritaire:=reg_majoritaire==my_reg]
  
  bvcv=merge(bvcv,cadre_national,
             by.x=c("agr"),by.y=c("bvcv"),all.x=T)    
  
  not_merged = bvcv[is.na(CN)]
  if(nrow(not_merged)==1){
    showNotification(
      sprintf("Attention, le BVCV de %s, n° %s n'a pas été trouvé dans le fichier cadre national, aucune valeur par défaut n'a été attribuée ! Merci de veillez à attribuer un zonage à ce BVCV.",
              not_merged$libagr,not_merged$agr),
      duration = NULL,type = "error",session = session)
  } else if (nrow(not_merged)>1){
    
    showNotification(
      sprintf("Attention, les BVCV de %s n'ont pas été trouvés dans le fichier cadre national, aucune valeur par défaut n'a été attribuée ! Merci de veillez à attribuer un zonage à ces BVCV.",
              paste(paste0(not_merged$libagr," (",not_merged$agr,")"),collapse=", ")),
      duration = NULL,type = "error",session = session)
    
  }
  
  radio_buttons=expand.grid(agr=bvcv$agr,
                            statut=c("VUD","UD","Int","VD","OD"),stringsAsFactors = F)%>%data.table
  radio_buttons=merge(radio_buttons,
                      bvcv[,c("agr","is_majoritaire","CN","ZE_UD","ZE_OD","reg_majoritaire")],
                      by="agr")
  
  
  vals_zonage_historique$check_historique=T
  radio_buttons=merge(radio_buttons,vals_zonage_historique,
                      by.x=c("agr","statut"),by.y=c("bvcv","CN"),all.x=T)
  # radio_buttons[(check_historique)]
  # radio_buttons=data.table(radio_buttons)
  radio_buttons[is.na(check_historique),check_historique:=F]
  
  # Sages-femmes
  # Seules zones échangeables : ZE_UD / ZE_OD = 1
  # Bloquer zones très sous-dotées
  # ZE_UD = 1 pour toutes les zones sous-dotées et certaines zones intermédiaires 
  # --> mettre alerte quand échange, pour dire qu'il faut faire un échange réciproque 
  # ZE_OD = 1 pour certaines zones très dotées et toutes les zones surdotées
  # --> mettre alerte quand échange, pour dire qu'il faut faire un échange réciproque 
  # Finalement, bloquer toutes les zones pour lesquelles ZE_UD = 0 ET ZE_OD = 0 (mais ne suffit pas)
  # ... à la réflexion, ne pas mettre d'alerte sinon ça va être trop chiant pour les ARS
  # Peut-être qu'un "showNotification" serait un bon compromis.
  print("millesime existant ?")
  print(choix_mil)
  print(choix_mil%in%my_dropbox_files$name)
  print("available")
  print(my_dropbox_files$name)
  if(!choix_mil%in%my_dropbox_files$name){
    print("from default values")
    radio_buttons$value_set = F
    
    vals=data.table(vals_zonage_historique[,c("bvcv","CN")])
    setnames(vals,c("bvcv","CN"),c("agr","picked_zonage"))
    
    setorder(vals,agr)
    if(choix_mil==""){
      filename=paste0(my_ps,"_",my_reg,"_cadre_national.csv")
      local_name=paste0("data/",filename)
    } else {
      filename=choix_mil
      local_name=paste0("data/",filename)
      
    }
    fwrite(unique(vals),file=local_name)
    
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
    
    # assign("vals",vals,env)
    
    
    
  } else {
    print("using historical data")
    print("choix_mil2") ; print(choix_mil)
    zonage_saved <- NULL
    attempt <- 1
    while( is.null(zonage_saved) && attempt <= 10 ) {
      print(paste("try read sheet in dropbox:",attempt))
      attempt <- attempt + 1
      try({
        drop_download(paste0(dropbox_ps_folder,choix_mil),local_path = "data/",overwrite = T,verbose = T)
        print(list.files("data/"))
        zonage_saved <- fread(paste0("data/",choix_mil),colClasses = "character")%>%as.data.frame()
      })
      if(is.null(zonage_saved)){
        Sys.sleep(0.5)
      }
    } 
    if(is.null(zonage_saved)){
      showNotification(session=session,"Impossible de récupérer les données, merci de vous reconnecter dans 1 minute. Si le problème persiste, envoyez-nous un message.")
      stop("Impossible de récupérer les fichiers sur dropox")
    }
    zonage_saved = zonage_saved%>%
      mutate_all(as.character)%>%
      mutate(agr=stringi::stri_pad_right(agr,5,"_"))
    # assign("vals",zonage_saved,env)
    vals <- zonage_saved
    
    zonage_saved$value_set=T
    
    radio_buttons=merge(radio_buttons,zonage_saved,
                        by.x=c("agr","statut"),
                        by.y=c("agr","picked_zonage"),all.x=T)
    radio_buttons=data.table(radio_buttons)
    radio_buttons[is.na(value_set),value_set:=F]
    
    
  }
  zonages_en_vigueur$value_set_en_vigueur = T
  radio_buttons = merge(radio_buttons, zonages_en_vigueur,
                        by.x=c("agr","reg_majoritaire","statut"),by.y=c("agr","reg","en_vigueur_autre_reg"),all.x=T)
  # désactiver la valeur historique pour aller directement appliquer celle du zonage "en vigueur"
  radio_buttons[!(is_majoritaire),zonage_en_vigueur:=sum(!is.na(value_set_en_vigueur))>0,by="agr"]
  radio_buttons[(zonage_en_vigueur),value_set:=F]
  # si aucun zonage en vigueur et zone d'échange, on n'utilise pas le cadre nationale mais la recommandation de la FAQ : Int temporaire
  # radio_buttons[!(zonage_en_vigueur)&(ZE_OD==1|ZE_UD==1),`:=`(value_set=F,check_historique=F,value_provisoire_mino=T)]
  radio_buttons[!(zonage_en_vigueur)&(ZE_OD==1|ZE_UD==1)&!(is_majoritaire),`:=`(value_set=F,check_historique=F,value_provisoire_mino=T)]
  # radio_buttons[!(zonage_en_vigueur)&!(is_majoritaire)&(statut=="Int")&(ZE_OD==1|ZE_UD==1),value_provisoire_mino:=T]
  radio_buttons[(zonage_en_vigueur),check_historique:=F]
  #Pour différencier le cas où la valeur a déjà été remplie (ancienne valeur) ou non.
  radio_buttons[,value_is_set:=sum(value_set)>0,by="agr"]
  
  if (my_ps == "sf"){
    ps_ZE_UD = c("Int","UD")
    ps_ZE_OD = c("OD","VD")
  } else if (my_ps == "inf"){
    ps_ZE_UD = c("VUD","UD")
    ps_ZE_OD = c("OD","VD")
  }
  radio_buttons[,class:=paste0(
    ifelse(check_historique|(CN==statut)," historical_choice",""),
    ifelse(value_set," saved_choice",""),
    ifelse(is.na(value_provisoire_mino),"",ifelse(value_provisoire_mino&statut=="Int"," temp_minoritaire","")),
    ifelse(is.na(value_set_en_vigueur),"",ifelse(value_set_en_vigueur," en_vigueur_choice",""))
  )]
  
  radio_buttons[,checked:=
                  ifelse(check_historique|(CN==statut),ifelse(value_is_set,F,T),F)+
                  ifelse(value_set,T,F)+
                  ifelse(is.na(value_provisoire_mino),F,ifelse(value_provisoire_mino&statut=="Int",T,F))+
                  ifelse(is.na(value_set_en_vigueur),F,ifelse(value_set_en_vigueur,T,F))
  ]
  
  radio_buttons[,extra:=paste0(
    ifelse(!is_majoritaire," disabled='disabled'",""),
    ifelse(is.na(value_provisoire_mino),"",ifelse(value_provisoire_mino&statut=="Int"," title='FaQ: Intermédiaire si ARS majoritaire doit encore saisir son zonage'","")),
    ifelse(is.na(CN)|(ZE_OD==1&statut%in%ps_ZE_OD)|(ZE_UD==1&statut%in%ps_ZE_UD),""," disabled='disabled'")
  )]
  
  
  radio_buttons[,html:=sprintf(
    "<input type='radio' name='%s' value='%s' class='zonage_radio_button%s'%s%s/>",
    agr,
    statut,
    class,
    ifelse(checked>0," checked='checked'",""),
    extra)]
  
  # none_check = radio_buttons[,.(checked=sum(checked)),by="agr"][checked==0]
  # if(nrow(none_check)>0){
  #   showNotification(session=session,sprintf("Aucune case n'est cochée pour les BVCV suivants : %s. Merci de veiller à renseigner le zonage pour ces zones.",paste(none_check$agr,collapse=", ")),duration = NULL,type = "error",closeButton = T)
  # }
  
  # radio_buttons[,html:=sprintf(
  #   "<input type='radio' name='%s' value='%s' %sclass='zonage_radio_button%s%s%s%s'%s/>",
  #   agr,
  #   statut,
  #   ifelse(!is_majoritaire,"disabled='disabled'",""),
  #   ifelse(check_historique|(CN==statut),ifelse(value_is_set," historical_choice",
  #                                  " historical_choice' checked='checked"),""),
  #   ifelse(value_set," saved_choice' checked='checked",""),
  #   ifelse(is.na(value_provisoire_mino),"",ifelse(value_provisoire_mino&statut=="Int"," temp_minoritaire' title='FaQ: Intermédiaire si ARS majoritaire doit encore saisir son zonage' checked='checked","")),
  #   ifelse(is.na(value_set_en_vigueur),"",ifelse(value_set_en_vigueur," en_vigueur_choice' checked='checked","")),
  #   ifelse((ZE_OD==1&statut%in%ps_ZE_OD)|(ZE_UD==1&statut%in%ps_ZE_UD),""," disabled='disabled'")
  #   
  # )]
  
  
  # print("radio_buttons3") ; print(head(radio_buttons))
  
  radio_buttons=dcast(radio_buttons,agr~statut,value.var="html")
  radio_buttons = radio_buttons[,c("agr","VUD","UD","Int","VD","OD"),with=F]
  bvcv=merge(bvcv,radio_buttons,by="agr")
  
  bvcv[,libCN:=case_when(
    CN=="VUD"~"Très sous-doté",
    CN=="UD"~"Sous-doté",
    CN=="Int"~"Intermédiaire",
    CN=="VD"~"Très doté",
    CN=="OD"~"Sur-doté")]
  
  # bvcv$agr=factor(bvcv$agr)
  # bvcv$libagr=factor(bvcv$libagr)
  
  setorder(bvcv,-proportion_pop)
  print("names bvcv")
  print(class(bvcv))
  # assign("bvcv",bvcv,env)
  # assign("radio_buttons",radio_buttons,env)
  return(list(vals=vals,bvcv=bvcv,radio_buttons=radio_buttons))
  
}
