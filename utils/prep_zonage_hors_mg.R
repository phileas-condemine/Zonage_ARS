# input = list(choix_ps="sf",choix_reg=24)
# input = list(choix_ps="inf",choix_reg=24)
# load(sprintf("data/%s_preprocessed_BVCV.RData",input$choix_reg))
# my_reg = input$choix_reg
zonage_historique=sas7bdat::read.sas7bdat(paste0("data/",input$choix_ps,"/cadre_nat_",input$choix_ps,".sas7bdat"))
zonage_historique$zonage_nat=factor(zonage_historique$zonage_nat)



levels(zonage_historique$zonage_nat) <- c("VUD","UD","Int","VD","OD")
zonage_historique=zonage_historique%>%
  mutate_if(is.factor,as.character)
zonage_historique=data.table(zonage_historique)
zonage_historique=unique(zonage_historique)

if (input$choix_ps=="inf"){
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
CN = zonage_historique[,c("bvcv","CN","ZE_UD","ZE_OD","apl")]%>%unique
VZN = zonage_historique[,c("bvcv","CN")]

# zonage_historique_reg=zonage_historique[reg==my_reg,
#                                         c("bvcv","zonage_nat","ZE_UD","ZE_OD","apl")]
# setnames(zonage_historique_reg,"zonage_nat","CN")
# zonage_historique_reg=unique(zonage_historique_reg)
# zonage_historique_reg$bvcv = stringi::stri_pad_right(zonage_historique_reg$bvcv,5,"_")
# CN = zonage_historique_reg[,c("bvcv","CN","ZE_UD","ZE_OD","apl")]%>%unique
# VZN = zonage_historique_reg[,c("bvcv","CN")]


prep_zonage <- function(cadre_national=CN,
                        vals_zonage_historique=VZN,
                        my_google_files,choix_mil,env){
  bvcv=data.table(communes_BVCV)
  if(input$choix_ps == "sf"){
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
  print("millesime existant ?")
  print(choix_mil)
  print(choix_mil%in%my_google_files$name)
  print("available")
  print(my_google_files$name)
  if(!choix_mil%in%my_google_files$name){
    print("from default values")
    radio_buttons$value_set = F

    vals=data.table(vals_zonage_historique[,c("bvcv","CN")])
    setnames(vals,c("bvcv","CN"),c("agr","picked_zonage"))
    
    setorder(vals,agr)
    if(choix_mil==""){
      sheet_name=paste0(input$choix_ps,"_",input$choix_reg,"_cadre_national")
      gs_file_nm=paste0("data/",input$choix_ps,"_",input$choix_reg,"_cadre_national.csv")
    } else {
      sheet_name=choix_mil
      gs_file_nm=paste0("data/",choix_mil,".csv")
      
    }
    fwrite(vals,file=gs_file_nm)
    drive_upload(media = gs_file_nm,path = sheet_name,overwrite = T,  type = "csv")
    
    assign("vals",vals,env)
    
    
    
  } else {
    print("using historical data")
    print("choix_mil2") ; print(choix_mil)
    zonage_saved <- NULL
    attempt <- 1
    while( is.null(zonage_saved) && attempt <= 5 ) {
      print(paste("try read sheet in gs:",attempt))
      attempt <- attempt + 1
      try({
        
        drive_download(file = choix_mil,path=paste0("data/",choix_mil,".csv"),overwrite = T,type="csv")
        print(list.files("data/"))
        zonage_saved <- fread(paste0("data/",choix_mil,".csv"),colClasses = "character")%>%as_tibble()
        }
      )
    } 
    
    zonage_saved = zonage_saved%>%mutate_all(as.character)%>%
    mutate(agr=stringi::stri_pad_right(agr,5,"_"))
    assign("vals",zonage_saved,env)
    
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
  radio_buttons[!(zonage_en_vigueur)&(ZE_OD==1|ZE_UD==1),`:=`(value_set=F,check_historique=F)]
  radio_buttons[!(zonage_en_vigueur)&!(is_majoritaire)&(statut=="Int")&(ZE_OD==1|ZE_UD==1),value_provisoire_mino:=T]
  
  #Pour différencier le cas où la valeur a déjà été remplie (ancienne valeur) ou non.
  radio_buttons[,value_is_set:=sum(value_set)>0,by="agr"]
    
  if (input$choix_ps == "sf"){
    ps_ZE_UD = c("Int","UD")
    ps_ZE_OD = c("OD","VD")
  } else if (input$choix_ps == "inf"){
    ps_ZE_UD = c("VUD","UD")
    ps_ZE_OD = c("OD","VD")
  }
  

  radio_buttons[,html:=sprintf(
    "<input type='radio' name='%s' value='%s' %sclass='zonage_radio_button%s%s%s'%s/>",
    agr,
    statut,
    ifelse(!is_majoritaire,"disabled='disabled'",""),
    ifelse(check_historique,ifelse(value_is_set," historical_choice",
                                   " historical_choice' checked='checked"),""),
    ifelse(value_set," saved_choice' checked='checked",""),
    ifelse(value_provisoire_mino," temp_minoritaire' title='FaQ: Intermédiaire si ARS majoritaire doit encore saisir son zonage' checked='checked",""),
    ifelse((ZE_OD==1&statut%in%ps_ZE_OD)|(ZE_UD==1&statut%in%ps_ZE_UD),""," disabled='disabled'")
    
  )]
  

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
  assign("bvcv",bvcv,env)
  assign("radio_buttons",radio_buttons,env)
  
}
