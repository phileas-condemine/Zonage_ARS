gen_table_recap_dt = function(input,
                              session,
                              vals_reac,
                              VZN_reac,
                              info_recap_reac,
                              tableau_reg,
                              hist_qpv,
                              zonage_qpv,
                              communes_TVS = NULL,
                              communes_BVCV = NULL) {
  message("func : gen_table_recap_dt")
  
  infos <- vals_reac()
  req(!is.null(infos))
  if(input$choix_ps=="mg"){
    #### TVS
    load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
    infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
    infos <- merge(infos, unique(tableau_reg[,.(agr,is_majoritaire,en_vigueur_autre_reg,CN)]),by="agr")
    setnames(infos,"my_reg_TVS","dansMaRegion")
    infos = data.table(infos)
    
    infos = merge(infos,VZN_reac()[,.SD[1],by="tvs"],by.x="agr",by.y="tvs",all.x=T)
    infos = infos[(zonage_ars!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
    
    #### QPV
    
    infos_zonage_qpv = merge(hist_qpv[,c("cod","libqpv","agr","pop")],zonage_qpv,by="cod")
    setnames(infos_zonage_qpv,"picked_zonage","picked_zonage_qpv")
    infos_zonage_qpv = merge(infos_zonage_qpv,unique(tableau_reg[,.(agr,CN)]),by="agr")
    infos_zonage_qpv = merge(infos_zonage_qpv,vals_reac(),by="agr")
    
    infos_zonage_qpv = infos_zonage_qpv[picked_zonage_qpv!=picked_zonage,c("libqpv","cod","picked_zonage_qpv","picked_zonage","pop")]
    names(infos_zonage_qpv) <- c("libagr","agr","picked_zonage","CN","population")
    infos_zonage_qpv$CN = paste0("TVS : ",infos_zonage_qpv$CN)
    if(nrow(infos_zonage_qpv)>0){
      infos = rbind(infos,infos_zonage_qpv)
    }
    
    
    
  } else if (input$choix_ps %in% c("sf","inf")){
    load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
    infos <- merge(infos,communes_BVCV[,c("agr","libagr","libcom","depcom","my_reg_BVCV","population")],by="agr")
    infos <- merge(infos, unique(tableau_reg[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg,CN)]),by="agr")
    # infos$echangeable = infos$ZE_UD + infos$ZE_OD
    # infos$enVigueurAutreReg = !is.na(infos$en_vigueur_autre_reg)
    # setnames(infos,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
    infos = data.table(infos)
    infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
    
  }
  
  
  if (nrow(infos)>0){
    info_recap_reac(copy(infos))
    removeNotification("justification_form_available",session)
    showNotification("Le bouton \"Justification du zonage pris\" vous permet d'ajouter des commentaires à l'attention de la DGOS et de la CNAM pour étayer vos choix de zonage.",
                     duration = NULL,id = "justification_form_available",type = "message",session=session)
  }
  
  
  # infos = data.table(infos)
  # infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
  setnames(infos,c("libagr","agr","picked_zonage","CN","population"),c("Libelle","Code","Zonage","Cadre National","Population"))
  if(nrow(infos)>0){
    datatable(infos,
              rownames=F,
              options=list(
                dom = "t",
                pageLength = -1
              ))
  } else NULL
}