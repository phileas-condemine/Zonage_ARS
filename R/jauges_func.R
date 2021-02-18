
gen_ui_jauges = function(input){
  message("func : gen_ui_jauges")
  req(input$choix_ps)
  if(input$choix_ps == "mg"){
    tagList(fluidRow(
      column(12,flexdashboard::gaugeOutput("threshold_ZIP",height = "auto")),
      column(12,flexdashboard::gaugeOutput("threshold_ZAC",height = "auto")),
      column(12,flexdashboard::gaugeOutput("threshold_MD",height = "auto"))))
  } else if (input$choix_ps =="sf"){
    tagList(fluidRow(
      column(12,flexdashboard::gaugeOutput("threshold_UD",height = "auto")),
      column(12,flexdashboard::gaugeOutput("threshold_OD",height = "auto"))))
  } else if (input$choix_ps =="inf"){
    tagList(fluidRow(
      column(12,flexdashboard::gaugeOutput("threshold_UD",height = "auto")),
      column(12,flexdashboard::gaugeOutput("threshold_OD",height = "auto"))))
  } else NULL
}


jauge_threshold_OD = function(input,zonage_pop_reac,regions_reac){
  message("func : jauge_threshold_OD")
  
  req(input$choix_reg)
  info_reg=regions_reac()[reg%in%input$choix_reg]
  min_val=0
  max_val=if(input$choix_ps=='sf'){info_reg$OD_sf}else if(input$choix_ps=='inf'){info_reg$OD_inf}
  print(max_val)
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="OD"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val&!input$remove_alerte_jauge)
    shinyalert(title = "Dépassement de la population en zone sur-dotée (jauges en bas à droite)",
               text="Ces alertes peuvent être désactivées dans le menu \"Paramétrage\" situé dans le bandeau de gauche.",
               closeOnClickOutside = T)
  flexdashboard::gauge(label = "Zones sur-dotées",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.95*(max_val-min_val)),
                                                           danger=c(min_val+.95*(max_val-min_val),max_val)))
  
}

jauge_threshold_UD = function(input,zonage_pop_reac,regions_reac){
  message("func : jauge_threshold_UD")
  
  
  req(input$choix_reg)
  info_reg=regions_reac()[reg%in%input$choix_reg]
  print(info_reg)
  min_val=0
  max_val=if(input$choix_ps=='sf'){info_reg$UD_sf}else if(input$choix_ps=='inf'){info_reg$UD_inf}
  print(max_val)
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="UD"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val&!input$remove_alerte_jauge)
    shinyalert(title = "Dépassement de la population en zone sous-dotée (jauges en bas à droite)"
               ,text="Ces alertes peuvent être désactivées dans le menu \"Paramétrage\" situé dans le bandeau de gauche.",
               closeOnClickOutside = T)
  
  flexdashboard::gauge(label = "Zones sous dotées",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.95*(max_val-min_val)),
                                                           danger=c(min_val+.95*(max_val-min_val),max_val)))
  
}



jauge_threshold_MD= function(input,zonage_pop_reac_md,regions_reac){
  message("func : jauge_threshold_MD")
  
  req(input$choix_reg)
  info_reg=regions_reac()[reg%in%input$choix_reg]
  min_val=0
  max_val=5
  req(max_val)
  val = round(100*(ifelse(length(zonage_pop_reac_md()[picked_zonage=="ZIP"&CN=="ZZ_Hors vivier"]$pop)!=0,
                          zonage_pop_reac_md()[picked_zonage=="ZIP"&CN=="ZZ_Hors vivier"]$pop,0)+
                     ifelse(length(zonage_pop_reac_md()[picked_zonage=="ZAC"&CN=="ZZ_Hors vivier"]$pop)!=0,
                            zonage_pop_reac_md()[picked_zonage=="ZAC"&CN=="ZZ_Hors vivier"]$pop,0)),1)
  if (length(val)==0)val<-0
  print(val)
  if (val > max_val&!input$remove_alerte_jauge)
    shinyalert(title = "Dépassement de la population en marge dérogatoire (jauges en bas à droite)",
               text="Ces alertes peuvent être désactivées dans le menu \"Paramétrage\" situé dans le bandeau de gauche.",
               closeOnClickOutside = T)
  flexdashboard::gauge(label = "Marge dérogatoire",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(
                         success=c(min_val,min_val+.5*(max_val-min_val)),
                         warning=c(min_val+.5*(max_val-min_val),min_val+.95*(max_val-min_val)),
                         danger=c(min_val+.95*(max_val-min_val),max_val)))
}

jauge_threshold_ZAC = function(input,zonage_pop_reac,regions_reac){
  message("func : jauge_threshold_ZAC")
  
  
  req(input$choix_reg)
  info_reg=regions_reac()[reg%in%input$choix_reg]
  min_val=0
  max_val=info_reg$maxZAC
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="ZAC"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val&!input$remove_alerte_jauge)
    shinyalert(title = "Dépassement de la population en ZAC (jauges en bas à droite)"
               ,text="Ces alertes peuvent être désactivées dans le menu \"Paramétrage\" situé dans le bandeau de gauche.",
               closeOnClickOutside = T)
  flexdashboard::gauge(
    label = "ZAC",
    symbol = '%',
    value = val,
    min = min_val,
    max = max_val,
    sectors = flexdashboard::gaugeSectors(
      success = c(min_val, min_val + .5 * (max_val - min_val)),
      warning = c(min_val + .5 * (max_val - min_val), min_val + .95 * (max_val - min_val)),      
      danger = c(min_val + .95 * (max_val - min_val), max_val)
    )
  )
}


jauge_threshold_ZIP = function(input,zonage_pop_reac,regions_reac){
  message("func : jauge_threshold_ZIP")
  
  req(input$choix_reg)
  info_reg=regions_reac()[reg%in%input$choix_reg]
  min_val=info_reg$SN
  max_val=info_reg$maxZIP
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="ZIP"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val&!input$remove_alerte_jauge){
    shinyalert(title = "Dépassement de la population en ZIP (jauges en bas à droite)",
               text="Ces alertes peuvent être désactivées dans le menu \"Paramétrage\" situé dans le bandeau de gauche.",
               closeOnClickOutside = T)
  }
  flexdashboard::gauge(label = "ZIP",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=gaugeSectors(
                         success=c(min_val,min_val+.5*(max_val-min_val)),
                         warning=c(min_val+.5*(max_val-min_val),min_val+.95*(max_val-min_val)),
                         danger=c(min_val+.95*(max_val-min_val),max_val)))
  
}




zonage_pop_md_func = function(input,session,vals_reac,tableau_reg){
  message("func : zonage_pop_md_func")
  
  print("zonage_pop_reac_md")
  infos_md=dplyr::left_join(tableau_reg[,c("agr","population","CN")],
                            vals_reac(),by="agr")
  print(head("infos_md")) ; print(head(infos_md)) 
  
  zonage_pop_md=data.table(infos_md)[
    ,list(pop=sum(population,na.rm=T)),
    by=c("picked_zonage","CN")]
  print("zonage_pop_md") ; print(zonage_pop_md)
  
  pop_reg_md=sum(tableau_reg[which(tableau_reg$CN=="02_Vivier")]$population)
  print("pop_reg_md") ; print(pop_reg_md)
  zonage_pop_md[,pop:=pop/pop_reg_md]
  print("zonage_pop_md") ; print(zonage_pop_md)
  
  print("zonage_pop_reac_md OK")
  zonage_pop_md
}



zonage_pop_func = function(input,session,vals_reac,hist_qpv_reac,zonage_qpv_reac,tableau_reg){
  message("func : zonage_pop_func")
  
  
  req(input$choix_ps)
  if(input$choix_ps == "mg"){# A vérifier, je compte toute la pop de la région y compris minoritaire mais à partir de population_reg (cf prep_zonage_mg)
    
    infos=merge(tableau_reg[,c("agr","population")],
                vals_reac(),by="agr",all.x=T)
    
    na_pop = tableau_reg[is.na(population)]
    if(nrow(na_pop)>0){
      showNotification(sprintf("La population est manquante pour les TVS [%s]. Merci de faire remonter celle alerte aux équipes techniques.",paste(paste0(na_pop$libagr," (",na_pop$agr,")"),collapse=", ")),type = "error",duration = NULL,session=session)
    }
    
    infos_qpv = merge(hist_qpv_reac(),zonage_qpv_reac(),by="cod")[,c("agr","cod","picked_zonage","pop")]
    setnames(infos_qpv,c("picked_zonage","pop"),c("picked_zonage_qpv","pop_qpv"))
    
    infos_qpv = merge(infos_qpv,infos,by="agr")
    infos_qpv = infos_qpv[picked_zonage!=picked_zonage_qpv]
    a_soustraire = infos_qpv[,.(pop_suppr = sum(pop_qpv)),by="agr"]
    infos[a_soustraire,population:=population-i.pop_suppr,on="agr"]
    
    infos = infos[,c("picked_zonage","population")]
    infos_qpv = infos_qpv[,c("picked_zonage_qpv","pop_qpv")]
    names(infos_qpv) <- c("picked_zonage","population")
    infos = rbind(infos,infos_qpv[,c("picked_zonage","population")])
    
    zonage_pop=data.table(infos)[
      ,list(pop=sum(population,na.rm=T)),
      by="picked_zonage"]
    
    
    pop_reg=sum(zonage_pop$pop,na.rm=T)
    zonage_pop[,pop:=pop/pop_reg]
    print("zonage_pop_reac OK")
    
    zonage_pop 
  } else if(input$choix_ps %in% c("sf","inf")){#on s'intéresse uniquement aux BVCV pr lesquels la région est majoritaire, pour ceux-là on compte toutes les communes du BVCV y compris régions voisines.
    
    infos=merge(tableau_reg[,c("agr","population","is_majoritaire")],
                vals_reac(),by="agr",all.x=T)
    zonage_pop=data.table(infos)[(is_majoritaire)
                                 ,list(pop=sum(population,na.rm=T)),
                                 by="picked_zonage"]
    na_pop = tableau_reg[(is_majoritaire) & is.na(population)]
    if(nrow(na_pop)>0){
      showNotification(sprintf("La population est manquante pour les BVCV [%s]. Merci de faire remonter celle alerte aux équipes techniques.",paste(paste0(na_pop$libagr," (",na_pop$agr,")"),collapse=", ")),type = "error",duration = NULL,session=session)
    }
    pop_reg=sum(tableau_reg[(is_majoritaire)]$population,na.rm=T)
    zonage_pop[,pop:=pop/pop_reg]
    print("zonage_pop_reac OK")
    zonage_pop
  }
}
