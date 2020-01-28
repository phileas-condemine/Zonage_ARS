zonage_pop_reac=reactive({
  print("zonage_pop_reac")
  req(input$choix_ps)
  if(input$choix_ps == "mg"){# A vérifier, je compte toute la pop de la région y compris minoritaire mais à partir de population_reg (cf prep_zonage_mg)
    infos=merge(tableau_reg()[,c("agr","population")],
                vals_reac(),by="agr",all.x=T)
    zonage_pop=data.table(infos)[
      ,list(pop=sum(population,na.rm=T)),
      by="picked_zonage"]
    pop_reg=sum(zonage_pop$pop)
    zonage_pop[,pop:=pop/pop_reg]
    print("zonage_pop_reac OK")
    zonage_pop 
  } else if(input$choix_ps %in% c("sf","inf")){#on s'intéresse uniquement aux BVCV pr lesquels la région est majoritaire, pour ceux-là on compte toutes les communes du BVCV y compris régions voisines.
    infos=merge(tableau_reg()[,c("agr","population","is_majoritaire")],
                vals_reac(),by="agr",all.x=T)
    # browser()
    zonage_pop=data.table(infos)[(is_majoritaire)
                                 ,list(pop=sum(population,na.rm=T)),
                                 by="picked_zonage"]
    
    pop_reg=sum(tableau_reg()[(is_majoritaire)]$population)
    zonage_pop[,pop:=pop/pop_reg]
    print("zonage_pop_reac OK")
    zonage_pop
  }
})

zonage_pop_reac_md=reactive({
  print("zonage_pop_reac_md")
  infos_md=dplyr::left_join(tableau_reg()[,c("agr","population","CN")],
                            vals_reac(),by="agr")
  print(head("infos_md")) ; print(head(infos_md)) 
  
  zonage_pop_md=data.table(infos_md)[
    ,list(pop=sum(population,na.rm=T)),
    by=c("picked_zonage","CN")]
  print("zonage_pop_md") ; print(zonage_pop_md)
  
  pop_reg_md=sum(tableau_reg()[which(tableau_reg()$CN=="02_Vivier")]$population)
  print("pop_reg_md") ; print(pop_reg_md)
  zonage_pop_md[,pop:=pop/pop_reg_md]
  print("zonage_pop_md") ; print(zonage_pop_md)
  
  print("zonage_pop_reac_md OK")
  zonage_pop_md
})

output$threshold_ZIP=flexdashboard::renderGauge({
  print("Jauge ZIP")
  req(input$choix_reg)
  info_reg=regions[reg%in%input$choix_reg]
  min_val=info_reg$SN
  max_val=info_reg$maxZIP
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="ZIP"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val)
    shinyalert(title = "Dépassement de la population en ZIP",closeOnClickOutside = T)
  flexdashboard::gauge(label = "ZIP",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.8*(max_val-min_val)),
                                                           danger=c(min_val+.8*(max_val-min_val),max_val)))
})


output$threshold_ZAC=flexdashboard::renderGauge({
  print("Jauge ZAC")
  req(input$choix_reg)
  info_reg=regions[reg%in%input$choix_reg]
  min_val=0
  max_val=info_reg$maxZAC
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="ZAC"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val)
    shinyalert(title = "Dépassement de la population en ZAC",closeOnClickOutside = T)
  flexdashboard::gauge(label = "ZAC",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.8*(max_val-min_val)),
                                                           danger=c(min_val+.8*(max_val-min_val),max_val)))
})


output$threshold_MD=flexdashboard::renderGauge({ #changer pour que ça soit en % du vivier, pas de la pop régionale
  print("Jauge marge dérogatoire")
  req(input$choix_reg)
  info_reg=regions[reg%in%input$choix_reg]
  min_val=0
  max_val=5
  req(max_val)
  val = round(100*(ifelse(length(zonage_pop_reac_md()[picked_zonage=="ZIP"&CN=="ZZ_Hors vivier"]$pop)!=0,
                          zonage_pop_reac_md()[picked_zonage=="ZIP"&CN=="ZZ_Hors vivier"]$pop,0)+
                     ifelse(length(zonage_pop_reac_md()[picked_zonage=="ZAC"&CN=="ZZ_Hors vivier"]$pop)!=0,
                            zonage_pop_reac_md()[picked_zonage=="ZAC"&CN=="ZZ_Hors vivier"]$pop,0)),1)
  if (length(val)==0)val<-0
  print(val)
  if (val > max_val)
    shinyalert(title = "Dépassement de la population en marge dérogatoire",closeOnClickOutside = T)
  flexdashboard::gauge(label = "Marge dérogatoire",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.8*(max_val-min_val)),
                                                           danger=c(min_val+.8*(max_val-min_val),max_val)))
})

output$threshold_UD=flexdashboard::renderGauge({ #spécifier les zones d'échange
  print("Jauge UD")
  req(input$choix_reg)
  info_reg=regions[reg%in%input$choix_reg]
  print(info_reg)
  min_val=0
  max_val=if(input$choix_ps=='sf'){info_reg$UD_sf}else if(input$choix_ps=='inf'){info_reg$UD_inf}
  print(max_val)
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="UD"]$pop,1)
  if (length(val)==0)val<-0
  # browser()
  if (val > max_val)
    shinyalert(title = "Dépassement de la population en zone sous-dotée",closeOnClickOutside = T)
  
  flexdashboard::gauge(label = "Zones sous dotées",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.8*(max_val-min_val)),
                                                           danger=c(min_val+.8*(max_val-min_val),max_val)))
})

output$threshold_OD=flexdashboard::renderGauge({ #spécifier les zones d'échange
  print("Jauge OD")
  req(input$choix_reg)
  info_reg=regions[reg%in%input$choix_reg]
  min_val=0
  max_val=if(input$choix_ps=='sf'){info_reg$OD_sf}else if(input$choix_ps=='inf'){info_reg$OD_inf}
  print(max_val)
  req(max_val)
  val = round(100*zonage_pop_reac()[picked_zonage=="OD"]$pop,1)
  if (length(val)==0)val<-0
  if (val > max_val)
    shinyalert(title = "Dépassement de la population en zone sur-dotée",closeOnClickOutside = T)
  flexdashboard::gauge(label = "Zones sur-dotées",symbol = '%',
                       value = val,
                       min = min_val,
                       max = max_val,
                       sectors=flexdashboard::gaugeSectors(success=c(min_val,min_val+.5*(max_val-min_val)),
                                                           warning=c(min_val+.5*(max_val-min_val),min_val+.8*(max_val-min_val)),
                                                           danger=c(min_val+.8*(max_val-min_val),max_val)))
})