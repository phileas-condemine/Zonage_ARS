plot_region = function(contours){
  message("func : plot_region")
  
  bbox_reg <- st_bbox(contours[as.numeric(contours$reg)>10,]) %>% 
    as.vector()
  leaflet(data = contours,options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")%>%
    addPolygons(label = ~ paste(libreg,"code :",reg),
                layerId = ~ as.numeric(reg),color=NULL)%>%
    fitBounds(bbox_reg[1]-1,bbox_reg[2],bbox_reg[3],bbox_reg[4])
}


plot_communes = function(input,tableau_reg,default_vals,fond_de_carte){
  message("func : plot_communes")
  
  
  
  infos=merge(tableau_reg[,c("agr","population","CN","communes_codes")],
              default_vals,by="agr",all.x=T)
  infos <- infos%>%mutate_if(is.factor,as.character)
  # infos <<- infos
  contours_reg=merge(fond_de_carte,infos,by.x="agr",
                     by.y="agr",all.x=T)    
  # contours_reg <<- contours_reg
  missing_contours=infos$agr[!infos$agr%in%fond_de_carte$agr]
  if(sum(is.na(contours_reg$picked_zonage))>0){
    contours_reg[is.na(contours_reg$picked_zonage),]$picked_zonage <- "Non-spécifié"
  }
  if(sum(is.na(contours_reg$agr))>0){
    if(input$ps=="mg"){
      contours_reg[is.na(contours_reg$agr),]$picked_zonage <- "Erreur TVS-COM"
    }else{
      contours_reg[is.na(contours_reg$agr),]$picked_zonage <- "Choix de l'ARS majoritaire"
      
    }
  }
  if(input$choix_ps%in%c("sf","inf")){
    
    contours_reg <- contours_reg%>%mutate(picked_zonage=case_when(
      picked_zonage=="VUD"~"Très sous-doté",
      picked_zonage=="UD"~"Sous-doté",
      picked_zonage=="Int"~"Intermédiaire",
      picked_zonage=="VD"~"Très doté",
      picked_zonage=="OD"~"Sur-doté"))
  }
  
  contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                    levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HZ","Non-spécifié","ZAC","ZIP")
                                    }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}) 
  print("leaflet picked zonage levels")
  print(levels(contours_reg$picked_zonage))
  
  
  factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#FB9A99','#E31A1C')
  } else{c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
  contours_reg$picked_zonage,alpha=.3)
  
  print("Carto OK")
  l <- leaflet(data=contours_reg) %>%
    addTiles()%>%
    addPolygons(data=contours_reg,
                fillColor = ~factpal(picked_zonage),
                smoothFactor = 0.2,group=~picked_zonage,
                fillOpacity = .5,stroke = FALSE,layerId=~agr,
                label = ~iconv(paste(libagr,"Zonage:",picked_zonage),to="UTF-8"),
                highlightOptions = highlightOptions(fillOpacity=1,bringToFront = TRUE))%>%
    # addLegend(pal = factpal,
    #           values = ~picked_zonage,
    #           opacity = .7,
    #           layerId = "legend",
    #           title="Zonage",
    #           group = "Légende")%>%
    addLayersControl(
      overlayGroups = setdiff(levels(contours_reg$picked_zonage),NA),
      options = layersControlOptions(collapsed = T)
    )
  
  
  l
}


add_qpv = function(input,session,coords){
  message("func : add_qpv")
  l = leafletProxy("communes_map")
  
  cols = c("ZIP"="red",
           "ZAC"="lightred",
           "HZ"="blue")
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = unname(cols[coords$picked_zonage])
  )
  
  if(input$toggle_qpv){
    l %>% clearGroup("marker_qpv")%>%
      addAwesomeMarkers(data=coords,layerId = ~CODE_QP,icon=icons,label=~CODE_QP,
                        popup = ~ paste0("code QPV : ",CODE_QP,
                                         "<br>nom QPV : ",NOM_QP,
                                         "<br>commune : ", COMMUNE_QP,
                                         "<br>pop municipale : ",pop,
                                         "<br>zonage : ",picked_zonage),group = "marker_qpv")
  }
  else {
    l %>% clearGroup("marker_qpv")
  }
}


update_qpv = function(input,session,coords,latest_modif){
  message("func : update_qpv")
  
  l = leafletProxy("communes_map",session=session)
  
  cols = c("ZIP"="red",
           "ZAC"="lightred",
           "HZ"="blue")
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = unname(cols[latest_modif$picked_zonage])
  )
  if(input$toggle_qpv){
    l %>% 
      removeMarker(latest_modif$cod)%>%
      addAwesomeMarkers(data=coords[coords$CODE_QP == latest_modif$cod,],
                        layerId = ~CODE_QP,icon=icons,label=~CODE_QP,
                        popup = ~ paste0("code QPV : ",CODE_QP,
                                         "<br>nom QPV : ",NOM_QP,
                                         "<br>commune : ", COMMUNE_QP,
                                         "<br>pop municipale : ",pop,
                                         "<br>zonage : ",picked_zonage),group = "marker_qpv")
  }
}



update_table2map = function(input,session,vals_reac,current_mapped_data_reac,tableau_reg,fond_de_carte){
  message("func : update_table2map")
  
  map_proxy <- leafletProxy("communes_map",session)
  # On récupère les éléments qui ont changé
  changed_data=anti_join(vals_reac(),current_mapped_data_reac(),
                         by=c("agr","picked_zonage"))
  current_mapped_data_reac(vals_reac())
  print("nb rows modif")
  print(nrow(changed_data))
  if(nrow(changed_data)>0){
    print("update polygon zonage")
    # cette fois inner join et non left join pour infos ET pour contours_reg pour seulement éditer les polygones nécessaires.
    infos=merge(tableau_reg[,c("agr","population","CN","communes_codes")],
                changed_data,by="agr",all.x=F)
    infos <- infos%>%mutate_if(is.factor,as.character)
    contours_reg=merge(fond_de_carte,infos,by="agr",all.x=F)
    print(nrow(contours_reg))
    missing_contours=infos$agr[!infos$agr%in%fond_de_carte$code]
    
    if(sum(is.na(contours_reg$picked_zonage))>0){
      contours_reg[is.na(contours_reg$picked_zonage),]$picked_zonage <- "Non-spécifié"
    }
    if(sum(is.na(contours_reg$agr))>0){
      if(input$choix_ps=="mg"){
        contours_reg[is.na(contours_reg$agr),]$picked_zonage <- "Erreur TVS-COM"
      }else{
        contours_reg[is.na(contours_reg$agr),]$picked_zonage <- "Choix de l'ARS majoritaire"
      }
    }
    if(input$choix_ps%in%c("sf","inf")){
      
      contours_reg <- contours_reg%>%mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    
    contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HZ","Non-spécifié","ZAC","ZIP")
                                        # }else{c("Choix de l'ARS majoritaire","VUD","UD","Int","VD","OD")}) 
                                      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}) 
    
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#FB9A99','#E31A1C')
    } else{c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
    contours_reg$picked_zonage,alpha=.3)
    
    print("update content")
    print(contours_reg)
    
    legend_data=vals_reac()
    if(sum(is.na(legend_data$picked_zonage))>0){
      legend_data[is.na(legend_data$picked_zonage),]$picked_zonage <- "Non-spécifié"
    }
    if(sum(is.na(legend_data$agr))>0){
      if(input$choix_ps=="mg"){
        legend_data[is.na(legend_data$agr),]$picked_zonage <- "Erreur TVS-COM"
      }else{
        legend_data[is.na(legend_data$agr),]$picked_zonage <- "Choix de l'ARS majoritaire"
      }
    }
    if(input$choix_ps%in%c("sf","inf")){
      
      legend_data <- legend_data%>%mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    
    if(input$choix_ps=="mg"){
      levels_picked_zonage = c("Erreur TVS-COM","HZ","Non-spécifié","ZAC","ZIP")
    }else{
      levels_picked_zonage = c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")
    }
    legend_data$picked_zonage = factor(legend_data$picked_zonage,
                                       levels = levels_picked_zonage)
    
    map_proxy %>% 
      addPolygons(data=contours_reg,
                  fillColor = ~factpal(picked_zonage),
                  smoothFactor = 0.2,
                  fillOpacity = .5,
                  stroke = FALSE,
                  layerId=~agr,
                  label = ~iconv(paste(libagr,"Zonage:",picked_zonage),to="UTF-8"),
                  highlightOptions = highlightOptions(fillOpacity=1,bringToFront = TRUE))#%>%
  } else {
    showNotification(session=session,
                     "PROBLEME AVEC CE TVS, MAL REFERENCE !\nMerci d'en informer le support dans la rubrique \"Nous contacter\"",
                     type = "error",duration = NULL)
  }
  
}



hover_table2map = function(input,session,carte){
  message("func : hover_table2map")
  req(input$last_row_hovered)
  print("hovered row")
  print(input$last_row_hovered)
  if(input$last_row_hovered%in%carte$agr){
    map_proxy <- leafletProxy("communes_map",session)
    contours_reg = carte[carte$agr == input$last_row_hovered,]
    bbox <- st_bbox(contours_reg) %>% 
      as.vector()
    map_proxy %>% clearGroup("highlight_on_hover")%>%
      addPolygons(data=contours_reg,fill = F,stroke=T,opacity=1,
                  fillColor="black",group = "highlight_on_hover")
  }
  
  
}






