coord_qpv = reactive({
  
  req(input$choix_reg)
  my_reg=input$choix_reg
  reg_name=regions_reac()[reg==my_reg]$libreg
  my_deps=dep()[reg==my_reg]$dep
  my_qpv <- mom_markers()
  my_qpv$dep = gsub("^0","",substr(my_qpv$CODE_QP,3,5))
  my_qpv = my_qpv[my_qpv$dep%in%my_deps,]
  my_qpv = merge(my_qpv,hist_qpv()[,c("cod","pop")],by.x="CODE_QP",by.y="cod")
  my_qpv = merge(my_qpv,zonage_qpv(),by.x="CODE_QP",by.y="cod")
  
})

output$communes_map=renderLeaflet({
  print("Carto")
  if(has_logged_in()){
    infos=merge(tableau_reg()[,c("agr","population","CN","communes_codes")],
                default_vals(),by="agr",all.x=T)
    infos <- infos%>%mutate_if(is.factor,as.character)
    infos <<- infos
    contours_reg=merge(fond_de_carte(),infos,by.x="agr",
                       by.y="agr",all.x=T)    
    contours_reg <<- contours_reg
    missing_contours=infos$agr[!infos$agr%in%fond_de_carte()$agr]
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
                                      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
                                      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}) 
    print("leaflet picked zonage levels")
    print(levels(contours_reg$picked_zonage))
    
    
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
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
  } else NULL
})

observeEvent(modif_zonage_qpv(),{
  if(!is.null(input$toggle_qpv)){
    l = leafletProxy("communes_map")
    
    cols = c("ZIP"="red",
             "ZAC"="lightred",
             "ZV"="green",
             "HV"="blue")
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = unname(cols[modif_zonage_qpv()$picked_zonage])
    )
    coords = coord_qpv()
    if(input$toggle_qpv){
      l %>% removeMarker(modif_zonage_qpv()$cod)%>%
        addAwesomeMarkers(data=coords[coords$CODE_QP == modif_zonage_qpv()$cod,],layerId = ~CODE_QP,icon=icons,label=~CODE_QP,
                          popup = ~ paste0("code QPV : ",CODE_QP,
                                           "<br>nom QPV : ",NOM_QP,
                                           "<br>commune : ", COMMUNE_QP,
                                           "<br>pop municipale : ",pop,
                                           "<br>zonage : ",picked_zonage),group = "marker_qpv")
    }
    
  }
  
  
})



observeEvent(c(input$toggle_qpv),{
  
  
  if(!is.null(input$toggle_qpv)){
    l = leafletProxy("communes_map")
    
    cols = c("ZIP"="red",
             "ZAC"="lightred",
             "ZV"="green",
             "HV"="blue")
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = unname(cols[coord_qpv()$picked_zonage])
    )
    
    if(input$toggle_qpv){
      l %>% clearGroup("marker_qpv")%>%
        addAwesomeMarkers(data=coord_qpv(),layerId = ~CODE_QP,icon=icons,label=~CODE_QP,
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
  
  
})

# Update zonage sur la carte à partir de la saisie dans le tableau
observeEvent(input$last_btn,{
  # updated_data <<- vals_reac()
  map_proxy <- leafletProxy("communes_map",session)
  # On récupère les éléments qui ont changé
  changed_data=anti_join(vals_reac(),current_mapped_data(),
                         by=c("agr","picked_zonage"))
  current_mapped_data(vals_reac())
  # modified_only <<- changed_data
  print("nb rows modif")
  print(nrow(changed_data))
  if(nrow(changed_data)>0){
    print("update polygon zonage")
    # cette fois inner join et non left join pour infos ET pour contours_reg pour seulement éditer les polygones nécessaires.
    infos=merge(tableau_reg()[,c("agr","population","CN","communes_codes")],
                changed_data,by="agr",all.x=F)
    infos <- infos%>%mutate_if(is.factor,as.character)
    contours_reg=merge(fond_de_carte(),infos,by="agr",all.x=F)
    print(nrow(contours_reg))
    missing_contours=infos$agr[!infos$agr%in%fond_de_carte()$code]
    
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
                                      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
                                        # }else{c("Choix de l'ARS majoritaire","VUD","UD","Int","VD","OD")}) 
                                      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}) 
    
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
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
      levels_picked_zonage = c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
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
    # removeControl("legend") %>% 
    # addLegend(pal = factpal, data=legend_data,
    #           values = ~picked_zonage, 
    #           opacity = .7,
    #           layerId = "legend",
    #           title="Zonage",
    #           group = "Légende")
    
  } else {
    print("PROBLEME AVEC CE TVS, MAL REFERENCE !")
  }
})


observeEvent(input$communes_map_shape_click,{
  req(input$communes_map_shape_click)
  my_dt_output=dataTableProxy("zonage_dt",session)
  my_agr=input$communes_map_shape_click$id
  print("my_agr") ; print(my_agr)
  my_dt_output%>%updateSearch(keywords = list(global=my_agr))
})


# observeEvent(input$legend_click,{
#   if(input$choix_ps!="mg"){
#     my_color = stringr::str_extract(input$legend_click,"background:#.{6};")
#     my_color = gsub("(background:)|(;)","",my_color)
#     
#     cats=factor(if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP",NA)
#     }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté",NA)},
#     levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
#     }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")})
#     
#     factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
#     } else{c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},cats)
#     
#     zone_to_color = data.table(cat = cats, color=factpal(cats))
#     cat = zone_to_color[color==my_color]$cat%>%as.character
#     print(cat)
#     if(length(cat)==1)
#       dataTableProxy("zonage_dt",session)%>%updateSearch(keywords = list(global=cat))
#   }
# })

observeEvent(input$last_row_hovered,{
  # print("row hovered")
  req(input$last_row_hovered)
  print("hovered row")
  print(input$last_row_hovered)
  # print(input$last_row_hovered)
  carte=fond_de_carte()
  if(input$last_row_hovered%in%carte$agr){
    map_proxy <- leafletProxy("communes_map",session)
    contours_reg = carte[carte$agr == input$last_row_hovered,]
    bbox <- st_bbox(contours_reg) %>% 
      as.vector()
    map_proxy %>% clearGroup("highlight_on_hover")%>%
      addPolygons(data=contours_reg,fill = F,stroke=T,opacity=1,
                  fillColor="black",group = "highlight_on_hover")# %>%
  }
})


observeEvent(input$update_contours,{
  if(!is.null(input$update_contours)&!is.null(input$choix_reg)){
    if(input$update_contours){
      my_reg=input$choix_reg
      reg_name=regions_reac()[reg==my_reg]$libreg
      my_deps=dep()[reg==my_reg]$dep
      source("utils/handle_geo_data.R",local=T,encoding = "UTF-8")
      showNotification("Une fois les données mises à jour, l'application va redémarrer et vous devrez vous reconnecter.",type = "message",duration = NULL)
      my_ps = input$choix_ps
      if(my_ps=="mg"){
        mailles_geo = "TVS"
      } else {
        mailles_geo = "BVCV"
      }
      
      prep_geo_data_from_scratch(my_reg,refresh_geojson=T,mailles_geo=mailles_geo)
      session$reload()
      
    }
  }
})
