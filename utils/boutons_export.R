# DL table Excel
output$download_table = downloadHandler(
  filename = function() {
    req(input$choix_reg)
    my_reg=input$choix_reg
    reg_name=regions[reg%in%my_reg]$libreg
    paste0(reg_name,".xlsx")
  },
  content = function(file) {
    
    infos <- vals_reac()
    if(input$choix_ps=="mg"){
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","population")],by="agr")
    } else if (input$choix_ps %in% c("sf","inf")){
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      infos <- merge(infos,communes_BVCV[,c("agr","libagr","libcom","depcom","population")],by="agr")
    }
    infos <- infos[,c("libagr","agr","libcom","depcom","population","picked_zonage")]
    infos <- infos%>%mutate_if(is.factor,as.character)
    if(input$choix_ps%in%c("sf","inf")){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    
    
    
    infos <- infos%>%data.table
    if(input$choix_ps=="mg"){
      names(infos) <- c("Nom du territoire de vie-santé",
                        "Code du territoire de vie-santé",
                        "Nom de la commune",
                        "Code de la commune",
                        "Population",
                        "Zonage")
    } else if(input$choix_ps=="inf"){
      names(infos) <- c("Nom du bassin de vie ou pseudo-canton",
                        "Code du bassin de vie ou pseudo-canton",
                        "Nom de la commune",
                        "Code de la commune",
                        "Population",
                        "Zonage")
    } else if(input$choix_ps=="sf"){
      infos[,population:=NULL]
      infos = merge(infos,pop_femmes,by.x="depcom",by.y="CODGEO",all.x=T)
      names(infos) <- c("Nom du bassin de vie ou pseudo-canton",
                        "Code du bassin de vie ou pseudo-canton",
                        "Nom de la commune",
                        "Code de la commune",
                        "Zonage","Population_femmes")
    }
    # USE OFFICER PACKAGE TO EXPORT EXCEL
    write.xlsx(infos,file)
  }
)

# DL Carte ggplot2 geom_sf
output$download_plot <- downloadHandler(
  filename = function() {
    req(input$choix_reg)
    my_reg=input$choix_reg
    reg_name=regions[reg%in%my_reg]$libreg
    paste0(reg_name,"_",input$choix_ps,".png")
  },
  content = function(file) {
    infos=merge(tableau_reg()[,c("agr","libagr","population","CN","communes_codes")],
                vals_reac(),by="agr",all.x=T)
    infos <- infos%>%mutate_if(is.factor,as.character)
    if(input$choix_ps%in%c("sf","inf")){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    infos <- infos %>% mutate(nom_zonage=paste0(libagr,": ",picked_zonage))
    contours_reg=merge(fond_de_carte()[,c("agr","geometry")],infos,by.x="agr",
                       by.y="agr",all.x=T)
    contours_reg <- contours_reg %>% arrange(-population)
    contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
                                      }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")})
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    }else{c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
    contours_reg$picked_zonage,alpha=.3)
    if(input$choix_ps=='mg'){
      my_colors <- c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    } else if (input$choix_ps %in% c("sf","inf")){
      my_colors <- c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    }
    names(my_colors) <- if(input$choix_ps=='mg'){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
    }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}
    g <- ggplot(data=contours_reg)+geom_sf(aes(fill=picked_zonage),alpha=.5)+theme(legend.title = element_blank())+
      labs(caption=paste0("Source : COG ",year(Sys.Date()),", date : ",format.Date(Sys.Date(),"%d/%m/%Y")))+
      ggrepel::geom_label_repel(
        data = head(contours_reg,10),
        aes(label = nom_zonage, geometry = geometry),
        stat = "sf_coordinates",
        min.segment.length = 0
      ) +
      theme(axis.title.x = element_blank(),axis.title = element_blank())+
      scale_fill_manual(aesthetics = "fill",values=my_colors)+
      blank() +
      north(contours_reg,location = "bottomleft") +
      scalebar(contours_reg, dist = 20, dist_unit = "km",
               transform = TRUE, model = "WGS84")     
    if(input$choix_ps=="mg"){g <- g + ggtitle("Zonage médecin")
    }else if(input$choix_ps=="sf"){g <- g + ggtitle("Zonage sage-femme")
    }else if(input$choix_ps=="inf"){g <- g + ggtitle("Zonage infirmier")}
    
    # g
    ggsave(g,filename = file,width = 16,height = 10.4)
  }
)

# DL MS-Word Modèle d'arrêté
output$download_arrete <- downloadHandler(
  filename = function() {
    my_reg=input$choix_reg
    reg_name=regions[reg%in%my_reg]$libreg
    paste0(reg_name,"_model.docx")
  },
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    temp_dir=tempdir()
    print(paste0("create_arrete_",input$choix_ps,".Rmd"))
    tempReport <- file.path(temp_dir, paste0("create_arrete_",input$choix_ps,".Rmd"))
    file.copy(paste0("create_arrete_",input$choix_ps,".Rmd"), tempReport, overwrite = TRUE)
    tempimg <- file.path(temp_dir, "ARS.png")
    file.copy("utils/ARS.png", tempimg, overwrite = TRUE)
    jour_nommois_annee=function(d){
      paste(day(d),mois_noms[month(d)],year(d))
    }
    
    my_reg=input$choix_reg
    reg_name=regions[reg%in%my_reg]$libreg
    
    # LA TABLE
    
    my_table <- vals_reac()
    if(input$choix_ps=="mg"){
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      my_table <- merge(my_table,communes_TVS[,c("agr","libagr","libcom","depcom","population")],by="agr")
    } else if (input$choix_ps %in% c("sf","inf")){
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      my_table <- merge(my_table,communes_BVCV[,c("agr","libagr","libcom","depcom","population")],by="agr")
    }
    my_table <- my_table[,c("libagr","agr","libcom","depcom","population","picked_zonage")]
    my_table <- my_table%>%mutate_if(is.factor,as.character)
    if(input$choix_ps%in%c("sf","inf")){
      print(table(my_table$picked_zonage))
      my_table <- my_table %>% mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    
    my_table <- my_table%>%data.table
    if(input$choix_ps=="mg"){
      names(my_table) <- c("Nom du territoire de vie-santé",
                           "Code du territoire de vie-santé",
                           "Nom de la commune",
                           "Code de la commune",
                           "Population",
                           "Zonage")
    } else if(input$choix_ps=="inf"){
      names(my_table) <- c("Nom du bassin de vie ou pseudo-canton",
                           "Code du bassin de vie ou pseudo-canton",
                           "Nom de la commune",
                           "Code de la commune",
                           "Population",
                           "Zonage")
    } else if(input$choix_ps=="sf"){
      my_table[,population:=NULL]
      my_table = merge(my_table,pop_femmes,by.x="depcom",by.y="CODGEO",all.x=T)
      names(my_table) <- c("Nom du bassin de vie ou pseudo-canton",
                           "Code du bassin de vie ou pseudo-canton",
                           "Nom de la commune",
                           "Code de la commune",
                           "Zonage","Population_femmes")
    }
    print("la table")
    print(head(my_table))
    
    # LA CARTE
    
    infos=merge(tableau_reg()[,c("agr","libagr","population","CN","communes_codes")],
                vals_reac(),by="agr",all.x=T)
    infos <- infos%>%mutate_if(is.factor,as.character)
    if(input$choix_ps%in%c("sf","inf")){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    }
    
    
    infos <- infos %>% mutate(nom_zonage=paste0(libagr,": ",picked_zonage))
    contours_reg=merge(fond_de_carte()[,c("agr","geometry")],infos,by.x="agr",
                       by.y="agr",all.x=T)
    contours_reg <- contours_reg %>% arrange(-population)
    contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
                                      }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")})
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    }else{c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
    contours_reg$picked_zonage,alpha=.3)
    if(input$choix_ps=='mg'){
      my_colors <- c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    } else if (input$choix_ps %in% c("sf","inf")){
      my_colors <- c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    }
    names(my_colors) <- if(input$choix_ps=='mg'){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
    }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}
    g <- ggplot(data=contours_reg)+geom_sf(aes(fill=picked_zonage),alpha=.5)+theme(legend.title = element_blank())+
      labs(caption=paste0("Source : COG ",year(Sys.Date()),", date : ",format.Date(Sys.Date(),"%d/%m/%Y")))+
      ggrepel::geom_label_repel(
        data = head(contours_reg,10),
        aes(label = nom_zonage, geometry = geometry),
        stat = "sf_coordinates",
        min.segment.length = 0
      ) +
      theme(axis.title.x = element_blank(),axis.title = element_blank())+
      scale_fill_manual(aesthetics = "fill",values=my_colors)+
      blank() +
      north(contours_reg,location = "bottomleft") +
      scalebar(contours_reg, dist = 20, dist_unit = "km",
               transform = TRUE, model = "WGS84")     
    if(input$choix_ps=="mg"){g <- g + ggtitle("Zonage médecin")
    }else if(input$choix_ps=="sf"){g <- g + ggtitle("Zonage sage-femme")
    }else if(input$choix_ps=="inf"){g <- g + ggtitle("Zonage infirmier")}
    
    # Set up parameters to pass to Rmd document
    params <- list(REGION_NAME = reg_name,
                   DATE_NOMINATION_DG_ARS=input$DATE_DG_ELECTION%>%jour_nommois_annee,
                   DATE_DEBUT_DG_ARS=input$DATE_DG_EFFET%>%jour_nommois_annee,
                   NOM_DG_ARS = input$NOM_DG_ARS,
                   GENRE_DG_ARS = input$GENRE_DG_ARS,
                   DATE_PRECEDENT_ARRETE=input$DATE_LAST_ARRETE%>%jour_nommois_annee,
                   DATE_DECISION_ARS_ZONAGE=input$DATE_NOUVEL_ARRETE%>%jour_nommois_annee,
                   # LIEN_VERS_SITE_ARS,
                   # ANNEE_CALCUL_APL,
                   # PROP_5pct_VIVIER,
                   # NB_TVS_ZIP,
                   # NB_TVS_ZAC,
                   # NB_ZAC_INSULAIRES,
                   # NB_ZAC_AUTRES,
                   # LIST_TVS_ZIP,
                   # LIST_ZAC_INSULAIRES,
                   # LIST_ZAC_AUTRES,
                   TABLE = my_table,
                   CARTE=g,
                   VILLE_REDACTION=input$VILLE_TRIBUNAL_ADMINISTRATIF,
                   TODAY=Sys.Date()%>%jour_nommois_annee,
                   VILLE_REGION_TRIBUNAL=input$VILLE_TRIBUNAL_ADMINISTRATIF
                   
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

observeEvent(input$generate_arrete,{
  my_TAs=TA[reg%in%input$choix_reg]$TA
  showModal(modalDialog(title="Informations relatives à l'arrêté",
                        size="l",easyClose = T,footer=NULL,
                        fluidRow(
                          column(6,
                                 textInput("NOM_DG_ARS","Directeur.rice Général.e",placeholder = "Mme/M. X"),
                                 textInput("GENRE_DG_ARS","H / F",placeholder = "F"),
                                 dateInput("DATE_DG_ELECTION","Date du décret de nomination du DG",startview = "decade", language = "fr",value = Sys.Date()-100,format = "dd-mm-yyyy"),
                                 dateInput("DATE_DG_EFFET","Date de prise de fonction du DG",startview = "decade", language = "fr",value = Sys.Date()-30,format = "dd-mm-yyyy"),
                                 dateInput("DATE_LAST_ARRETE","Date du précédent arrêté",startview = "decade", language = "fr",value = Sys.Date()-400,format = "dd-mm-yyyy"),
                                 dateInput("DATE_NOUVEL_ARRETE","Date du nouvel arrêté",startview = "decade", language = "fr",value = Sys.Date(),format = "dd-mm-yyyy"),
                                 selectInput("VILLE_TRIBUNAL_ADMINISTRATIF","Ville du Tribunal Administratif de région",choices=my_TAs,selected=my_TAs[1]),
                                 downloadButton(outputId="download_arrete",label="Arrêté")),
                          column(6,div(style="display: table-cell;vertical-align: middle",
                                       HTML("Le rapport proposé en téléchargement est au format .docx Microsft Word afin de pouvoir être relu et complété."))
                          ))))
})
