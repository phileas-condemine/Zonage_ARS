# DL table Excel
output$download_table = downloadHandler(
  filename = function() {
    req(input$choix_reg)
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    paste0(reg_name,".xlsx")
  },
  content = function(file) {
    
    infos <- vals_reac()
    if(input$choix_ps=="mg"){
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
      setnames(infos,"my_reg_TVS","dansMaRegion")
      infos <- infos[,c("libagr","agr","libcom","depcom","dansMaRegion","population","picked_zonage")]
      
    } else if (input$choix_ps %in% c("sf","inf")){
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      infos <- merge(infos,communes_BVCV[,c("agr","libagr","libcom","depcom","my_reg_BVCV","population")],by="agr")
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg)]),by="agr")
      infos$echangeable = infos$ZE_UD + infos$ZE_OD
      infos$enVigueurAutreReg = !is.na(infos$en_vigueur_autre_reg)
      setnames(infos,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
      infos <- infos[,c("libagr","agr","libcom","depcom","dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg","population","picked_zonage")]
      infos$estMajoritaire = ifelse(infos$estMajoritaire,"oui","non")
      infos$echangeable = ifelse(infos$echangeable>0,"oui","non")
      infos$enVigueurAutreReg = ifelse(infos$enVigueurAutreReg,"oui","non")
    }
    infos$dansMaRegion = ifelse(infos$dansMaRegion,"oui","non")

    
    infos <- infos%>%mutate_if(is.factor,as.character)
    if(input$choix_ps%in%c("sf","inf")){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté"))
    } else if (input$choix_ps == "mg"){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
        picked_zonage=="HV"~"Hors-vivier",
        picked_zonage=="Non-spécifié"~"Non-spécifié",
        picked_zonage=="ZV"~"Zone de vigilance",
        picked_zonage=="ZAC"~"Zone d'action complémentaire",
        picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
      ))
    }
    
    
    infos <- infos%>%data.table
    if(input$choix_ps%in%c("sf","inf")){
      infos[dansMaRegion=="oui"&estMajoritaire=="non"&echangeable=="oui"&enVigueurAutreReg=="non", picked_zonage:="*Intermédiaire*"]
    }
    
    
    if(input$choix_ps=="mg"){
      names(infos) <- c("Nom du territoire de vie-santé",
                        "Code du territoire de vie-santé",
                        "Nom de la commune",
                        "Code de la commune",
                        "dansMaRegion",
                        # "estMajoritaire","echangeable","enVigueurAutreReg",
                        "Population",
                        "Zonage")
    } else if(input$choix_ps=="inf"){
      names(infos) <- c("Nom du bassin de vie ou pseudo-canton",
                        "Code du bassin de vie ou pseudo-canton",
                        "Nom de la commune",
                        "Code de la commune",
                        "dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg",
                        "Population",
                        "Zonage")
      
    } else if(input$choix_ps=="sf"){
      infos[,population:=NULL]
      infos = merge(infos,pop_femmes(),by.x="depcom",by.y="CODGEO",all.x=T)
      names(infos) <- c("Code de la commune","Nom du bassin de vie ou pseudo-canton",
                        "Code du bassin de vie ou pseudo-canton",
                        "Nom de la commune",
                        "dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg",
                        "Zonage","Population_femmes")
    }
    
    
    #### Ajout des QPV
    if(input$choix_ps == "mg"){
      
      infos_zonage_qpv = merge(hist_qpv()[,c("cod","libqpv","agr","pop")],zonage_qpv(),by="cod")
      setnames(infos_zonage_qpv,"picked_zonage","picked_zonage_qpv")
      
      
      infos_zonage_qpv <- infos_zonage_qpv %>% mutate(picked_zonage_qpv=case_when(
        picked_zonage_qpv=="Erreur TVS-COM"~"Erreur TVS-COM",
        picked_zonage_qpv=="HV"~"Hors-vivier",
        picked_zonage_qpv=="Non-spécifié"~"Non-spécifié",
        picked_zonage_qpv=="ZV"~"Zone de vigilance",
        picked_zonage_qpv=="ZAC"~"Zone d'action complémentaire",
        picked_zonage_qpv=="ZIP"~"Zone d'intervention prioritaire"
      ))
      infos_zonage_qpv = merge(infos_zonage_qpv,vals_reac(),by="agr")
      

      infos_zonage_qpv <- infos_zonage_qpv %>% mutate(picked_zonage=case_when(
        picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
        picked_zonage=="HV"~"Hors-vivier",
        picked_zonage=="Non-spécifié"~"Non-spécifié",
        picked_zonage=="ZV"~"Zone de vigilance",
        picked_zonage=="ZAC"~"Zone d'action complémentaire",
        picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
      ))
      
      
      infos_zonage_qpv = infos_zonage_qpv[picked_zonage_qpv!=picked_zonage,c("libqpv","cod","pop","picked_zonage_qpv","picked_zonage")]
      names(infos_zonage_qpv) <- c("Nom du QPV","Code du QPV","Population","Zonage QPV","Zonage du TVS")
      infos = list("TVS"=infos,"QPV"=infos_zonage_qpv)
    }
    
    
    
    write.xlsx(infos,file)
    
  }
)

# DL Carte ggplot2 geom_sf
output$download_plot <- downloadHandler(
  filename = function() {
    req(input$choix_reg)
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    paste0(reg_name,"_",input$choix_ps,".png")
  },
  content = function(file) {
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
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
    } else if (input$choix_ps == "mg"){
      print(table(infos$picked_zonage))
      infos <- infos %>% mutate(picked_zonage=case_when(
        picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
        picked_zonage=="HV"~"Hors-vivier",
        picked_zonage=="Non-spécifié"~"Non-spécifié",
        picked_zonage=="ZV"~"Zone de vigilance",
        picked_zonage=="ZAC"~"Zone d'action complémentaire",
        picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
        ))
    }
    infos <- infos %>% mutate(nom_zonage=paste0(libagr,": ",picked_zonage))
    contours_reg=merge(fond_de_carte()[,c("agr","geometry")],infos,by.x="agr",
                       by.y="agr",all.x=T)
    contours_reg <- contours_reg %>% arrange(-population)
    if(input$choix_ps=="mg"){
      # lev = c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
      lev = c("Erreur TVS-COM","Hors-vivier","Non-spécifié","Zone de vigilance","Zone d'action complémentaire","Zone d'intervention prioritaire")
    }else{
      lev = c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")
      }
    contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                      levels = lev)
    nb_per_zonage = data.table(contours_reg)[,.SD[1],by="agr"][,.N,by="picked_zonage"]
    setkey(nb_per_zonage,"picked_zonage")
    nb_per_zonage = nb_per_zonage[lev]
    nb_per_zonage[is.na(N),N:=0]
    nb_per_zonage$zonage_nb = paste0(nb_per_zonage$picked_zonage," (",nb_per_zonage$N,")")
    
    contours_reg = merge(contours_reg,nb_per_zonage[,c("picked_zonage","zonage_nb")],by="picked_zonage")
    
    # factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    # }else{c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
    # contours_reg$picked_zonage,alpha=.3)
    if(input$choix_ps=='mg'){
      my_colors <- c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    } else if (input$choix_ps %in% c("sf","inf")){
      my_colors <- c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    }
    # names(my_colors) <- if(input$choix_ps=='mg'){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
    # }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}
    names(my_colors) <- nb_per_zonage$zonage_nb
    # https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
    st_crs(contours_reg) <- 4326
    # st_crs(dep_contours) <- 4326
    g <- ggplot(data=contours_reg)+ 
      ggspatial::annotation_map_tile(zoomin = 0)+
      geom_sf(aes(fill=zonage_nb),alpha=.5)+theme(legend.title = element_blank())+
      labs(caption=paste0("Source : COG ",year(Sys.Date()),", date : ",format.Date(Sys.Date(),"%d/%m/%Y")))+
      geom_sf(data=dep_contours()[dep_contours()$reg==input$choix_reg,],aes(fill=NA),color="black",show.legend = F,lwd=1.5)+
      ggrepel::geom_label_repel(
        data = head(contours_reg,20),
        # data = contours_reg,
        # aes(label = nom_zonage, geometry = geometry),
        aes(label = libagr, geometry = geometry),
        stat = "sf_coordinates",
        min.segment.length = 0
      )+ 
      theme(axis.title.x = element_blank(),axis.title = element_blank())+
      scale_fill_manual(aesthetics = "fill",values=my_colors)+
      blank() +
      north(contours_reg,location = "bottomleft") +
      scalebar(contours_reg, dist = 20, dist_unit = "km",
               transform = TRUE, model = "WGS84")    
    
    if(input$choix_ps=="mg"){g <- g + ggtitle(paste0("Zonage médecin - ",reg_name))
    }else if(input$choix_ps=="sf"){g <- g + ggtitle(paste0("Zonage sage-femme - ",reg_name))
    }else if(input$choix_ps=="inf"){g <- g + ggtitle(paste0("Zonage infirmier - ",reg_name))}
    
    # g
    ggsave(g,filename = file,width = 16,height = 10.4)
  }
)


# DL MS-Word Modèle d'arrêté
output$download_arrete <- downloadHandler(
  filename = function() {
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    paste0(input$choix_ps,"_",reg_name,"_model.docx")
  },
  content = function(file) {

    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    # showNotification("Merci de patienter pendant que nous générons l'arrêté contenant la carte et le(s) tableau(x).",type = "message",duration = NULL)
    
    withProgress(message = "Merci de patienter pendant que nous générons l'arrêté contenant la carte et le(s) tableau(x).", {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      temp_dir=tempdir()
      print(paste0("create_arrete_",input$choix_ps,".Rmd"))
      tempReport <- file.path(temp_dir, paste0("create_arrete_",input$choix_ps,".Rmd"))
      tempModele <- file.path(temp_dir, "modele_word_arrete.docx")
      file.copy(paste0("utils/create_arrete_",input$choix_ps,".Rmd"), tempReport, overwrite = TRUE)
      file.copy(paste0("utils/modele_word_arrete.docx"), tempModele, overwrite = TRUE)
      tempimg <- file.path(temp_dir, "ARS.png")
      file.copy("utils/ARS.png", tempimg, overwrite = TRUE)
      jour_nommois_annee=function(d){
        paste(day(d),mois_noms[month(d)],year(d))
      }
      
      my_reg=input$choix_reg
      reg_name=regions_reac()[reg%in%my_reg]$libreg
      
      # LA TABLE
      
      my_table <- vals_reac()
      if(input$choix_ps=="mg"){
        load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
        my_table <- merge(my_table,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
        setnames(my_table,"my_reg_TVS","dansMaRegion")
        my_table <- my_table[,c("libagr","agr","libcom","depcom","dansMaRegion","population","picked_zonage")]
        
      } else if (input$choix_ps %in% c("sf","inf")){
        load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
        my_table <- merge(my_table,communes_BVCV[,c("agr","libagr","libcom","depcom","my_reg_BVCV","population")],by="agr")
        my_table <- merge(my_table, unique(tableau_reg()[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg)]),by="agr")
        my_table$echangeable = my_table$ZE_UD + my_table$ZE_OD
        my_table$enVigueurAutreReg = !is.na(my_table$en_vigueur_autre_reg)
        setnames(my_table,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
        my_table <- my_table[,c("libagr","agr","libcom","depcom","dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg","population","picked_zonage")]
        my_table$estMajoritaire = ifelse(my_table$estMajoritaire,"oui","non")
        my_table$echangeable = ifelse(my_table$echangeable>0,"oui","non")
        my_table$enVigueurAutreReg = ifelse(my_table$enVigueurAutreReg,"oui","non")
      }
      my_table$dansMaRegion = ifelse(my_table$dansMaRegion,"oui","non")

      
      my_table <- my_table%>%mutate_if(is.factor,as.character)
      if(input$choix_ps%in%c("sf","inf")){
        print(table(my_table$picked_zonage))
        my_table <- my_table %>% mutate(picked_zonage=case_when(
          picked_zonage=="VUD"~"Très sous-doté",
          picked_zonage=="UD"~"Sous-doté",
          picked_zonage=="Int"~"Intermédiaire",
          picked_zonage=="VD"~"Très doté",
          picked_zonage=="OD"~"Sur-doté"))
      } else if (input$choix_ps == "mg"){
        print(table(infos$picked_zonage))
        my_table <- my_table %>% mutate(picked_zonage=case_when(
          picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
          picked_zonage=="HV"~"Hors-vivier",
          picked_zonage=="Non-spécifié"~"Non-spécifié",
          picked_zonage=="ZV"~"Zone de vigilance",
          picked_zonage=="ZAC"~"Zone d'action complémentaire",
          picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
        ))
      }
      
      
      
      my_table <- my_table%>%data.table
      if(input$choix_ps%in%c("sf","inf")){
        my_table[dansMaRegion=="oui"&estMajoritaire=="non"&echangeable=="oui"&enVigueurAutreReg=="non", picked_zonage:="*Intermédiaire*"]
      }
      my_table2 = data.table()
      my_table3 = data.table()
      if(input$choix_ps=="mg"){
        names(my_table) <- c("Nom du territoire de vie-santé",
                             "Code du territoire de vie-santé",
                             "Nom de la commune",
                             "Code de la commune",
                             "dansMaRegion",
                             # "estMajoritaire",
                             # "echangeable",
                             # "enVigueurAutreReg",
                             "Population",
                             "Zonage")
        my_table$dep = substr(my_table$`Code de la commune`,1,2)
        setorder(my_table,"dep","Nom de la commune")
        my_table$dep=NULL
        
        my_table$dansMaRegion=NULL

        my_table2 = merge(hist_qpv()[,c("cod","libqpv","agr","pop")],zonage_qpv(),by="cod")
        setnames(my_table2,"picked_zonage","picked_zonage_qpv")
        
        my_table2 <- my_table2 %>% mutate(picked_zonage_qpv=case_when(
          picked_zonage_qpv=="Erreur TVS-COM"~"Erreur TVS-COM",
          picked_zonage_qpv=="HV"~"Hors-vivier",
          picked_zonage_qpv=="Non-spécifié"~"Non-spécifié",
          picked_zonage_qpv=="ZV"~"Zone de vigilance",
          picked_zonage_qpv=="ZAC"~"Zone d'action complémentaire",
          picked_zonage_qpv=="ZIP"~"Zone d'intervention prioritaire"
        ))
        
        my_table2 = merge(my_table2,vals_reac(),by="agr")

        my_table2 <- my_table2 %>% mutate(picked_zonage=case_when(
          picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
          picked_zonage=="HV"~"Hors-vivier",
          picked_zonage=="Non-spécifié"~"Non-spécifié",
          picked_zonage=="ZV"~"Zone de vigilance",
          picked_zonage=="ZAC"~"Zone d'action complémentaire",
          picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
        ))
        
        
        my_table2 = my_table2[picked_zonage_qpv!=picked_zonage,c("libqpv","cod","pop","picked_zonage_qpv","picked_zonage")]
        names(my_table2) <- c("Nom du QPV","Code du QPV","Population","Zonage QPV","Zonage du TVS")

      } else if(input$choix_ps=="inf"){
        names(my_table) <- c("Nom du bassin de vie ou pseudo-canton",
                             "Code du bassin de vie ou pseudo-canton",
                             "Nom de la commune",
                             "Code de la commune",
                             "dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg",
                             "Population",
                             "Zonage")
        my_table$dep = substr(my_table$`Code de la commune`,1,2)
        setorder(my_table,"dep","Nom de la commune")
        my_table2 = my_table[dansMaRegion=="oui"&estMajoritaire=="non",c(1,2,3,4,10),with=F]
        my_table3 = my_table[dansMaRegion=="non"&estMajoritaire=="oui",c(1,2,3,4,10),with=F]
        # my_table4 = my_table[dansMaRegion=="non"&estMajoritaire=="non"]
        my_table = my_table[dansMaRegion=="oui"&estMajoritaire=="oui",c(1,2,3,4,10),with=F]
        # fwrite(my_table,"data/test_arrete_kable_data.csv")
        # fwrite(my_table3,"data/test2_arrete_kable_data.csv")
        
        
      } else if(input$choix_ps=="sf"){
        my_table[,population:=NULL]
        my_table = merge(my_table,pop_femmes(),by.x="depcom",by.y="CODGEO",all.x=T)
        names(my_table) <- c("Code de la commune","Nom du bassin de vie ou pseudo-canton",
                             "Code du bassin de vie ou pseudo-canton",
                             "Nom de la commune",
                             "dansMaRegion","estMajoritaire","echangeable","enVigueurAutreReg",
                             "Zonage","Population_femmes")
        my_table$dep = substr(my_table$`Code de la commune`,1,2)
        setorder(my_table,"dep","Nom de la commune")
        my_table2 = my_table[dansMaRegion=="oui"&estMajoritaire=="non",c(2,3,1,4,9),with=F]
        my_table3 = my_table[dansMaRegion=="non"&estMajoritaire=="oui",c(2,3,1,4,9),with=F]
        my_table = my_table[dansMaRegion=="oui"&estMajoritaire=="oui",c(2,3,1,4,9),with=F]
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
      } else if (input$choix_ps == "mg"){
        print(table(infos$picked_zonage))
        infos <- infos %>% mutate(picked_zonage=case_when(
          picked_zonage=="Erreur TVS-COM"~"Erreur TVS-COM",
          picked_zonage=="HV"~"Hors-vivier",
          picked_zonage=="Non-spécifié"~"Non-spécifié",
          picked_zonage=="ZV"~"Zone de vigilance",
          picked_zonage=="ZAC"~"Zone d'action complémentaire",
          picked_zonage=="ZIP"~"Zone d'intervention prioritaire"
        ))
      }
      
      
      infos <- infos %>% mutate(nom_zonage=paste0(libagr,": ",picked_zonage))
      contours_reg=merge(fond_de_carte()[,c("agr","geometry")],infos,by.x="agr",
                         by.y="agr",all.x=T)
      contours_reg <- contours_reg %>% arrange(-population)
      if(input$choix_ps=="mg"){
        # lev = c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
        lev = c("Erreur TVS-COM","Hors-vivier","Non-spécifié","Zone de vigilance","Zone d'action complémentaire","Zone d'intervention prioritaire")
        
      }else{
        lev = c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")
      }
      contours_reg$picked_zonage=factor(contours_reg$picked_zonage,
                                        levels = lev)
      nb_per_zonage = data.table(contours_reg)[,.SD[1],by="agr"][,.N,by="picked_zonage"]
      setkey(nb_per_zonage,"picked_zonage")
      nb_per_zonage = nb_per_zonage[lev]
      nb_per_zonage[is.na(N),N:=0]
      nb_per_zonage$zonage_nb = paste0(nb_per_zonage$picked_zonage," (",nb_per_zonage$N,")")
      
      contours_reg = merge(contours_reg,nb_per_zonage[,c("picked_zonage","zonage_nb")],by="picked_zonage")
      
      # factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
      # }else{c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
      # contours_reg$picked_zonage,alpha=.3)
      if(input$choix_ps=='mg'){
        my_colors <- c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
      } else if (input$choix_ps %in% c("sf","inf")){
        my_colors <- c('#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
      }
      # names(my_colors) <- if(input$choix_ps=='mg'){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
      # }else{c("Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}
      names(my_colors) <- nb_per_zonage$zonage_nb
      # st_crs(contours_reg) <- 4326
      
      # https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
      g <- ggplot(data=contours_reg)+ 
        ggspatial::annotation_map_tile(zoomin = 0)+
        geom_sf(aes(fill=zonage_nb),alpha=.5)+theme(legend.title = element_blank())+
        labs(caption=paste0("Source : COG ",year(Sys.Date()),", date : ",format.Date(Sys.Date(),"%d/%m/%Y")))+
        # geom_sf(data=reg_cont[reg_cont$reg==input$choix_reg,],aes(fill=NA),color="black",show.legend = F,lwd=2)+
        ggrepel::geom_label_repel(
          data = head(contours_reg,20),
          # data = contours_reg,
          # aes(label = nom_zonage, geometry = geometry),
          aes(label = libagr, geometry = geometry),
          stat = "sf_coordinates",
          min.segment.length = 0
        )+ 
        theme(axis.title.x = element_blank(),axis.title = element_blank())+
        scale_fill_manual(aesthetics = "fill",values=my_colors)+
        blank() +
        north(contours_reg,location = "bottomleft") +
        scalebar(contours_reg, dist = 20, dist_unit = "km",
                 transform = TRUE, model = "WGS84")        
      if(input$choix_ps=="mg"){g <- g + ggtitle(paste0("Zonage médecin - ",reg_name))
      }else if(input$choix_ps=="sf"){g <- g + ggtitle(paste0("Zonage sage-femme - ",reg_name))
      }else if(input$choix_ps=="inf"){g <- g + ggtitle(paste0("Zonage infirmier - ",reg_name))}
      # print(input$add_annexes)
      # Set up parameters to pass to Rmd document
      params <- list(REGION_NAME = reg_name,
                     DATE_NOMINATION_DG_ARS=input$DATE_DG_ELECTION%>%jour_nommois_annee,
                     DATE_DEBUT_DG_ARS=input$DATE_DG_EFFET%>%jour_nommois_annee,
                     NOM_DG_ARS = input$NOM_DG_ARS,
                     GENRE_DG_ARS = input$GENRE_DG_ARS,
                     DATE_PRECEDENT_ARRETE=input$DATE_LAST_ARRETE%>%jour_nommois_annee,
                     # DATE_DECISION_ARS_ZONAGE=input$DATE_NOUVEL_ARRETE%>%jour_nommois_annee,
                     OBJ_LAST_ARRETE=input$OBJ_LAST_ARRETE,
                     DATE_DECISION_CONF_SANTE_AUTO=input$DATE_CONF_SANTE_AUTO%>%jour_nommois_annee,
                     DATE_DECISION_UNION_REG_PS=input$DATE_UNION_REG_PS%>%jour_nommois_annee,
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
                     TABLE2 = my_table2,
                     TABLE3 = my_table3,
                     CARTE=g,
                     VILLE_REDACTION=input$VILLE_TRIBUNAL_ADMINISTRATIF,
                     TODAY=Sys.Date()%>%jour_nommois_annee,
                     VILLE_REGION_TRIBUNAL=input$VILLE_TRIBUNAL_ADMINISTRATIF,
                     ADD_ANNEXES = input$add_annexes
                     
      )
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
  }
)

observeEvent(input$generate_arrete,{
  my_TAs=TA()[reg%in%input$choix_reg]$TA
  if(input$choix_ps%in%c("mg","inf","sf")){
    showModal(modalDialog(title="Informations relatives à l'arrêté",
                          size="l",easyClose = T,footer=NULL,
                          fluidRow(
                            column(6,
                                   textInput("NOM_DG_ARS","Directeur.rice Général.e",placeholder = "Mme/M. X"),
                                   textInput("GENRE_DG_ARS","H / F",placeholder = "F"),
                                   dateInput("DATE_DG_ELECTION","Date du décret de nomination du DG",startview = "decade", language = "fr",value = Sys.Date()-100,format = "dd-mm-yyyy"),
                                   dateInput("DATE_DG_EFFET","Date de prise de fonction du DG",startview = "decade", language = "fr",value = Sys.Date()-30,format = "dd-mm-yyyy"),
                                   dateInput("DATE_LAST_ARRETE","Date du précédent arrêté",startview = "decade", language = "fr",value = Sys.Date()-400,format = "dd-mm-yyyy"),
                                   textInput("OBJ_LAST_ARRETE","Objet du précédent arrêté",placeholder = "(relatif à) ...")
                            ),
                            column(6,
                                   # dateInput("DATE_NOUVEL_ARRETE","Date du nouvel arrêté",startview = "decade", language = "fr",value = Sys.Date(),format = "dd-mm-yyyy"),
                                   dateInput("DATE_CONF_SANTE_AUTO","Date avis de la conférence régionale de la santé et de l’autonomie ",startview = "decade", language = "fr",value = Sys.Date(),format = "dd-mm-yyyy"),
                                   dateInput("DATE_UNION_REG_PS","Date avis de l’union régionale des professionnels de santé",startview = "decade", language = "fr",value = Sys.Date(),format = "dd-mm-yyyy"),
                                   selectInput("VILLE_TRIBUNAL_ADMINISTRATIF","Ville du Tribunal Administratif de région",choices=my_TAs,selected=my_TAs[1]),
                                   # column(6,tags$div(id="loading")),
                                   # column(6,
                                   checkboxGroupInput("add_annexes",label = "Annexes à inclure",
                                                      choices = c("Tableaux","Cartes"),selected = c("Tableaux","Cartes")),
                                   
                                   downloadButton(outputId="download_arrete",label="Arrêté"),
                                   div(style="display: table-cell;vertical-align: middle",HTML("Le rapport proposé en téléchargement est au format .docx Microsoft Word afin de pouvoir être relu et complété."))
                                   # )
                            ))))
  } else {
    showNotification(duration = NULL,type = "warning",ui = HTML("<b>Attention</b> la génération de l'arrêté n'est pas encore disponible pour cette profession, n'hésitez pas à nous solliciter (Blandine Legendre, Clémence Lamoril et Philéas Condemine) pour en savoir plus sur l'avancement du projet."))
  }
})
