function(input, output,session) {
  
  # init_gs=reactiveVal(T)
  # zonage_reactif=reactiveVal("")
  fond_de_carte=reactiveVal(F)
  map_coord=reactiveVal(c(0,45))
  timer=reactiveVal(Sys.time())
  timer_qpv=reactiveVal(Sys.time())
  # no_archive=reactiveVal(T)
  millesimes=reactiveVal(NULL)
  autorefresh <- reactiveTimer(5000)
  new_modifs <- reactiveVal(0)
  new_modifs_qpv <- reactiveVal(0)
  has_logged_in <- reactiveVal(F)
  edition_forced <- reactiveVal(c())
  last_arg_clicked <- reactiveVal("")
  modif_zonage_qpv <- reactiveVal(list(cod="",picked_zonage=""))
  last_zonage_tvs <- reactiveVal(NULL)
  # google_files <- reactiveVal(NULL)
  default_vals <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  current_mapped_data <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  
  output$rappel_ps = renderUI({
    if(!is.null(input$choix_ps)&!is.null(input$choix_millesime)){
      tagList(tags$h2(paste0(
        "Projet de zonage des ",
        names(list_PS[list_PS==input$choix_ps])[1])),
        tags$i(paste0("Intitulé : ",
                      input$choix_millesime
        )))
    } else NULL
  })
  
  
  
  source("utils/import_file.R",local=T,encoding = "UTF-8")
  
  ###### TABLEAU ######
  
  source("utils/tableau_agr.R",local=T,encoding = "UTF-8")  
  
  ###### CARTE ######
  
  source("utils/carte_agr.R",local=T,encoding = "UTF-8")  
  
  #### EXPORTS ####
  source("utils/boutons_export.R",local=T,encoding = "UTF-8")
  
  # #### Persistance ####
  
  source("utils/persistance.R",local=T,encoding = "UTF-8")
  
  
  #### Jauges ####
  
  source("utils/jauges.R",local=T,encoding = "UTF-8")
  
  ##### Dist zonage #####
  
  source("utils/distribution_zonages_pop.R",local=T,encoding = "UTF-8")
  
  #### QPV ####
  
  source("utils/qpv.R",local=T,encoding = "UTF-8")
  
  ##### Recap modif #####
  
  output$recap_dt = renderDataTable({
    infos <- vals_reac()
    if(input$choix_ps=="mg"){
      #### TVS
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,en_vigueur_autre_reg,CN)]),by="agr")
      setnames(infos,"my_reg_TVS","dansMaRegion")
      infos = data.table(infos)
      
      infos = merge(infos,VZN[,.SD[1],by="tvs"],by.x="agr",by.y="tvs",all.x=T)
      infos = infos[(zonage_ars!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
      
      #### QPV
      
      infos_zonage_qpv = merge(hist_qpv[,c("cod","libqpv","agr","pop")],zonage_qpv(),by="cod")
      setnames(infos_zonage_qpv,"picked_zonage","picked_zonage_qpv")
      infos_zonage_qpv = merge(infos_zonage_qpv,unique(tableau_reg()[,.(agr,CN)]),by="agr")
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
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg,CN)]),by="agr")
      # infos$echangeable = infos$ZE_UD + infos$ZE_OD
      # infos$enVigueurAutreReg = !is.na(infos$en_vigueur_autre_reg)
      # setnames(infos,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
      infos = data.table(infos)
      infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
      
    }
    # infos = data.table(infos)
    # infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
    setnames(infos,c("libagr","agr","picked_zonage","CN","population"),c("Libelle","Code","Zonage","Cadre National","Population"))
    if(nrow(infos)>0){
      datatable(infos,
                rownames=F,
                options=list(
                  dom = "t"
                ))
    } else NULL
  })
  
  vals_reac=reactive({
    if(has_logged_in()){
      print("get vals_reac")
      my_agr=tableau_reg()[["agr"]]
      my_agr=as.character(my_agr)
      vals <- sapply(my_agr,function(i) input[[i]])
      
      
      vals=vals[sapply(vals,length)>0]
      req(length(vals)>0)
      vals=stack(vals)
      names(vals) <- c("picked_zonage","agr")
      vals <- vals %>% mutate_if(is.factor,as.character)
      print("get vals_reac OK")
      vals
    } else NULL
  })
  
  source("utils/navigation.R",local=T,encoding = "UTF-8")
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
  
  output$ui_doc_dl = renderUI({
    if(!is.null(input$choix_ps)&!is.null(input$choix_reg)){
      if(input$choix_ps=="mg"){
        
        tagList(
          downloadButton("dl_ref_zonage_med","Fichier réf. zonage médecin", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_tvs_com","Corres. TVS - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_tvs","Population jauges", style = "width:230px;color:#000")
        )
      } else if(input$choix_ps=="sf"){
        
        tagList(
          downloadButton("dl_faq_hors_mg","FAQ", style = "width:230px;color:#000"),br(),
          downloadButton("dl_ref_zonage_sf","Fichier réf. zonage SF", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_bvcv_com","Corres. BVCV - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_bvcv_femmes","Population jauges (femmes)", style = "width:230px;color:#000")
          
        )
      } else  if(input$choix_ps=="inf"){
        tagList(
          downloadButton("dl_faq_hors_mg","FAQ", style = "width:230px;color:#000"),br(),
          downloadButton("dl_ref_zonage_ide","Fichier réf. zonage IDE", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_bvcv_com","Corres. BVCV - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_bvcv_all","Population jauges", style = "width:230px;color:#000")
          
        )
      } else NULL
      
    } else NULL
    
    
  })
  
  
  
  
  output$dl_ref_zonage_med <- downloadHandler(
    filename = 'ref_zonage_medecin.xlsx',
    content = function(file) {
      showNotification("Ce fichier contient également la population des QPV")
      
      file.copy("data/Zonage_medecin_20190703.xlsx", file, overwrite = T)
    }
  )
  
  
  output$dl_corres_tvs_com <- downloadHandler(
    filename = 'corrs_tvs_com.sas7bdat',
    content = function(file) {
      file.copy("data/tvs2019.sas7bdat", file, overwrite = T)
    }
  )
  
  output$dl_pop_tvs <- downloadHandler(
    filename = 'pop_tvs.xlsx',
    content = function(file) {
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      pop_tvs = data.table(communes_TVS)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_tvs) <- c("Région","Département","TVS","Nom TVS","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_tvs,file)
      
      # file.copy("data/tvs2019.sas7bdat", file, overwrite = T)
    }
  )
  
  output$dl_ref_zonage_sf <- downloadHandler(
    filename = 'ref_zonage_sf..sas7bdat',
    content = function(file) {
      file.copy(paste0("data/",input$choix_ps,"/cadre_nat_",input$choix_ps,".sas7bdat"), file, overwrite = T)
    }
  )
  
  output$dl_ref_zonage_ide <- downloadHandler(
    filename = 'ref_zonage_ide..sas7bdat',
    content = function(file) {
      file.copy(paste0("data/",input$choix_ps,"/cadre_nat_",input$choix_ps,".sas7bdat"), file, overwrite = T)
    }
  )
  
  
  output$dl_corres_bvcv_com <- downloadHandler(
    filename = 'corrs_bvcv_com.sas7bdat',
    content = function(file) {
      file.copy("data/bvcv2019.sas7bdat", file, overwrite = T)
    }
  )
  
  output$dl_pop_bvcv_femmes <- downloadHandler(
    filename = 'pop_bvcv_femmes.xlsx',
    content = function(file) {
      openxlsx::write.xlsx(pop_femmes,file)
    }
  )
  
  output$dl_pop_bvcv_all <- downloadHandler(
    filename = 'pop_bvcv.xlsx',
    content = function(file) {
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      pop_bvcv = data.table(communes_BVCV)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_bvcv) <- c("Région","Département","BVCV","Nom BVCV","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_bvcv,file)
      
    }
  )
  
  
  output$gauges = renderUI({
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
    
  })  
  
  output$ui_go_zonage = renderUI({
    req(input$choix_millesime)
    
    box(width = 12, 
        actionBttn(
          inputId = "go_zonage",
          label = "Accéder au formulaire de zonage",
          color = "success",size = "lg",
          style = "material-flat",
          icon = icon("door-open"),
          block = TRUE
        )
    )
  })
  
  
  output$dl_faq_hors_mg <- downloadHandler(
    filename = 'FAQ_hors_mg.pdf',
    content = function(file) {
      file.copy("www/FAQ_hors_mg.pdf", file, overwrite = T)
      
    }
  )
  
}