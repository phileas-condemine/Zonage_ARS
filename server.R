function(input, output,session) {
  
  # init_gs=reactiveVal(T)
  # zonage_reactif=reactiveVal("")
  fond_de_carte=reactiveVal(F)
  map_coord=reactiveVal(c(0,45))
  timer=reactiveVal(Sys.time())
  # no_archive=reactiveVal(T)
  millesimes=reactiveVal(NULL)
  autorefresh <- reactiveTimer(5000)
  new_modifs <- reactiveVal(0)
  # google_files <- reactiveVal(NULL)
  default_vals <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  current_mapped_data <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  
  
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
  
  ##### Recap modif #####
  
  output$recap_dt = renderDataTable({
    # browser()
    infos <- vals_reac()
    if(input$choix_ps=="mg"){
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
      setnames(infos,"my_reg_TVS","dansMaRegion")
    } else if (input$choix_ps %in% c("sf","inf")){
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      infos <- merge(infos,communes_BVCV[,c("agr","libagr","libcom","depcom","my_reg_BVCV","population")],by="agr")
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg,CN)]),by="agr")
      # infos$echangeable = infos$ZE_UD + infos$ZE_OD
      # infos$enVigueurAutreReg = !is.na(infos$en_vigueur_autre_reg)
      # setnames(infos,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
    }
    infos = data.table(infos)
    infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
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
  })
  
  source("utils/navigation.R",local=T,encoding = "UTF-8")
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
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
  
}