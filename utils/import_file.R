
importFile = reactiveVal(NULL)

observeEvent(input$from_file,{
  req(input$from_file)
  inFile <- input$from_file
  print(inFile$datapath)
  file_format = stringr::str_extract(inFile$datapath,"(csv$)|(xls$)|(xlsx$)")
  print(file_format)
  if(is.null(input$choix_ps) | is.null(input$choix_reg)){
    shinyalert(title="Merci d'indiquer la profession de santé et la région avant d'importer un fichier.")
  }
  req(input$choix_ps)
  req(input$choix_reg)
  
  if(file_format == "csv"){
    import_table = fread(inFile$datapath)
    importFile(import_table)
  } else if(file_format %in% c("xls","xlsx")){
    import_table = readxl::read_excel(inFile$datapath)%>%data.table()
    importFile(import_table)
    
  } else {
    shinyalert(title="Mauvais format")
  }
  output$import_table_overview = renderDataTable(import_table,options=list(dom = "t"))
  print(input$import_data_model)
  if(input$import_data_model=="cast"){
    if(input$choix_ps == "mg"){
      inputs = tagList(
        selectInput("var_agr",label = "Variable de TVS/BVCV",choices = names(import_table)),
        selectInput("var_zip",label = "Variable ZIP",choices = names(import_table)),
        selectInput("var_zac",label = "Variable ZAC",choices = names(import_table)),
        selectInput("var_zv" ,label = "Variable Zone de vigilance",choices = names(import_table)),
        selectInput("var_hv" ,label = "Variable Hors-Vivier",choices = names(import_table))
      )
    } else if(input$choix_ps %in% c("sf","inf")) {
      inputs = tagList(
        selectInput("var_agr",label = "Variable de TVS/BVCV",choices = names(import_table)),
        selectInput("var_tsd",label = "Variable zone très sous-dotée",choices = names(import_table)),
        selectInput("var_sod",label = "Variable zone sous-dotée",choices = names(import_table)),
        selectInput("var_int",label = "Variable zone intermédiaire",choices = names(import_table)),
        selectInput("var_td" ,label = "Variable zone très dotée",choices = names(import_table)),
        selectInput("var_sud",label = "Variable zone sur-dotée",choices = names(import_table))
        
      )
    }
  } else if (input$import_data_model=="melt") {
    
    if(input$choix_ps == "mg"){
      inputs = tagList(
        selectInput("var_agr"   , label = "Variable de TVS/BVCV",choices = names(import_table),selected=names(import_table)[1]),
        selectInput("var_zonage", label = "Variable de zonage",choices = names(import_table),selected=names(import_table)[2]),
        selectInput("mod_zip"   , label = "Modalité zone d'intervention prioritaire",multiple=T,choices = ""),
        selectInput("mod_zac"   , label = "Modalité zone d'accompagnement complémentaire",multiple=T,choices = ""),
        selectInput("mod_zv"    , label = "Modalité zone de vigilance",multiple=T,choices = ""),
        selectInput("mod_hv"    , label = "Modalité hors-vivier",multiple=T,choices = ""))
    } else if(input$choix_ps %in% c("sf","inf")) {
      inputs = tagList(
        selectInput("var_agr"   , label = "Variable de TVS/BVCV",choices = names(import_table),selected=names(import_table)[1]),
        selectInput("var_zonage", label = "Variable de zonage",choices = names(import_table),selected=names(import_table)[2]),
        selectInput("mod_tsd"   ,multiple=T, label = "Modalité zone très sous-dotée",choices = ""),
        selectInput("mod_sod"   ,multiple=T, label = "Modalité zone sous-dotée",choices = ""),
        selectInput("mod_int"   ,multiple=T, label = "Modalité zone intermédiaire",choices = ""),
        selectInput("mod_td"    ,multiple=T, label = "Modalité zone très dotée",choices = ""),
        selectInput("mod_sud"   ,multiple=T, label = "Modalité sur-dotée",choices = ""))
    }
  }
  
  showModal(modalDialog(title="Analyse du fichier",
                        inputs,
                        dataTableOutput("import_table_overview"),
                        footer = tagList(
                          actionButton("parse_file","Soumettre",icon = shiny::icon("save")),
                          modalButton("Annuler",icon=shiny::icon("window-close"))
                        )))
})



observe({
  print("update mods zonage")
  print(input$var_zonage)
  req(input$var_zonage)
  if (input$import_data_model=="melt") {
    mods_choices = unique(importFile()[[input$var_zonage]])
    if(input$choix_ps == "mg"){
      
      updateSelectInput(session,"mod_zip",selected = input$mod_zip,choices = setdiff(mods_choices,c(input$mod_zac,input$mod_zv,input$mod_hv)))
      updateSelectInput(session,"mod_zac",selected = input$mod_zac,choices = setdiff(mods_choices,c(input$mod_zip,input$mod_zv,input$mod_hv)))
      updateSelectInput(session,"mod_zv" ,selected = input$mod_zv,choices = setdiff(mods_choices,c(input$mod_zac,input$mod_zip,input$mod_hv)))
      updateSelectInput(session,"mod_hv" ,selected = input$mod_hv,choices = setdiff(mods_choices,c(input$mod_zac,input$mod_zv,input$mod_zip)))
      
    } else if(input$choix_ps %in% c("sf","inf")){
      updateSelectInput(session,"mod_tsd",selected = input$mod_tsd,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_sod",selected = input$mod_sod,choices = setdiff(mods_choices,c(input$mod_tsd,input$mod_int,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_int",selected = input$mod_int,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_tsd,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_td" ,selected = input$mod_td,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_tsd,input$mod_sud)))
      updateSelectInput(session,"mod_sud",selected = input$mod_sud,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_td,input$mod_tsd)))
      
    }
  }
  
})

observeEvent(input$parse_file,{
  # MAINTENANT IL FAUT NORMALISER LE FICHIER ET VERIFIER LA JOINTURE
  my_data = importFile()
  req(my_data)
  insertUI(session=session,selector = "#parse_file",where = "beforeBegin",immediate = T,ui = tags$div(id="loading"))
  removeUI(session = session,selector = "#parse_file",immediate = T)
  removeUI(session = session,selector = "#shiny-modal button.btn",immediate = T)
  
  if(input$import_data_model=="melt"){
    setnames(my_data,c(input$var_agr,input$var_zonage),c("agr","picked_zonage"))
    my_data = my_data[,c("agr","picked_zonage")]
    if (input$choix_ps=="mg"){
      my_data[picked_zonage%in%input$mod_zip]$picked_zonage <- "ZIP"
      my_data[picked_zonage%in%input$mod_zac]$picked_zonage <- "ZAC"
      my_data[picked_zonage%in%input$mod_zv]$picked_zonage <- "ZV"
      my_data[picked_zonage%in%input$mod_hv]$picked_zonage <- "HV"
    } else if (input$choix_ps %in% c("sf","inf")){
      my_data[picked_zonage%in%input$mod_tsd]$picked_zonage <- "VUD"
      my_data[picked_zonage%in%input$mod_sod]$picked_zonage <- "UD"
      my_data[picked_zonage%in%input$mod_int]$picked_zonage <- "Int"
      my_data[picked_zonage%in%input$mod_td]$picked_zonage <- "VD"
      my_data[picked_zonage%in%input$mod_sud]$picked_zonage <- "OD"
    }
  } else if (input$import_data_model=="cast"){
    if(input$choix_ps == "mg"){
      setnames(my_data,c(input$var_agr, input$var_zip, input$var_zac, input$var_zv, input$var_hv),
               c("agr","ZIP","ZAC","ZV","HV"))
      my_data = melt(my_data,id.vars="agr",variable.factor=F,variable.name="picked_zonage")
      my_data = my_data[value==1]
      my_data[,value:=NULL]
    } else if(input$choix_ps %in% c("sf","inf")) {
      
      setnames(my_data,c(input$var_agr, input$var_tsd, input$var_sod, input$var_int, input$var_td, input$var_sud),
               c("agr","VUD","UD","Int","VD","OD"))
      my_data = melt(my_data,id.vars="agr",variable.factor=F,variable.name="picked_zonage")
      my_data = my_data[value==1]
      my_data[,value:=NULL]
    }
  }
  
  
  filenm_no_extension = gsub('(.xls$)|(.xlsx$)|(.csv$)','',input$from_file$name)
  filename = paste0(input$choix_ps,"_",input$choix_reg,"_",filenm_no_extension,".csv")
  local_filenm = paste0("data/",filename)
  fwrite(unique(my_data),local_filenm)
  
  drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
  
  updateSelectizeInput(session,'choix_millesime',
                       choices=c(millesimes(),setNames(drop_filenm,filenm_no_extension)),
                       selected=drop_filenm)
  importFile(my_data)
  shinyjs::runjs("$('#file_import_box button.btn-box-tool').trigger('click');")
  shinyjs::runjs("$('button#go_zonage').addClass('pulse');")
  removeModal()
})
