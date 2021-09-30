



get_millesimes = function(input,session,millesimes_reac,info_region,dropbox_ps_folder){
  message("func : get_millesimes")
  
  if(!is.null(input$choix_reg) & !is.null(input$choix_ps) & !is.null(dropbox_ps_folder)){
    input$refresh_millesime
    my_reg=input$choix_reg
    reg_name=info_region[reg==my_reg]$libreg
    regex = paste0(input$choix_ps,'_',input$choix_reg,'_')
    reg_files = drop_dir(dropbox_ps_folder)
    
    if (!is.null(reg_files)){
      
      if(nrow(reg_files)>0){#premier filtre
        reg_files = data.table(reg_files)
        reg_files = reg_files[grepl(regex,name)]
        reg_files = reg_files[!grepl("en_vigueur",name)]
        reg_files = reg_files[!grepl("qpv_",name)]
        reg_files = reg_files[!grepl("justification",name)]
        print(head(reg_files))
      }
      
      if(nrow(reg_files)>0){#s'il en reste encore
        millesimes_reac(setNames(reg_files$name,
                                 reg_files$name%>%
                                   gsub(pattern = paste0(input$choix_ps,"_",input$choix_reg,"_"),replacement = "")%>%
                                   gsub(pattern = "_+",replacement = "_")%>%
                                   gsub(pattern = ".csv$",replacement = "")%>%
                                   gsub(pattern = "(^_)|(_$)",replacement = "")))
      } else  {millesimes_reac("")}
    } else {millesimes_reac("")}
    print("millesimes_reac") ; print(millesimes_reac())
    
    if(length(millesimes_reac())==1 ){
      if(millesimes_reac()==""){
        showNotification("Aucun projet en cours, merci de créer un \"nouveau projet de zonage\" en cliquant sur la disquette",
                         type = "warning",duration = 10,session = session)
      }
    }
  }
}



get_dropbox_files = function(input,dropbox_ps_folder){
  message("func : get_dropbox_files")
  req(input$choix_reg)
  req(input$choix_ps)
  req(dropbox_ps_folder)
  
  regex = paste0(input$choix_ps,'_',input$choix_reg,'_')
  reg_files = drop_dir(dropbox_ps_folder)
  reg_files = data.table(reg_files)
  if(nrow(reg_files)>0){#premier filtre
    reg_files = reg_files[grepl(regex,name)]
    reg_files = reg_files[!grepl("qpv_",name)]
  }
  if(input$choix_ps == "mg"){
    regex = paste0("qpv_",input$choix_ps,'_',input$choix_reg,'_')
    qpv_files = drop_dir(dropbox_ps_folder)
    qpv_files = data.table(qpv_files)
    if(nrow(qpv_files)>0){
      qpv_files = qpv_files[grepl(regex,name)]
    }
    
    reg_files <- rbind(reg_files,qpv_files)
  }
  
  if(nrow(reg_files)>0){#s'il en reste
    print("found google files !")
    reg_files
  } else NULL
}



###### IMPORT FILES FROM EXISTING ZONAGE ######

file_import_modal_ui = function(input,output,session,importFile_reac){
  message("func : file_import_modal_ui")
  
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
    importFile_reac(import_table)
  } else if(file_format %in% c("xls","xlsx")){
    import_table = readxl::read_excel(inFile$datapath)%>%data.table()
    importFile_reac(import_table)
    
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
        selectInput("var_hZ" ,label = "Variable Hors zonage",choices = names(import_table))
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
        selectInput("mod_hz"    , label = "Modalité hors zonage",multiple=T,choices = ""))
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
                        )),session=session)
  
}


file_import_form_dynamic_update = function(input,session,importFile_reac){
  message("func : file_import_form_dynamic_update")
  print(input$var_zonage)
  req(input$var_zonage)
  if (input$import_data_model=="melt") {
    mods_choices = unique(importFile_reac()[[input$var_zonage]])
    if(input$choix_ps == "mg"){
      
      updateSelectInput(session,"mod_zip",selected = input$mod_zip,choices = setdiff(mods_choices,c(input$mod_zac,input$mod_hz)))
      updateSelectInput(session,"mod_zac",selected = input$mod_zac,choices = setdiff(mods_choices,c(input$mod_zip,input$mod_hz)))
      updateSelectInput(session,"mod_hz" ,selected = input$mod_hz,choices = setdiff(mods_choices,c(input$mod_zac,input$mod_zip)))
      
    } else if(input$choix_ps %in% c("sf","inf")){
      updateSelectInput(session,"mod_tsd",selected = input$mod_tsd,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_sod",selected = input$mod_sod,choices = setdiff(mods_choices,c(input$mod_tsd,input$mod_int,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_int",selected = input$mod_int,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_tsd,input$mod_td,input$mod_sud)))
      updateSelectInput(session,"mod_td" ,selected = input$mod_td,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_tsd,input$mod_sud)))
      updateSelectInput(session,"mod_sud",selected = input$mod_sud,choices = setdiff(mods_choices,c(input$mod_sod,input$mod_int,input$mod_td,input$mod_tsd)))
      
    }
  }
}

file_import_validate_join_update = function(input,output,session,importFile_reac,dropbox_ps_folder,millesimes_reac){
  message("func : file_import_validate_join_update")
  
  my_data = importFile_reac()
  req(my_data)
  insertUI(session=session,selector = "#parse_file",where = "beforeBegin",immediate = T,ui = tags$div(id="loading",class="loading_spinner"))
  removeUI(session = session,selector = "#parse_file",immediate = T)
  removeUI(session = session,selector = "#shiny-modal button.btn",immediate = T)
  
  if(input$import_data_model=="melt"){
    setnames(my_data,c(input$var_agr,input$var_zonage),c("agr","picked_zonage"))
    my_data = my_data[,c("agr","picked_zonage")]
    if (input$choix_ps=="mg"){
      my_data$agr = paste0(input$choix_reg,"x",my_data$agr)
      my_data[picked_zonage%in%input$mod_zip]$picked_zonage <- "ZIP"
      my_data[picked_zonage%in%input$mod_zac]$picked_zonage <- "ZAC"
      my_data[picked_zonage%in%input$mod_hz]$picked_zonage <- "HZ"
    } else if (input$choix_ps %in% c("sf","inf")){
      my_data[picked_zonage%in%input$mod_tsd]$picked_zonage <- "VUD"
      my_data[picked_zonage%in%input$mod_sod]$picked_zonage <- "UD"
      my_data[picked_zonage%in%input$mod_int]$picked_zonage <- "Int"
      my_data[picked_zonage%in%input$mod_td]$picked_zonage <- "VD"
      my_data[picked_zonage%in%input$mod_sud]$picked_zonage <- "OD"
    }
  } else if (input$import_data_model=="cast"){
    if(input$choix_ps == "mg"){
      setnames(my_data,c(input$var_agr, input$var_zip, input$var_zac, input$var_hz),
               c("agr","ZIP","ZAC","HZ"))
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
  
  drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
  
  updateSelectizeInput(session,'choix_millesime',
                       choices=c(millesimes_reac(),setNames(filename,filenm_no_extension)),
                       selected=filename)
  importFile_reac(my_data)
  
  shinyjs::runjs("$('#file_import_box button.btn-box-tool').trigger('click');")
  shinyjs::runjs("$('button#go_zonage').addClass('pulse');")
  removeModal(session = session)
}

login_w_key = function(input,session,curr_millesimes,has_logged_in){
  message("func : login_w_key")
  if(input$choix_reg == 6 & input$choix_ps != "mg"){
    showNotification("Pour l'instant seule la profession de médecin généraliste est traitée dans cette application pour Mayotte",session = session)
  } else {
    
    if(is.null(input$choix_millesime)|input$choix_millesime==""){
      new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_cadre_national')
      updateSelectizeInput(session,'choix_millesime',
                           choices=c(curr_millesimes,setNames(new_mil,"cadre_national")),
                           selected=new_mil)
    }
    
    updateTabsetPanel(session,"sidebarmenu","zonage")
    
    if(!has_logged_in){
      showModal(modalDialog(title="Identification requise",easyClose = F,
                            footer=tagList(actionButton("send_pwd","Soumettre"),modalButton("Annuler")),
                            passwordInput("my_auth",label = "",placeholder = "Clef d'identification")),
                session = session)
    }
  }
}



adjust_width = function(curr_width){
  message("func : adjust_width")
  
  
  if(curr_width>0 & curr_width<12){
    shinyjs::runjs(sprintf("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(curr_width)))
    shinyjs::runjs(sprintf("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(12-curr_width)))
  } else if (curr_width==0){
    shinyjs::runjs("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).addClass('hide');")
    shinyjs::runjs(sprintf("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(12-curr_width)))
  } else if (curr_width==12){
    shinyjs::runjs("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).addClass('hide');")
    shinyjs::runjs(sprintf("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(curr_width)))
  }
}
