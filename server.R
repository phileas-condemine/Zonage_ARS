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
  
  observeEvent(input$choix_reg,{
    print("valeur par défaut import data model")
    print(input$import_data_model)
    output$auth=renderText({
      "KO"
    })
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
  
  importFile = reactiveVal(NULL)
  observeEvent(input$from_file,{
    req(input$from_file)
    inFile <- input$from_file
    print(inFile$datapath)
    file_format = stringr::str_extract(inFile$datapath,"(csv$)|(xls$)|(xlsx$)")
    print(file_format)
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
    local_filenm = paste0("data/",input$choix_ps,"_",input$choix_reg,"_",filenm_no_extension,".csv")
    gs_filenm = paste0(input$choix_ps,"_",input$choix_reg,"_",filenm_no_extension)
    fwrite(my_data,local_filenm)
    # gs_upload(local_filenm,gs_filenm,overwrite = T)
    drive_upload(media = local_filenm,path = gs_filenm,overwrite = T,  type = "csv")
    
    updateSelectizeInput(session,'choix_millesime',
                         choices=c(millesimes(),setNames(gs_filenm,filenm_no_extension)),
                         selected=gs_filenm)
    importFile(my_data)
    shinyjs::runjs("$('#file_import_box button.btn-box-tool').trigger('click');")
    shinyjs::runjs("$('button#go_zonage').addClass('pulse');")
    removeModal()
  })
  
  google_files = function(){
    # relevant_sheets=sapply(as.character(regions$reg),stringr::str_which,string = my_google_files$sheet_title)
    # relevant_sheets=unname(unlist(relevant_sheets))
    # my_google_files <- my_google_files[relevant_sheets,]
    # my_google_files$sheet_title
    # google_files(my_google_files)
    req(input$choix_reg)
    req(input$choix_ps)
    # input = list(choix_ps="sf",choix_reg="93")
    # reg_google_files <- gs_ls(regex = paste0('[("',input$choix_reg,'")|("',input$choix_ps,')]'))%>%
    # reg_google_files <- gs_ls(regex=c(input$choix_reg,input$choix_ps))
    reg_google_files <- drive_find(type = "csv",pattern = paste0(input$choix_ps,'_',input$choix_reg))
    
    if(nrow(reg_google_files)>0){
      print("found google files !")
      return(reg_google_files)
    } else NULL
  }
  
  output$ui_millesime=renderUI({
    req(input$choix_reg)
    req(input$choix_ps)
    input$refresh_millesime
    my_reg=input$choix_reg
    reg_name=regions[reg==my_reg]$libreg
    
    # my_google_files <- gs_ls()
    # # relevant_sheets=sapply(as.character(regions$reg),stringr::str_which,string = my_google_files$sheet_title)
    # # relevant_sheets=unname(unlist(relevant_sheets))
    # # my_google_files <- my_google_files[relevant_sheets,]
    # # my_google_files$sheet_title
    # # google_files(my_google_files)
    # 
    reg_google_files <- drive_find(type = "csv",pattern = paste0(input$choix_ps,'_',input$choix_reg))
    
    # 
    # google_files(reg_google_files)
    print(head(reg_google_files))
    if (!is.null(reg_google_files)){
    millesimes(setNames(reg_google_files$name,
                        reg_google_files$name%>%
                          gsub(pattern = input$choix_reg,replacement = "")%>%
                          gsub(pattern = input$choix_ps,replacement = "")%>%
                          gsub(pattern = "_+",replacement = "_")%>%
                          gsub(pattern = "(^_)|(_$)",replacement = "")))
    } else {millesimes("")}
    print("millesimes") ; print(millesimes())
    
    # no_archive(nrow(reg_google_files)==0)
    
    selectizeInput('choix_millesime',"",width="100%",
                   choices=millesimes(),selected=millesimes()[1],
                   options = list(placeholder = 'Dernier arrêté ou saisie en cours',
                                  plugins= list('remove_button')))
  })
  
  tableau_reg = reactive({
    
    print("tableau")
    
    req(input$choix_reg)
    
    # print("Variables à afficher") ; print(input$vars_to_show)
    # print("Largeur boxes") ; print(input$table_width)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    if(shiny_running())
      progress$set(message = "Chargement", value = 0)
    
    my_reg=input$choix_reg
    reg_name=regions[reg==my_reg]$libreg
    my_deps=dep[reg==my_reg]$dep
    
    if(input$choix_ps=="mg"){
      AGR <- TVS
      nom_fichier_dropbox <- "_preprocessed_TVS.RData"
    } else{
      AGR <- BVCV
      nom_fichier_dropbox <- "_preprocessed_BVCV.RData"
    }
    
    
    if(shiny_running())
      progress$inc(1/9, detail = "Vérification de l'historique")
    source("utils/handle_geo_data.R",local=T,encoding = "UTF-8")
    progress$inc(2/9, detail = "Récupération des données géographiques")
    get_geo_data(environment())
    
    
    
    maille_geo = ifelse(input$choix_ps=="mg","TVS","BVCV")
    historique=rdrop2::drop_history(paste0("zonage/",my_reg,"_preprocessed_",maille_geo,".RData"),dtoken=token,limit = 1)
    print(historique$server_modified)
    print(historique$client_modified)
    
    # if(shiny_running()){
      print("req choix_mil")
      req(input$choix_millesime)
      my_mil = input$choix_millesime
      # if (is.null(input$choix_millesime))
      #   my_mil = millesimes()[1]
      print("my_mil1") ; print(my_mil)
      my_mil = my_mil
    # }
    
    progress$inc(2/9, detail = "Chargement des fichiers historiques")
    if(input$choix_ps=='mg'){
      source(paste0("utils/prep_zonage_mg.R"),local=T,encoding = "UTF-8")
    }else{
      source(paste0("utils/prep_zonage_hors_mg.R"),local=T,encoding = "UTF-8")
    }
    
    
    if (shiny_running()){
      output$date_contours_update=renderText({
        paste("Date de dernière mise à jour des contours géographiques :",
              as.character(as.Date(historique$client_modified)))
      })    
      
      
    }
    
    progress$inc(2/9, detail = "Mise en forme des données de zonage")
    prep_zonage(env=environment(),
                choix_mil = my_mil,
                # no_archive=no_archive(),
                my_google_files=google_files())
    
    progress$inc(2/9, detail = "OK !")
    
    if(input$choix_ps=="mg"){
      fond_de_carte(carte_TVS)
    } else{
      fond_de_carte(carte_BVCV)
    }
    default_vals(vals)
    current_mapped_data(vals)
    
    if(input$choix_ps=="mg"){tvs}else{bvcv}
  })
  

  output$ui_vars_to_show = renderUI({
    req(input$choix_ps)
    tagList(
      # selectInput(inputId="vars_to_show",label="Variables à afficher",
      #           selected = vars_to_show_list[[input$choix_ps]],
      #           choices = vars_to_choose_from[[input$choix_ps]],
      #           multiple=T),
    sliderInput("table_width","Ajuster la table",min=0,max=12,value=8))
    
    
  })
  outputOptions(output, "ui_vars_to_show", suspendWhenHidden=FALSE)
  
  
  ###### TABLEAU ######
  
  output$zonage_dt=DT::renderDataTable(server=F,{
    # req(input$vars_to_show)
    print("DT")
    my_data=tableau_reg()
    # print(head(my_data,2))
    # req(input$display_non_modifiable)
    # if(!input$display_non_modifiable){
    #   my_data = my_data[(is_majoritaire)]
    # }
    # if (input$choix_ps %in% c("sf","inf")){
    #   my_data = my_data[ZE_UD==1|ZE_OD==1]
    # }
    
    if(input$choix_ps %in% c("sf","inf")){
      my_data[,degre_liberte := (ZE_UD+ZE_OD)*is_majoritaire]
      # setorder(my_data,-degre_liberte)
    } else if(input$choix_ps =="mg"){
      my_data[,degre_liberte := (CN=="02_Vivier")*is_majoritaire]
    }
    nb_rows = nrow(my_data)
    my_data = rbind(my_data[degre_liberte==1],my_data[degre_liberte==0],my_data[is.na(degre_liberte)])
    print(paste0("Conservation des lignes: ",round(100*nrow(my_data)/nb_rows),"%"))
    my_data[,degre_liberte:=ifelse(degre_liberte==1,"modifiable","hors-champs")]
    # req(input$vars_to_show)
    print("display DT")
    # print(head(my_data,1))
    all_vars_to_show = vars_to_show_list[[input$choix_ps]]
    container = fread(sprintf("www/sketch_%s.html",input$choix_ps),encoding="UTF-8")
    container = HTML(container[
      var %in% c("top",all_vars_to_show,"bottom")][
        match(c("top",all_vars_to_show,"bottom"), var)]$html)
    print(nrow(my_data[,all_vars_to_show,with=F]))
    
    datatable(my_data[,all_vars_to_show,with=F],
              container=container,
              class = "display hover",
              rownames=F,
              selection = 'none',
              escape=F,
              # filter = 'top',
              callback = JS(readLines("www/dt_callback.js",encoding='UTF-8')),
              extensions = c(
                'Buttons'),
              #   # ,"FixedHeader"
              #   ,"Scroller"),
              options = list(searchHighlight = TRUE, fixedHeader = TRUE,
                             # ordering=F,
                             buttons = list(list(extend = 'colvis', columns = which(all_vars_to_show %in% vars_to_toggle)-1)),
                             searchHighlight = TRUE,
                             columnDefs = list(
                               # list(targets = which(all_vars_to_show %in% c("HV","ZAC","ZIP","ZV","VUD","UD","Int","VD","OD"))-1, searchable = F),
                               # list(targets = which(all_vars_to_show %in% c("HV","ZAC","ZIP","ZV","VUD","UD","Int","VD","OD","degre_liberte"))-1, ordering = F),
                               list(targets= which(!all_vars_to_show%in%vars_to_choose_from[[input$choix_ps]])-1,visible = F)
                                               ,list(targets=which(all_vars_to_show == "communes")-1,render = JS(
                                                 "function(data, type, row, meta) {",
                                                 sprintf("return type === 'display' && data.length > %s ?",40),
                                                 sprintf("'<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;",40),
                                                 "}"
                                               ))
                                               # ,autoWidth = TRUE
                                               # ,list(width = '200px', targets = 4)
                             ),
                             search = list(regex = TRUE, caseInsensitive = FALSE),
                             language = list(
                               info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                               paginate = list(previous = 'Précédent', `next` = 'Suivant'),
                               search="Rechercher",
                               lengthMenu='Afficher _MENU_ résultats',
                               zeroRecords='Aucune zone ne correspond à ces filtres',
                               emptyTable='Tableau vide'
                             ),
                             dom = "Bft",# "Blftipr"
                             deferRender = TRUE,
                             scroller = TRUE,
                             scrollX=TRUE,
                             scrollY = 600,
                             pageLength = -1#très important pour que tous les "input" soient bien présents, nécessaire pour vals_reac() !
                             # pageLength = 10
              )
    )%>% formatStyle(
      ifelse(input$choix_ps=="mg","CN","libCN"),
      target = 'row',
      backgroundColor = if(input$choix_ps=="mg"){styleEqual(c("ZZ_Hors vivier", 
                                                              "02_Vivier",
                                                              "01_Sélection nationale"), 
                                                            c('rgba(31,120,180,0.2)', #1F78B4
                                                              'rgba(51,160,44,0.2)',#33A02C
                                                              'rgba(227,26,28,0.2)'))#E31A1C
      }else{styleEqual(c("Très sous-doté",
                         "Sous-doté", 
                         "Intermédiaire", 
                         "Très doté",
                         "Sur-doté"
      ), 
      c('rgba(31,120,180,0.2)',#1F78B4
        'rgba(178,223,138,0.2)',#B2DF8a
        'rgba(51,160,44,0.2)',#33A02C
        'rgba(251,154,153,0.2)',#FB9A99
        'rgba(227,26,28,0.2)'))}#E31A1C
    )
  })

  
  ###### CARTE ######
  
  
  output$communes_map=renderLeaflet({
    print("Carto")
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
                                      # }else{c("Choix de l'ARS majoritaire","VUD","UD","Int","VD","OD")}) 
                                      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")}) 
    print("leaflet picked zonage levels")
    print(levels(contours_reg$picked_zonage))
    # if(input$choix_ps%in%c("sf","inf")){
    #   levels(contours_reg$picked_zonage) <- c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")
    # }
    
    factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
    } else{c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},
    contours_reg$picked_zonage,alpha=.3)
    
    # contours_reg$popup = paste(contours_reg$libagr,"zonage:",
    #                            as.character(contours_reg$picked_zonage))
    
    print("Carto OK")
    leaflet(data=contours_reg) %>%
      addTiles()%>%
      addPolygons(data=contours_reg,
                  fillColor = ~factpal(picked_zonage),
                  smoothFactor = 0.2,group=~picked_zonage,
                  fillOpacity = .5,stroke = FALSE,layerId=~agr,
                  label = ~iconv(paste(libagr,"Zonage:",picked_zonage),to="UTF-8"),
                  highlightOptions = highlightOptions(fillOpacity=1,bringToFront = TRUE))%>%
      addLegend(pal = factpal, 
                values = ~picked_zonage, 
                opacity = .7,
                layerId = "legend",
                title="Zonage",
                group = "Légende")%>%
      # addLayersControl(overlayGroups = c("afficher_legende"),
      #                  options = layersControlOptions(collapsed = F))%>%
      addLayersControl(
        overlayGroups = setdiff(c("Légende",as.character(unique(contours_reg$picked_zonage))),NA),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  #### EXPORTS ####
  
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
      tempReport <- file.path(temp_dir, "create_arrete.Rmd")
      file.copy("utils/create_arrete.Rmd", tempReport, overwrite = TRUE)
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
                                   dateInput("DATE_DG_ELECTION","Date du décret de nomination du DG",startview = "decade", language = "fr",value = Sys.Date()-100,format = "dd-mm-yyyy"),
                                   dateInput("DATE_DG_EFFET","Date de prise de fonction du DG",startview = "decade", language = "fr",value = Sys.Date()-30,format = "dd-mm-yyyy"),
                                   dateInput("DATE_LAST_ARRETE","Date du précédent arrêté",startview = "decade", language = "fr",value = Sys.Date()-400,format = "dd-mm-yyyy"),
                                   dateInput("DATE_NOUVEL_ARRETE","Date du nouvel arrêté",startview = "decade", language = "fr",value = Sys.Date(),format = "dd-mm-yyyy"),
                                   selectInput("VILLE_TRIBUNAL_ADMINISTRATIF","Ville du Tribunal Administratif de région",choices=my_TAs,selected=my_TAs[1]),
                                   downloadButton(outputId="download_arrete",label="Arrêté")),
                            column(6,div(style="display: table-cell;vertical-align: middle",
                                         HTML("<p> Le rapport proposé en téléchargement est au format .docx Microsft Word afin de pour être édité.</p>
                                  <p> Dans l'article 3, des exemples de justification des choix de mise en ZIP/ZAC sont proposés mais devront être ajustés.</p>
                                  <p> En particulier si la méthodologie applicable à la détermination des zones est revue, l'article 3 devra être modifié sensiblement.</p>"))
                            ))))
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
      # contours_reg <<- contours_reg
      # missing_contours=infos_split$communes_codes[!infos_split$communes_codes%in%fond_de_carte()$code]
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
      
      # contours_reg$popup= paste(contours_reg$agr,"zonage:",
      #                           contours_reg$picked_zonage)
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
                    highlightOptions = highlightOptions(fillOpacity=1,bringToFront = TRUE))%>%
        removeControl("legend") %>% 
        addLegend(pal = factpal, data=legend_data,
                  values = ~picked_zonage, 
                  opacity = .7,
                  layerId = "legend",
                  title="Zonage",
                  group = "Légende")
        # addLegend(data = legend_data,
        #           pal = factpal,
        #           values = ~picked_zonage,
        #           opacity = .7,
        #           layerId = "legend",
        #           title="Zonage",
        #           labels=c("Très sous-dotée","Sous-dotée","Intermédiaire","Très dotée","Sur-dotée"))
      
    } else {
      print("PROBLEME AVEC CE TVS, MAL REFERENCE !")
    }
  })
  
  # #### Persistance ####
  # 
  last_force_save = reactiveVal(-1)
  observeEvent(c(autorefresh(),input$force_save),{
    req(input$choix_reg)
    req(input$force_save)
    print(input$force_save)
    if((((difftime(Sys.time(),timer(),units = "sec") > 20)|(input$force_save!=last_force_save()))&new_modifs()>0)){
      # init_gs(F)
      print("Persistance")
      last_force_save(input$force_save)
      my_reg=input$choix_reg
      reg_name=regions[reg%in%my_reg]$libreg
      # print("head(tableau_reg())")
      # print(head(tableau_reg()))
      # print("head(vals_reac())")
      # print(head(vals_reac()))
      my_dt=merge(tableau_reg()[,"agr"],
                  vals_reac(),
                  by="agr",all.x=T)
      my_dt[is.na(picked_zonage)]$picked_zonage <- ""
      setorder(my_dt,agr)
      sheet_name=input$choix_millesime
      gs_file_nm=paste0("data/",input$choix_millesime,".csv")
      # output$nb_modif_unsaved = renderText({"Sauvegarde en cours"})
      fwrite(my_dt,file=gs_file_nm)
      drive_upload(media = gs_file_nm,path = sheet_name,overwrite = T,  type = "csv")
      # gs_upload(gs_file_nm,sheet_title=sheet_name,overwrite = T)
      timer(Sys.time())
      new_modifs(0)
    }
    # print("Persistance OK")
    
  })
  
  #### Jauges ####
  
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
  
  ##### Dist zonage #####
  
  output$dist_zonages = renderPlotly({
    infos=merge(tableau_reg()[,c("agr","population","is_majoritaire")],
                vals_reac(),by="agr",all.x=T)

    infos[,is_majoritaire:=ifelse(is_majoritaire,"majoritaire","minoritaire")]
    if(input$choix_ps%in%c("sf","inf")){
      infos[,picked_zonage:=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté")]
    }
    infos[,zonage_majoritaire:=paste0(picked_zonage," - ",is_majoritaire)]
    zonage_pop=data.table(infos)[
      ,list(pop=sum(population,na.rm=T)),
      by="zonage_majoritaire"]
    print("zones x majoritaire sur piechart")
    print(unique(zonage_pop$zonage_majoritaire))
    
    if(input$choix_ps=="mg"){
      zonage_pop[,zonage_majoritaire:=factor(zonage_majoritaire,levels=c("NA - majoritaire","NA - minoritaire",
                                                                        "HV - majoritaire","HV - minoritaire",
                                                                        "ZAC - majoritaire","ZAC - minoritaire",
                                                                        "ZIP - majoritaire","ZIP - minoritaire",
                                                                        "ZV - majoritaire", "ZV - minoritaire"))]
      my_colors = data.table(zonage_majoritaire = c("NA - majoritaire","NA - minoritaire",
                             "HV - majoritaire","HV - minoritaire",
                             "ZAC - majoritaire","ZAC - minoritaire",
                             "ZIP - majoritaire","ZIP - minoritaire",
                              "ZV - majoritaire", "ZV - minoritaire"),
                             color = c('#A6CEE3','#95BDD2',
                             '#1F78B4','#0E67A3',
                             '#33A02C','#228F1B',
                             '#FB9A99','#EA8988',
                             '#E31A1C','#D2090B'))
    } else if (input$choix_ps%in%c("sf","inf")){
      zonage_pop[,zonage_majoritaire:=factor(zonage_majoritaire,levels=c("NA - minoritaire",
                                                                        "NA - majoritaire",
                                                                        "Très sous-doté - majoritaire",
                                                                        "Sous-doté - majoritaire",
                                                                        "Intermédiaire - majoritaire",
                                                                        "Très doté - majoritaire",
                                                                        "Sur-doté - majoritaire"))]
      my_colors = data.table(zonage_majoritaire = c("NA - minoritaire","NA - majoritaire",
                             "Très sous-doté - majoritaire",
                             "Sous-doté - majoritaire",
                             "Intermédiaire - majoritaire",
                             "Très doté - majoritaire",
                             "Sur-doté - majoritaire"),
                           color = c('#A6CEE3','#95BDD2',
                             '#1F78B4',
                             '#B2DF8a',
                             '#33A02C',
                             '#FB9A99',
                             '#E31A1C'))
    }
    print("actual levels")
    print(levels(zonage_pop$zonage_majoritaire))
    zonage_pop[,zonage_majoritaire:=droplevels(zonage_majoritaire)]
    zonage_pop = merge(zonage_pop,my_colors,by="zonage_majoritaire")
    tot_pop = sum(zonage_pop$pop)
    plot_ly(zonage_pop, labels = ~zonage_majoritaire, values = ~pop,hoverinfo = 'text',textinfo='label',
            text = ~paste0(zonage_majoritaire,' : ',round(100*pop/tot_pop,1),'% - soit ',pop,ifelse(input$choix_ps=="sf"," femmes"," habitants")), 
            marker = list(colors = zonage_pop$color),
            type = 'pie')%>%hide_legend()%>%
      layout(title = 'Distribution par zone de la population de la région',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  
  #### Click commune
  observeEvent(input$communes_map_shape_click,{
    req(input$communes_map_shape_click)
    my_dt_output=dataTableProxy("zonage_dt",session)
    my_agr=input$communes_map_shape_click$id
    print("my_agr") ; print(my_agr)
    my_dt_output%>%updateSearch(keywords = list(global=my_agr))
  })
  
  observeEvent(input$modifiable_only,{
    req(input$modifiable_only)
    print(input$modifiable_only)
    my_dt_output=dataTableProxy("zonage_dt",session)
    if(input$modifiable_only){
      my_dt_output%>%updateSearch(keywords = list(global="modifiable"))
    } else {
      my_dt_output%>%updateSearch(keywords = list(global=""))
      
    }
  })
  
  
  
  observeEvent(input$clicked_legend,{
    print("clicked on legend")
    print(input$clicked_legend)
  })
  
  output$choix_reg_map=renderLeaflet({
    print("create map")
    bbox_reg <- st_bbox(reg_cont[as.numeric(reg_cont$reg)>10,]) %>% 
      as.vector()
    leaflet(data = reg_cont,options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")%>%
      addPolygons(label = ~ paste(libreg,"code :",reg),
                  layerId = ~ as.numeric(reg),color=NULL)%>%
      fitBounds(bbox_reg[1]-1,bbox_reg[2],bbox_reg[3],bbox_reg[4])
  })
  
  #### Click région
  observeEvent(input$choix_reg_map_shape_click,{
    print("got clicked in map shape")
    print(input$choix_reg_map_shape_click)
    click=input$choix_reg_map_shape_click
    updateSelectizeInput(session,"choix_reg",selected=click$id)
    
  })
  
  
  
  observeEvent(input$modal_save_current,{
    showModal(modalDialog(title="Enregistrer le zonage actuel",footer=NULL,easyClose = T,size="m",
                          fluidRow(column(9,textInput("millesime_name","Nom de l'enregistrement",placeholder = "Zonage_pour_arrêté_2020")),
                                   column(3,tags$label("Enregistrer"),br(),
                                          actionButton('save_current_view',"",icon=icon("save")))),
                          helpText("Le nom de la région sera ajouté automatiquement.\nPensez à préciser la date dans le nom de l'enregistrement.\nLa date est particulièrement importante s'il s'agit d'un zonage arrêté.")))
  })
  
  observeEvent(input$save_current_view,{
    req(input$save_current_view)
    # print(input$save_current_view)
    # if(input$save_current_view>0){
    my_reg=input$choix_reg
    # reg_name=regions[reg%in%my_reg]$libreg
    # my_dt=merge(tableau_reg()[,"agr"],
    #             vals_reac(),
    #             by="agr",all.x=T)
    # my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    # setorder(my_dt,agr)
    # sheet_name=paste0(input$choix_ps,"_",input$choix_reg,"_",input$millesime_name)
    # gs_file_nm=paste0("data/",input$choix_ps,"_",input$choix_reg,"_",input$millesime_name,".csv")
    # print("write locally")
    # fwrite(my_dt,file=gs_file_nm)
    # print("upload")
    # gs_upload(gs_file_nm,sheet_title=sheet_name,overwrite = T)
    # print("update millesime input")
    new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_',input$millesime_name)
    updateSelectizeInput(session,'choix_millesime',
                         choices=c(millesimes(),setNames(new_mil,input$millesime_name)),
                         selected=new_mil)
    # print("close modal")
    removeModal()
    # }
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
  
  ####  focus map & highlight contour hovered TVS in DT  #### 
  observeEvent(input$last_row_hovered,{
    # print("row hovered")
    req(input$last_row_hovered)
    print("hovered row")
    print(input$last_row_hovered)
    # print(input$last_row_hovered)
    carte=fond_de_carte()
    # bbox_reg <- st_bbox(carte) %>% 
    #   as.vector()
    # long_spread=bbox_reg[3]-bbox_reg[1]
    # lat_spread=bbox_reg[4]-bbox_reg[2]
    if(input$last_row_hovered%in%carte$agr){
      map_proxy <- leafletProxy("communes_map",session)
      contours_reg = carte[carte$agr == input$last_row_hovered,]
      bbox <- st_bbox(contours_reg) %>% 
        as.vector()
      map_proxy %>% clearGroup("highlight_on_hover")%>%
        addPolygons(data=contours_reg,fill = F,stroke=T,opacity=1,
                    fillColor="black",group = "highlight_on_hover")# %>%
      # flyToBounds(bbox[1]-long_spread/2, bbox[2]-lat_spread/2, 
      #             bbox[3]+long_spread/2, bbox[4]+lat_spread/2)
      # setView(lng = (bbox[1] +  bbox[3]) /2,
      #         lat = (bbox[2] + bbox[4]) /2)
    }
  })
  
  
  observeEvent(input$go_zonage,{
    print("current mil selected")
    print(input$choix_millesime)
    if(is.null(input$choix_millesime)|input$choix_millesime==""){
      new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_cadre_national')
      updateSelectizeInput(session,'choix_millesime',
                           choices=c(millesimes(),setNames(new_mil,"cadre_national")),
                           selected=new_mil)
    }
    
    updateTabsetPanel(session,"sidebarmenu","zonage")
    
    showModal(modalDialog(title="Identification requise",footer=NULL,easyClose = F,
                          passwordInput("my_auth",label = "",placeholder = "Clef d'identification"),
                          actionButton("send_pwd","Soumettre")))
  })
  
  observeEvent(input$send_pwd,{
    req(input$my_auth)
    
    auth = fread("data/auth.txt")
    auth = auth[key==input$my_auth & reg == input$choix_reg]
    if(nrow(auth)>0){
      print("OK")
      output$auth=renderText({
        "OK"
      })
      outputOptions(output, "auth", suspendWhenHidden=FALSE)
      
      removeModal()
    }
    
  })
  
  
  
  observeEvent(input$go_params,{
    updateTabsetPanel(session,"sidebarmenu","my_params")
  })
  
  
  observeEvent(input$legend_click,{
    # input=list(legend_click="background:#1F78B4;opacity:0.7",choix_ps ="sf")
    if(input$choix_ps!="mg"){
      my_color = stringr::str_extract(input$legend_click,"background:#.{6};")
      my_color = gsub("(background:)|(;)","",my_color)
      
      cats=factor(if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP",NA)
      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté",NA)},
      levels = if(input$choix_ps=="mg"){c("Erreur TVS-COM","HV","Non-spécifié","ZV","ZAC","ZIP")
      }else{c("Choix de l'ARS majoritaire","Très sous-doté","Sous-doté","Intermédiaire","Très doté","Sur-doté")})
      
      factpal <- colorFactor(if(input$choix_ps=="mg"){c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')
      } else{c('#A6CEE3','#1F78B4','#B2DF8a','#33A02C','#FB9A99','#E31A1C')},cats)
      
      zone_to_color = data.table(cat = cats, color=factpal(cats))
      cat = zone_to_color[color==my_color]$cat%>%as.character
      print(cat)
      if(length(cat)==1)
        dataTableProxy("zonage_dt",session)%>%updateSearch(keywords = list(global=cat))
    }
  })
  
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
  
  #### Warning click CN ou TVS minoritaire  #### 
  # faire aussi des warnings pour SF et INF, mais empêcher complètement l'édition quand BVCV à cheval
  observeEvent(input$zonage_dt_cell_clicked,{
    req(input$zonage_dt_cell_clicked)
    print("got clicked in DT cell")
    if(length(input$zonage_dt_cell_clicked)>0){
      my_row=tableau_reg()[input$zonage_dt_cell_clicked$row]
      # print(input$zonage_dt_cell_clicked)
      # print(my_row$is_majoritaire)
      new_modifs(new_modifs()+1)
      if(!my_row$is_majoritaire){
        shinyalert("Attention!", "Vous avez sélectionné un territoire de vie-santé minoritaire en termes de population dans votre région.",
                   cancelButtonText = "Annuler",
                   confirmButtonText = "Forcer l'édition",
                   type = "error",showCancelButton = T,
                   callbackJS = sprintf("
                   function(x) { 
                     if (x == true) {
                      $('#%s .zonage_radio_button').prop('disabled',false);
                     }
                   }"
                                        ,my_row$agr))
        # shinyalert(
        #   "Enter your name", type = "input",
        #   callbackJS = "function(x) { if (x !== false) { alert('Hello ' + x); } }"
        # )
      }
      if (!is.na(my_row$CN)&my_row$CN=="01_Sélection nationale"){
        shinyalert("Attention!", "Ce territoire de vie-santé faire partie de la sélection nationale",
                   cancelButtonText = "Annuler",
                   confirmButtonText = "Forcer l'édition",
                   type = "error",showCancelButton = T,
                   callbackJS = sprintf("
                   function(x) { 
                     if (x == true) {
                      $('#%s .zonage_radio_button').prop('disabled',false);
                     }
                   }"
                                        ,my_row$agr))
      }
    }
    
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
  
  
  observeEvent(input$table_width,{
    req(input$table_width)
    if(input$table_width>0 & input$table_width<12){
    shinyjs::runjs(sprintf("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(input$table_width)))
      shinyjs::runjs(sprintf("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(12-input$table_width)))
    } else if (input$table_width==0){
      shinyjs::runjs("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).addClass('hide');")
      shinyjs::runjs(sprintf("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(12-input$table_width)))
    } else if (input$table_width==12){
      shinyjs::runjs("$('div#box_carte_jauges > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).addClass('hide');")
      shinyjs::runjs(sprintf("$('div#box_tableau > div').removeClass(function (index, className) {
    return (className.match (/col-sm-[0-9]+/) || []).join(' ');
}).removeClass('hide').addClass('col-sm-%s');",as.character(input$table_width)))
    }
    
  })
  
  # output$box_tableau = renderUI({
  #   if(input$table_width>0){
  #     box(width = input$table_width,
  #         DTOutput("zonage_dt"),
  #         tags$br(),
  #         fluidRow(
  #           column(2,actionButton("force_save","Sauvegarder",icon=shiny::icon("save"))),
  #           column(6,textOutput("nb_modif_unsaved")),
  #           column(4,tags$div(id="loading"))))
  #         } else NULL
  # })  
  # 
  # output$box_carte_jauges = renderUI({
  #   if(input$table_width<12){
  #     tagList(box(width = 12-input$table_width,
  #                 conditionalPanel("input.choix_reg !== null",
  #                                  fluidRow(
  #                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
  #                                                 downloadButton(outputId="download_plot",label="Carte"))),
  #                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
  #                                                 downloadButton(outputId="download_table",label="Tableau"))),
  #                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
  #                                                 actionButton("generate_arrete","Arrêté",icon=icon("edit"))
  #                                    ))
  #                                  ),
  #                                  
  #                                  fluidRow(
  #                                    column(12,
  #                                           leafletOutput("communes_map",width = "auto"))
  #                                  ),
  #                                  uiOutput("gauges"),
  # 
  #                                  tags$br(),
  #                                  fluidRow(
  #                                    column(8,textOutput("date_contours_update")),
  #                                    column(4,actionButton("update_contours","Mettre à jour",icon=icon("cogs")))
  #                                  ),
  #                                  tags$br(),
  #                                  fluidRow(
  #                                    column(12,plotlyOutput("dist_zonages",width="auto"))
  #                                  ),
  #                                  tags$br()
  #                                  
  #                 )
  #     ))
  #   } else NULL
  # })
  
  observeEvent(c(input$feedback_send),{
    req(input$feedback_send)
    text_to_send=input$feedback_content
    print("comment to send to slack")
    print(text_to_send)
    if(text_to_send!=""){
      adresse_mail=ifelse(is.null(input$adresse_mail),"",input$adresse_mail)
      name_sender=ifelse(is.null(input$name_sender),"",input$name_sender)
      updateTextAreaInput(session,"feedback_content",value = "")
      
      message=paste0("App:ZonageARS\n",
                     "Mail: ",adresse_mail,
                     "\nNom: ",name_sender,
                     "\nContenu: ",text_to_send)
      message=gsub("\"","*",message)
      
      
      # sendEmail(to = adresse_mail, 
      #           mail_message = sprintf("Bonjour %s,\n Merci pour votre contribution, nous allons prendre en compte vos suggestions.\n %s",
      #                                  name_sender,text_to_send))
      
      
      # slackr({message})
      slackr_bot(message)
      
      showModal(modalDialog(title="Merci pour votre commentaire !",size="s",
                            footer=NULL,easyClose = T,"Cliquer dans la zone grisée pour revenir à la liste des indicateurs de santé."))
      showNotification(ui="Merci pour votre commentaire !",duration = 5)
      shinyjs::runjs("$('.sidebar-menu > li:nth-child(5) > a').trigger('click');")
      
    } else if (text_to_send==""){
      showNotification(ui="Commentaire vide. Ecrivez quelque-chose, toute remarque est bonne à prendre !",duration = 5)
      
    }
  })
}