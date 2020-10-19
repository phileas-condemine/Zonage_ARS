observeEvent(input$save_latest_check,{
  req(input$choix_reg)
  req(input$save_latest_check)
  insertUI(session=session,selector = "#save_latest_check",where = "beforeBegin",immediate = T,ui = tags$div(id="loading"))
  removeUI(session = session,selector = "#save_latest_check",immediate = T)
  removeUI(session = session,selector = "#shiny-modal button.btn",immediate = T)
  print("Persistance")
  my_reg=input$choix_reg
  reg_name=regions[reg%in%my_reg]$libreg
  my_dt=merge(tableau_reg()[,"agr"],
              vals_reac(),
              by="agr",all.x=T)
  my_dt[is.na(picked_zonage)]$picked_zonage <- ""
  setorder(my_dt,agr)
  sheet_name=paste("en_vigueur",input$choix_ps,input$choix_reg,sep="_")
  gs_file_nm=paste0("data/",sheet_name,".csv")
  fwrite(my_dt,file=gs_file_nm)
  drive_upload(media = gs_file_nm,path = sheet_name,overwrite = T,  type = "csv")
  
  if(input$choix_ps=="mg"){
    save_qpv = sheet_name=paste("en_vigueur","qpv",input$choix_reg,sep="_")
    file_qpv = paste0("data/",save_qpv,".csv")
    drive_upload(media = file_qpv,path = save_qpv,overwrite = T,  type = "csv")
    timer_qpv(Sys.time())
    new_modifs_qpv(0)
  }
  
  removeModal()
})

last_force_save = reactiveVal(-1)
observeEvent(c(autorefresh(),input$force_save),{
  req(input$choix_reg)
  req(input$force_save)
  print(input$force_save)
  if((((difftime(Sys.time(),timer(),units = "sec") > 20)|(input$force_save!=last_force_save()))&new_modifs()>0)){
    print("Persistance")
    last_force_save(input$force_save)
    my_reg=input$choix_reg
    reg_name=regions[reg%in%my_reg]$libreg
    my_dt=merge(tableau_reg()[,"agr"],
                vals_reac(),
                by="agr",all.x=T)
    my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    setorder(my_dt,agr)
    sheet_name=input$choix_millesime
    gs_file_nm=paste0("data/",input$choix_millesime,".csv")
    fwrite(my_dt,file=gs_file_nm)
    drive_upload(media = gs_file_nm,path = sheet_name,overwrite = T,  type = "csv")
    timer(Sys.time())
    new_modifs(0)
  }
  
  
  if((((difftime(Sys.time(),timer(),units = "sec") > 20))&new_modifs_qpv()>0)){
    save_qpv = paste0("qpv_",input$choix_millesime)
    file_qpv = paste0("data/",save_qpv,".csv")
    drive_upload(media = file_qpv,path = save_qpv,overwrite = T,  type = "csv")
    timer_qpv(Sys.time())
    new_modifs_qpv(0)
  }
  # print("Persistance OK")
  
})

observeEvent(input$save_current_view,{
  req(input$save_current_view)
  my_reg=input$choix_reg
  
  if (sum(grepl("vigueur",input$millesime_name))==0){
    new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_',input$millesime_name)
    updateSelectizeInput(session,'choix_millesime',
                         choices=c(millesimes(),setNames(new_mil,input$millesime_name)),
                         selected=new_mil)
    # print("close modal")
    removeModal()
  } else {
    removeUI(selector = "#forbidden_name",immediate = T,session=session)
    insertUI(selector = "#millesime_name",where = "beforeBegin",session = session,immediate = T,
             ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b('Merci de choisir un autre nom, le mot clef "en vigueur" est réservé à la publication.'))))
  }
  # }
})

observeEvent(input$modal_save_current,{
  showModal(modalDialog(title="Enregistrer le zonage actuel",footer=NULL,easyClose = T,size="m",
                        fluidRow(column(9,textInput("millesime_name","Nom de l'enregistrement",placeholder = "Zonage_pour_arrêté_2020")),
                                 column(3,tags$label("Enregistrer"),br(),
                                        actionButton('save_current_view',"",icon=icon("save")))),
                        helpText("Le nom de la région sera ajouté automatiquement.\nPensez à préciser la date dans le nom de l'enregistrement.\nLa date est particulièrement importante s'il s'agit d'un zonage arrêté.")))
})

observeEvent(input$save_latest,{
  showModal(modalDialog(title="Valider zonage pour arrêté",size = "m",easyClose = F,footer = tagList(
    modalButton("Annuler"),actionButton("save_latest_check","Valider",icon=shiny::icon("check-double"))),
    tags$p("En validant ce menu, vous allez exporter ce zonage afin que les autres ARS puissent en tenir compte dans la publication de leurs arrêtés.")
  ))
})