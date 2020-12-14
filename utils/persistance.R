observeEvent(input$save_latest_check,{
  req(input$choix_reg)
  req(input$save_latest_check)
  insertUI(session=session,selector = "#save_latest_check",where = "beforeBegin",immediate = T,ui = tags$div(id="loading"))
  removeUI(session = session,selector = "#save_latest_check",immediate = T)
  removeUI(session = session,selector = "#shiny-modal button.btn",immediate = T)
  print("Persistance")
  
  has_recap_w_missing_justification = F
  if(!is.null(info_recap_reac())){
    if(nrow(info_recap_reac())>0){
      filename = paste0("justification_",input$choix_millesime,".csv")
      print("filename")
      print(filename)
      if(!filename%in%list.files("data")){
        has_recap_w_missing_justification = T
      }
    }
  }
  if(input$choix_ps=="mg" & has_recap_w_missing_justification){
    
    
    showModal(modalDialog(title="Merci de vérifier le contenu du champ \"justification du zonage pris\" avant de valider votre zonage.",
                          "Le zonage n'a pas été sauvegardé, il pourra l'être une fois que vous aurez ajouté/relu le champ \"justification du zonage pris\" en cliquant sur le bouton éponyme situé au dessus du tableau de zonage.",
                          footer = modalButton("J'ai compris"),easyClose = T,size="m"))
    
    
  } else {
    
    
    
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    my_dt=merge(tableau_reg()[,"agr"],
                vals_reac(),
                by="agr",all.x=T)
    my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    setorder(my_dt,agr)
    sheet_name=paste("en_vigueur",input$choix_ps,input$choix_reg,sep="_")
    filename = paste0(sheet_name,".csv")
    local_name=paste0("data/",filename)
    fwrite(unique(my_dt),file=local_name)
    
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
    
    
    if(input$choix_ps=="mg"){
      #### add qpv
      save_qpv = paste("en_vigueur","qpv",input$choix_ps,input$choix_reg,sep="_")
      filename = paste0(save_qpv,".csv")
      local_qpv = paste0("data/",filename)
      file.copy(paste0("data/qpv_",input$choix_millesime,".csv"),local_qpv,overwrite = T)
      drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
      
      
      #### add justification
      if(!is.null(info_recap_reac())){
        if(nrow(info_recap_reac())>0){

          filename_millesimed = paste0("justification_",input$choix_millesime,".csv")
          if(filename_millesimed%in%list.files("data")){
            save_justification = paste("en_vigueur","justification",input$choix_ps,input$choix_reg,sep="_")
            filename = paste0(save_justification,".csv")
            local_justification = paste0("data/",filename)
            file.copy(paste0("data/",filename_millesimed),local_justification,overwrite = T)
            drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
            # } else {
            #   showModal(modalDialog(title="Merci de vérifier le contenu du champ \"justification du zonage pris\" avant de valider votre zonage.",
            #                         "Le zonage n'a pas été sauvegardé, il pourra l'être une fois que vous aurez ajouté/relu le champ \"justification du zonage pris\" en cliquant sur le bouton éponyme situé au dessus du tableau de zonage."))
            # }
          }
        }
      }
      
      
      
      timer_qpv(Sys.time())
      new_modifs_qpv(0)
      
    }
    
    
    
    if(session$clientData$url_pathname=="/Zonage_ARS/"){
      message=sprintf("App:ZonageARS\nEvent: la région %s vient de valider une zonage *en vigueur* pour les %s !",input$choix_reg,input$choix_ps)
      slackr_setup(config_file = "www/slackr_config.txt",echo = F)
      slackr_bot(message)
    }
    
    removeModal()
  }
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
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    my_dt=merge(tableau_reg()[,"agr"],
                vals_reac(),
                by="agr",all.x=T)
    my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    setorder(my_dt,agr)
    filename=input$choix_millesime
    local_name=paste0("data/",filename)
    fwrite(unique(my_dt),file=local_name)
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
    
    timer(Sys.time())
    new_modifs(0)
  }
  
  
  if((((difftime(Sys.time(),timer_qpv(),units = "sec") > 20))&new_modifs_qpv()>0)){
    filename = paste0("qpv_",input$choix_millesime,".csv")# le fichier est déjà enregistré à chaque modif d'un QPV !
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder())
    
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