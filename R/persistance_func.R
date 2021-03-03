
save_zonage = function(input,
                       session,
                       regions_reac,
                       vals_reac,
                       timer_reac,
                       timer_qpv_reac,
                       last_force_save_reac,
                       new_modifs_reac,
                       new_modifs_qpv_reac,
                       tableau_reg,
                       dropbox_ps_folder) {
  message("func : save_zonage")
  
  req(input$choix_reg)
  req(!is.null(input$force_save))
  print(input$force_save)
  if(((difftime(Sys.time(),timer_reac(),units = "sec") > 20)|(input$force_save!=last_force_save_reac()))&new_modifs_reac()>0){
    print("Persistance force_save or autorefresh")
    last_force_save_reac(input$force_save)
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    my_dt=merge(tableau_reg[,"agr"],
                vals_reac(),
                by="agr",all.x=T)
    my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    setorder(my_dt,agr)
    filename=input$choix_millesime
    local_name=paste0("data/",filename)
    fwrite(unique(my_dt),file=local_name)
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
    
    timer_reac(Sys.time())
    new_modifs_reac(0)
  }
  
  
  if((((difftime(Sys.time(),timer_qpv_reac(),units = "sec") > 20)) & new_modifs_qpv_reac()>0)){
    print("Persistance QPV")
    filename = paste0("qpv_",input$choix_millesime,".csv")# le fichier est déjà enregistré à chaque modif d'un QPV !
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
    
    timer_qpv_reac(Sys.time())
    new_modifs_qpv_reac(0)
  }
  print("Persistance OK")
  
}









confirm_save_envigueur = function(session){
  message("func : confirm_save_envigueur")
  
  showModal(
    session = session,
    modalDialog(
      title = "Valider zonage pour arrêté",
      size = "m",
      easyClose = F,
      footer = tagList(
        modalButton("Annuler"),
        actionButton(
          "save_envigueur_confirm",
          "Valider",
          icon = shiny::icon("check-double")
        )
      ),
      tags$p(
        "En validant ce menu, vous allez exporter ce zonage afin que les autres ARS puissent en tenir compte dans la publication de leurs arrêtés."
      )
    )
  )
}


save_upload_en_vigueur = function(input,
                                  session,
                                  info_recap_reac,
                                  regions_reac,
                                  vals_reac,
                                  timer_qpv_reac,
                                  new_modifs_qpv_reac,
                                  tableau_reg,
                                  log_is_admin,
                                  dropbox_ps_folder  
) {
  message("func : save_upload_en_vigueur")
  
  req(input$choix_reg)
  req(input$save_envigueur_confirm)
  insertUI(session = session,selector = "#save_envigueur_confirm",where = "beforeBegin",immediate = T,ui = tags$div(id="loading"))
  removeUI(session = session,selector = "#save_envigueur_confirm",immediate = T)
  removeUI(session = session,selector = "#shiny-modal button.btn",immediate = T)
  print("Persistance : save_envigueur_confirm")
  
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
                          footer = modalButton("J'ai compris"),easyClose = T,size="m"),session = session)
    
    
  } else {
    
    
    
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg%in%my_reg]$libreg
    my_dt=merge(tableau_reg[,"agr"],
                vals_reac(),
                by="agr",all.x=T)
    my_dt[is.na(picked_zonage)]$picked_zonage <- ""
    setorder(my_dt,agr)
    sheet_name=paste("en_vigueur",input$choix_ps,input$choix_reg,sep="_")
    filename = paste0(sheet_name,".csv")
    local_name=paste0("data/",filename)
    fwrite(unique(my_dt),file=local_name)
    
    drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
    
    
    if(input$choix_ps=="mg"){
      #### add qpv
      save_qpv = paste("en_vigueur","qpv",input$choix_ps,input$choix_reg,sep="_")
      filename = paste0(save_qpv,".csv")
      local_qpv = paste0("data/",filename)
      file.copy(paste0("data/qpv_",input$choix_millesime,".csv"),local_qpv,overwrite = T)
      drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
      
      
      #### add justification
      if(!is.null(info_recap_reac())){
        if(nrow(info_recap_reac())>0){
          
          filename_millesimed = paste0("justification_",input$choix_millesime,".csv")
          if(filename_millesimed%in%list.files("data")){
            save_justification = paste("en_vigueur","justification",input$choix_ps,input$choix_reg,sep="_")
            filename = paste0(save_justification,".csv")
            local_justification = paste0("data/",filename)
            file.copy(paste0("data/",filename_millesimed),local_justification,overwrite = T)
            drop_clean_upload(filename = filename,drop_path = dropbox_ps_folder)
            
          }
        }
      }
      
      
      
      timer_qpv_reac(Sys.time())
      new_modifs_qpv_reac(0)
      
    }
    
    
    
    if(session$clientData$url_pathname=="/Zonage_ARS/" & !log_is_admin){
      message=sprintf("App:ZonageARS\nEvent: la région %s vient de valider une zonage *en vigueur* pour les %s !",input$choix_reg,input$choix_ps)
      try({
        slackr_setup(config_file = "www/slackr_config.txt",echo = F)
        slackr_bot(message)
      })
      
      send_mail_user_validate_zonage(my_reg=input$choix_reg,my_ps=input$choix_ps,info_region = regions_reac())
      
    }
    
    removeModal(session=session)
  }
  
}
