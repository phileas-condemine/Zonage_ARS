gen_ui_open_form_justification = function(info_recap_reac){
  message("func : gen_ui_open_form_justification")
  req(info_recap_reac())
  if(nrow(info_recap_reac())>0){
    actionButton("open_form_justification","Justification du zonage pris",icon=icon("edit"))
  }
  
}


gen_form_justification = function(input,session,info_recap_reac,dropbox_ps_folder){
  message("func : gen_form_justification")
  
  req(input$open_form_justification)
  if(input$open_form_justification){
    if (nrow(info_recap_reac())>0){
      infos = info_recap_reac()
      save_justification = paste0("justification_",input$choix_millesime,".csv")
      drop_name = paste0(dropbox_ps_folder,save_justification)
      local_name = paste0("data/",save_justification)
      
      if(!drop_exists(drop_name)){
        # INIT from file zonage 2019
        justification=data.table(time=as.character(Sys.time()),txt="")
      } else {
        # FROM SAVED
        if(!save_justification%in%list.files("data/"))
          drop_download(drop_name,local_path = "data/",overwrite = T,verbose = T)
        justification <- fread(local_name,colClasses = "character",encoding="UTF-8")
      }
      showModal(session=session,
                modalDialog(title="Explications du choix de zonage",
                            "Merci de préciser les indicateurs complémentaires utilisés pour déterminer le zonage.",
                            # "N'hésitez pas à justifier le zonage pris pour chaque TVS et chaque QPPV",
                            # "Rappel des zonages pris qui diffèrent du cadre national: ",
                            # paste(paste0(infos$libagr," (",infos$agr,") zonage choisi: ",infos$picked_zonage," (cadre national: ",infos$CN,")"),collapse=" - "),
                            textAreaInput("justification_zonage",NULL,value=justification$txt[1],placeholder = "Insérer votre texte ici...",
                                          width = "800px",height = "400px",resize = "both"),easyClose = F,size="l",
                            footer=tagList(modalButton("Annuler"),actionButton("validation_justification","Valider",icon=icon("check")))))
      
    }
  }
}

save_justification = function(input,dropbox_ps_folder){
  message("func : save_justification")
  
  req(input$justification_zonage)
  if(input$justification_zonage!=""){
    save_justification = paste0("justification_",input$choix_millesime,".csv")
    drop_name = paste0(dropbox_ps_folder,save_justification)
    local_name = paste0("data/",save_justification)
    justification=data.table(time=as.character(Sys.time()),txt=gsub('"','',input$justification_zonage))
    fwrite(justification,file=local_name)
    drop_clean_upload(filename = save_justification,drop_path = dropbox_ps_folder)
  }
  removeModal()
  
}