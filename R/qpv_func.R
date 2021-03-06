
gen_ui_toggle_qpv = function(input){
  message("func : gen_ui_toggle_qpv")
  if(!is.null(input$choix_ps)){
    if(input$choix_ps=="mg"&!input$choix_reg%in%c("4","6")){
      shinyWidgets::switchInput(inputId = "toggle_qpv",
                                label = "Ajouter QPV",
                                value = F,
                                onLabel = "TVS+QPV",offLabel = "TVS",
                                labelWidth = "120",handleWidth = "80",
                                # onStatus = "#0f0",offStatus = "#00f",
                                size = "normal",inline = T
      )
    } else NULL
  }
}

gen_ui_search_qpv = function(input){
  message("func : gen_ui_search_qpv")
  if(!is.null(input$toggle_qpv)){
    if(input$toggle_qpv){
      actionButton("search_qpv","Rechercher un QPV",icon = icon("search"))
    } else NULL
  }
}


gen_ui_modif_qpv = function(input,session,zonage_qpv_reac){
  message("func : gen_ui_modif_qpv")
  
  if(input$search_qpv | !is.null(input$communes_map_marker_click)){
    showModal(session=session,modalDialog(
      title="Modifier le zonage d'un QPV",
      selectizeInput("edit_one_qpv","Choix du QPV à éditer",choices = zonage_qpv_reac()$cod,multiple=T,
                     options=list(maxItems=1,plugins= list('remove_button')),
                     selected = input$communes_map_marker_click$id),
      uiOutput("edit_qpv_options"),
      easyClose = T,size = "m",footer = modalButton("Fermer")
    ))
  }
  
}


gen_edit_qpv_options = function(input,vals_reac,regions_reac,hist_qpv,zonage_qpv,tableau_reg,TVS){
  message("func : gen_edit_qpv_options")
  
  if(!is.null(input$edit_one_qpv)&length(input$edit_one_qpv)>0){
    info_qpv = hist_qpv[cod %in% input$edit_one_qpv]
    info_tvs = TVS[agr == info_qpv$agr][1]#une ligne par commune
    info_curr = tableau_reg[agr==info_qpv$agr]
    
    tagList(
      tags$p(paste0("Région : ",regions_reac()[reg==info_qpv$reg]$libreg)),
      tags$p(paste0("TVS : ", info_tvs$libagr," (",info_tvs$agr, ")")),
      tags$p(paste0("Nom QPV : ", info_qpv$libqpv)),
      tags$p(paste0("Population : ",info_qpv$pop)),
      tags$p(paste0("Zonage TVS : ",vals_reac()[vals_reac()$agr==info_qpv$agr,]$picked_zonage)),
      tags$p(paste0("Cadre national : ", info_curr$CN)),
      selectizeInput("zonage_one_qpv","Choix du zonage du QPV",
                     selected=zonage_qpv[cod%in%input$edit_one_qpv]$picked_zonage,
                     choices=c("ZIP"="ZIP","ZAC"="ZAC","Zone de vigilance"="ZV","Hors-Vivier"="HV")),
      actionButton("save_zonage_qpv","Enregistrer ce zonage pour ce QPV",icon=icon("save"))
    )
  } else NULL
  
}


save_qpv_func = function(input,session,new_modifs_qpv_reac,modif_zonage_qpv_reac,zonage_qpv){
  message("func : save_qpv_func")
  
  if(!is.null(input$edit_one_qpv)&length(input$edit_one_qpv)>0&!is.null(input$zonage_one_qpv)){
    if(input$zonage_one_qpv!=zonage_qpv[cod%in%input$edit_one_qpv]$picked_zonage){
      save_qpv = paste0("qpv_",input$choix_millesime,".csv")
      local_name = paste0("data/",save_qpv)
      qpv = copy(zonage_qpv)
      qpv[cod%in%input$edit_one_qpv,picked_zonage:=input$zonage_one_qpv]
      fwrite(unique(qpv),file=local_name)
      new_modifs_qpv_reac(new_modifs_qpv_reac()+1)
      modif_zonage_qpv_reac(list(cod=input$edit_one_qpv,picked_zonage=input$zonage_one_qpv))
      removeNotification("save_qpv",session)
      showNotification(sprintf("Le choix du zonage %s pour le QPV %s est enregistré.",input$zonage_one_qpv,input$edit_one_qpv),
                       closeButton = T,id = "save_qpv",session=session)
    }
  }
  
  
}


create_table_zonage_qpv = function(input,modif_zonage_qpv,hist_qpv,zonage_qpv_en_vigueur,dropbox_ps_folder){
  message("func : create_table_zonage_qpv")
  
  req(input$choix_millesime)
  modif_zonage_qpv# => save everytime there is a modif !
  if(!is.null(input$choix_ps)){
    if(input$choix_ps=="mg"){
      
      
      save_qpv = paste0("qpv_",input$choix_millesime,".csv")
      drop_name = paste0(dropbox_ps_folder,save_qpv)
      local_name = paste0("data/",save_qpv)
      if(!drop_exists(drop_name)){
        # INIT from file zonage 2019
        qpv=data.table::copy(hist_qpv)[reg==input$choix_reg,c("cod","zonage_ars")]
        setnames(qpv,"zonage_ars","picked_zonage")
        fwrite(unique(qpv),file=local_name)
        drop_clean_upload(filename = save_qpv,drop_path = dropbox_ps_folder)
        
      } else {
        # FROM SAVED
        if(!save_qpv%in%list.files("data/"))
          drop_download(drop_name,local_path = "data/",overwrite = T,verbose = T)
        qpv <- fread(local_name,colClasses = "character")
      }
      
      # VERIFIER ZONAGE EN VIGUEUR AUTRES REG
      if(nrow(zonage_qpv_en_vigueur)>0){
        qpv = qpv[zonage_qpv_en_vigueur,picked_zonage:=i.en_vigueur_autre_reg,on="cod"]
      }
      qpv
    }
  }
}



warning_qpv_in_tvs = function(input,session,vals_reac,last_zonage_tvs_reac,hist_qpv){
  message("func : warning_qpv_in_tvs")
  
  if(input$sidebarmenu=="zonage" & input$choix_ps == "mg"){
    if(!is.null(last_zonage_tvs_reac())){
      latest = vals_reac()
      old = last_zonage_tvs_reac()
      modified = data.table(merge(latest,old,by="agr",suffixes=c(".new",".old")))[picked_zonage.new!=picked_zonage.old]
      tvs_has_qpv = modified$agr%in%hist_qpv$agr
      if(tvs_has_qpv){
        showNotification(sprintf("Attention, ce TVS (%s) contient des QPV, merci de vérifier la cohérence du zonage de ces QPV.",
                                 paste(modified$agr,collapse=" & ")),
                         closeButton = T,type = "warning",session=session,duration = NULL)
        
        
      }
      
    }
    last_zonage_tvs_reac(vals_reac())
  }
}
