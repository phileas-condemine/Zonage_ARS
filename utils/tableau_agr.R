

tableau_reg = reactive({
  print("tableau")
  if(has_logged_in()){
    req(input$choix_reg)
    req(input$choix_ps)
    # print("Variables à afficher") ; print(input$vars_to_show)
    # print("Largeur boxes") ; print(input$table_width)
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    if(shiny_running())
      progress$set(message = "Chargement", value = 0)
    
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg==my_reg]$libreg
    my_deps=dep()[reg==my_reg]$dep
    
    if(input$choix_ps=="mg"){
      AGR <- TVS()
      nom_fichier_dropbox <- "_preprocessed_TVS.RData"
    } else{
      AGR <- BVCV()
      nom_fichier_dropbox <- "_preprocessed_BVCV.RData"
    }
    
    
    if(shiny_running())
      progress$inc(1/9, detail = "Vérification de l'historique")
    source("utils/handle_geo_data.R",local=T,encoding = "UTF-8")
    progress$inc(2/9, detail = "Récupération des données géographiques")
    get_geo_data(my_reg,dropbox_folder(),environment())
    
    
    
    maille_geo = ifelse(input$choix_ps=="mg","TVS","BVCV")
    print("recup historique fichier preprocessed reg")
    err = try({
      historique=rdrop2::drop_history(paste0(dropbox_folder(),my_reg,"_preprocessed_",maille_geo,".RData"),limit = 1)
      print(historique$server_modified)
      print(historique$client_modified)
    })
    print(err)

    print("req choix_mil")
    req(input$choix_millesime)
    my_mil = input$choix_millesime
    print("my_mil1") ; print(my_mil)
    my_mil = my_mil
    progress$inc(2/9, detail = "Chargement des fichiers historiques")
    source("utils/get_zonage_en_vigueur.R",local=T,encoding = "UTF-8")
    zonages_en_vigueur = dl_zonage_en_vigueur_agr(input$choix_ps,dropbox_ps_folder(),input$choix_reg)

    
    if(input$choix_ps=='mg'){
      source(paste0("utils/prep_zonage_mg.R"),local=T,encoding = "UTF-8")
    }else{
      source(paste0("utils/prep_zonage_hors_mg.R"),local=T,encoding = "UTF-8")
    }
    
    
    if (shiny_running()){
      last_modif = ifelse("historique"%in%ls(),
                     as.character(as.Date(historique$client_modified)),
                     "")
      output$date_contours_update=renderText({
        paste("Date de dernière mise à jour des contours géographiques :",last_modif)
      })    
      
      
    }
    
    progress$inc(2/9, detail = "Mise en forme des données de zonage")
    prep_zonage(env=environment(),
                choix_mil = my_mil,
                # no_archive=no_archive(),
                my_dropbox_files=dropbox_files())
    
    progress$inc(2/9, detail = "OK !")
    
    if(input$choix_ps=="mg"){
      fond_de_carte(carte_TVS)
    } else{
      fond_de_carte(carte_BVCV)
    }
    
    # On remplace les valeurs en s'appuyant sur les zonages en vigueur des autres régions.
    if(nrow(zonages_en_vigueur)>0){
      vals = data.table(vals)[zonages_en_vigueur[majoritaire==1],picked_zonage:=i.en_vigueur_autre_reg,on="agr"]
    }
    vals = data.frame(vals)
    default_vals(vals)
    current_mapped_data(vals)
    
    if(input$choix_ps=="mg"){
      tvs = merge(tvs,zonages_en_vigueur[,.(agr,en_vigueur_autre_reg)],by="agr",all.x=T)
      tvs[,degre_liberte := (CN=="02_Vivier")*is_majoritaire]
      
      tvs = rbind(tvs[degre_liberte==1],tvs[degre_liberte==0],tvs[is.na(degre_liberte)])
      
      tvs
    } else {
      browser()
      bvcv = merge(bvcv,zonages_en_vigueur[,.(agr,en_vigueur_autre_reg)],by="agr",all.x=T)
      bvcv[,degre_liberte := (ZE_UD+ZE_OD)*is_majoritaire]
      
      bvcv = rbind(bvcv[degre_liberte==1],bvcv[degre_liberte==0],bvcv[is.na(degre_liberte)])
      bvcv
    }
  }
})

output$zonage_dt=DT::renderDataTable(server=F,{
  print("DT")
  my_data=data.table::copy(tableau_reg())
  nb_rows = nrow(my_data)
  print(paste0("Conservation des lignes: ",round(100*nrow(my_data)/nb_rows),"%"))
  my_data[,degre_liberte:=ifelse(degre_liberte==1,"modifiable","hors-champs")]
  print("display DT")
  all_vars_to_show = unname(vars_to_choose_from[[input$choix_ps]])
  
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
            callback = JS(readLines("www/dt_callback.js",encoding='UTF-8')),
            options = list(searchHighlight = TRUE, fixedHeader = TRUE,
                           searchHighlight = TRUE,
                           columnDefs = list(
                             list(targets = which(!all_vars_to_show%in%vars_to_show_list[[input$choix_ps]])-1,visible = F)
                             ,list(targets = which(all_vars_to_show == "communes")-1,render = JS(
                               "function(data, type, row, meta) {",
                               sprintf("return type === 'display' && data.length > %s ?",40),
                               sprintf("'<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;",40),
                               "}"
                             ))
                           ),
                           search = list(regex = TRUE, caseInsensitive = TRUE),
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
    c('rgba(31,120,180,0.2)', #1F78B4
      'rgba(178,223,138,0.2)', #B2DF8a
      'rgba(51,160,44,0.2)', #33A02C
      'rgba(251,154,153,0.2)', #FB9A99
      'rgba(227,26,28,0.2)'))} #E31A1C
  )
})

observeEvent(input$last_forced_edition,{
  req(input$last_forced_edition)
  if(!last_arg_clicked()%in%edition_forced()){
    edition_forced(c(edition_forced(),last_arg_clicked()))
  }
})

observeEvent(input$zonage_dt_cell_clicked,{
  req(input$zonage_dt_cell_clicked)
  print("got clicked in DT cell")
  if(length(input$zonage_dt_cell_clicked)>0){
    my_row=tableau_reg()[input$zonage_dt_cell_clicked$row]
    if(!my_row$agr%in%edition_forced()){
      last_arg_clicked(my_row$agr)
      # print(input$zonage_dt_cell_clicked)
      # print(my_row$is_majoritaire)
      new_modifs(new_modifs()+1)
      if(!my_row$is_majoritaire){
        shinyalert("Attention!", paste("Vous avez sélectionné un",ifelse(input$choix_ps=="mg","Territoire de Vie-Santé","Bassin de Vie - Canton-Ville"),"minoritaire en termes de population dans votre région."),
                   cancelButtonText = "Annuler",
                   confirmButtonText = "Forcer l'édition",
                   type = "error",showCancelButton = T,
                   callbackJS = sprintf("
                   function(x) {
                     if (x == true) {
                      $('#%s .zonage_radio_button').prop('disabled',false);
                      Shiny.setInputValue('last_forced_edition','newval', {priority: 'event'});
                     }
                   }",my_row$agr))
      }
      if (!is.na(my_row$CN)&(my_row$CN=="01_Sélection nationale"|(input$choix_ps!="mg"&my_row$degre_liberte==0))){
        shinyalert("Attention!", ifelse(input$choix_ps=="mg",
                                        ifelse(my_row$CN=="01_Sélection nationale",
                                               "Ce Territoire de Vie-Santé fait partie de la sélection nationale.",
                                               "Ce Territoire de Vie-Santé est hors-vivier."
                                        ),"Ce Bassin de Vie - Canton-Ville n'est pas en zone d'échange."),
                   cancelButtonText = "Annuler",
                   confirmButtonText = "Forcer l'édition",
                   type = "error",showCancelButton = T,
                   callbackJS = sprintf(
                     "function(x) { 
                                   if (x == true) {
                                    $('#%s .zonage_radio_button').prop('disabled',false);
                                    Shiny.setInputValue('last_forced_edition','newval', {priority: 'event'});
                                   }
                                 }"
                     ,my_row$agr))
      }
    }
  }
  
})
