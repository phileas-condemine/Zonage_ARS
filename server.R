function(input, output,session) {
  drop_auth(rdstoken = "droptoken.rds")
  params = fread("params.csv",sep=":")
  source("utils/load_files.R",encoding = "UTF-8")
  
  
  dropbox_folder = reactive({
    req(session$clientData$url_pathname)
    if(session$clientData$url_pathname=="/Zonage_ARS/"){
      "zonage/"
    } else {
      "zonage_dev/"
    }
  })
  
  observeEvent(session$clientData$url_pathname,{
    req(session$clientData$url_pathname)
    if(session$clientData$url_pathname!="/Zonage_ARS/"){
      showNotification(HTML("<p><b>Attention</b>, vous êtes sur une page de test.<br>",
                            "<a href=\"https://drees.shinyapps.io/Zonage_ARS/\">",
                            "Me rediriger vers la bonne adresse</a>.<br></p>"),duration = NULL,type = "warning")
    }
  })
  
  dropbox_ps_folder = reactive({
    req(input$choix_ps)
    paste0(dropbox_folder(),input$choix_ps,"/")
  })
  
  TVS = reactive(get_TVS(dropbox_folder(),params[file=="tvs"]$name))
  
  dep = reactive(unique(TVS()[,c("dep","reg","libdep")]))
  
  mom_markers = reactive(get_QPV(dropbox_folder(),params[file=="qpv"]$name))
  
  hist_qpv = reactive(get_hist_qpv(dropbox_folder(),params[file=="zonage_mg"]$name))
  
  BVCV = reactive(get_BVCV(dropbox_folder(),params[file=="bvcv"]$name))
  
  pop_femmes = reactive({get_pop_femmes(dropbox_folder(),params[file=="pop_femmes"]$name)})
  
  dep_contours = reactive({get_dep_contours(dropbox_folder(),params[file=="contours_dep"]$name)})
  
  reg_cont = reactive({get_reg_contours(dropbox_folder(),params[file=="contours_reg"]$name)})
  
  regions_reac = reactive({get_regions_seuils(dropbox_folder(),params[file=="seuils_arretes"]$name,TVS())})
  
  TA = reactive({get_TA(dropbox_folder(),params[file=="liste_tribunaux"]$name)})
  
  
  
  # REG MAJORITAIRE
  
  file = "agr_reg_majoritaire.RData"
  local_name = paste0("data/",file)
  drop_name = paste0("zonage/",file)
  if(drop_exists(drop_name)){
    print("recup régions majoritaires par AGR")
    drop_download(drop_name,local_path = "data/",overwrite = T
                  # ,verbose = T
    )
  } else {
    print("construction du fichier des régions majoritaires par AGR from scratch")
    source("utils/region_majoritaire.R",local=T,encoding="UTF-8")
  }
  load(local_name)#bvcv_reg_majoritaire & tvs_reg_majoritaire
  setnames(bvcv_reg_majoritaire,"reg_majoritaire","reg")
  setnames(tvs_reg_majoritaire,"reg_majoritaire","reg")
  bvcv_reg_majoritaire
  tvs_reg_majoritaire
  
  
  
  # init_gs=reactiveVal(T)
  # zonage_reactif=reactiveVal("")
  fond_de_carte=reactiveVal(F)
  map_coord=reactiveVal(c(0,45))
  timer=reactiveVal(Sys.time())
  timer_qpv=reactiveVal(Sys.time())
  # no_archive=reactiveVal(T)
  millesimes=reactiveVal(NULL)
  autorefresh <- reactiveTimer(5000)
  new_modifs <- reactiveVal(0)
  new_modifs_qpv <- reactiveVal(0)
  has_logged_in <- reactiveVal(F)
  enable_dl_zonage_en_vigueur <- reactiveVal(F)
  edition_forced <- reactiveVal(c())
  last_arg_clicked <- reactiveVal("")
  modif_zonage_qpv <- reactiveVal(list(cod="",picked_zonage=""))
  last_zonage_tvs <- reactiveVal(NULL)
  # google_files <- reactiveVal(NULL)
  default_vals <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  current_mapped_data <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  
  output$ui_choix_reg = renderUI({
    regions = regions_reac()
    selectizeInput('choix_reg','Sélectionner votre région',width = "100%",
                   choices=setNames(regions$reg,regions$libreg),multiple=T,
                   options = list(placeholder = 'Le nom de votre région',plugins= list('remove_button'),maxItems=1))%>%shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Choisissez la région dont vous souhaitez renseigner le zonage.")
                   )
  })
  
  
  
  output$rappel_ps = renderUI({
    if(!is.null(input$choix_ps)&!is.null(input$choix_millesime)){
      tagList(tags$h2(paste0(
        "Projet de zonage des ",
        names(list_PS[list_PS==input$choix_ps])[1])),
        tags$i(paste0("Intitulé : ",
                      input$choix_millesime
        )))
    } else NULL
  })
  
  
  
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
  
  #### QPV ####
  
  source("utils/qpv.R",local=T,encoding = "UTF-8")
  
  ##### Recap modif #####
  
  info_recap_reac = reactiveVal()
  
  output$recap_dt = renderDataTable({
    infos <- vals_reac()
    req(!is.null(infos))
    if(input$choix_ps=="mg"){
      #### TVS
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      infos <- merge(infos,communes_TVS[,c("agr","libagr","libcom","depcom","my_reg_TVS","population")],by="agr")
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,en_vigueur_autre_reg,CN)]),by="agr")
      setnames(infos,"my_reg_TVS","dansMaRegion")
      infos = data.table(infos)
      
      infos = merge(infos,VZN[,.SD[1],by="tvs"],by.x="agr",by.y="tvs",all.x=T)
      infos = infos[(zonage_ars!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
      
      #### QPV
      
      infos_zonage_qpv = merge(hist_qpv()[,c("cod","libqpv","agr","pop")],zonage_qpv(),by="cod")
      setnames(infos_zonage_qpv,"picked_zonage","picked_zonage_qpv")
      infos_zonage_qpv = merge(infos_zonage_qpv,unique(tableau_reg()[,.(agr,CN)]),by="agr")
      infos_zonage_qpv = merge(infos_zonage_qpv,vals_reac(),by="agr")
      
      infos_zonage_qpv = infos_zonage_qpv[picked_zonage_qpv!=picked_zonage,c("libqpv","cod","picked_zonage_qpv","picked_zonage","pop")]
      names(infos_zonage_qpv) <- c("libagr","agr","picked_zonage","CN","population")
      infos_zonage_qpv$CN = paste0("TVS : ",infos_zonage_qpv$CN)
      if(nrow(infos_zonage_qpv)>0){
        infos = rbind(infos,infos_zonage_qpv)
      }
      
      if (nrow(infos)>0){
        info_recap_reac(copy(infos))
        removeNotification("justification_form_available",session)
        showNotification("Le bouton \"Justification du zonage pris\" vous permet d'ajouter des commentaires à l'attention de la DGOS et de la CNAM pour étayer vos choix de zonage.",
                         duration = NULL,id = "justification_form_available",type = "message")
      }
      
      
    } else if (input$choix_ps %in% c("sf","inf")){
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      infos <- merge(infos,communes_BVCV[,c("agr","libagr","libcom","depcom","my_reg_BVCV","population")],by="agr")
      infos <- merge(infos, unique(tableau_reg()[,.(agr,is_majoritaire,ZE_UD,ZE_OD,en_vigueur_autre_reg,CN)]),by="agr")
      # infos$echangeable = infos$ZE_UD + infos$ZE_OD
      # infos$enVigueurAutreReg = !is.na(infos$en_vigueur_autre_reg)
      # setnames(infos,c("my_reg_BVCV","is_majoritaire"),c("dansMaRegion","estMajoritaire"))
      infos = data.table(infos)
      infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
      
    }
    # infos = data.table(infos)
    # infos = infos[(CN!=picked_zonage)&(is_majoritaire),.(population=sum(population)),by=c("libagr","agr","picked_zonage","CN")]
    setnames(infos,c("libagr","agr","picked_zonage","CN","population"),c("Libelle","Code","Zonage","Cadre National","Population"))
    if(nrow(infos)>0){
      datatable(infos,
                rownames=F,
                options=list(
                  dom = "t",
                  pageLength = -1
                ))
    } else NULL
  })
  
  output$ui_open_form_justification = renderUI({
    req(info_recap_reac())
    if(nrow(info_recap_reac())>0){
      actionButton("open_form_justification","Justification du zonage pris",icon=icon("edit"))
    }
    
  })
  
  observeEvent(input$open_form_justification,{
    req(input$open_form_justification)
    if(input$open_form_justification){
      if (nrow(info_recap_reac())>0){
        infos = info_recap_reac()
        save_justification = paste0("justification_",input$choix_millesime)
        drop_name = paste0(dropbox_ps_folder(),save_justification)
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
        showModal(modalDialog(title="Explications du choix de zonage",
                              "Merci de préciser les indicateurs complémentaires utilisés pour déterminer le zonage.",
                              # "N'hésitez pas à justifier le zonage pris pour chaque TVS et chaque QPPV",
                              # "Rappel des zonages pris qui diffèrent du cadre national: ",
                              # paste(paste0(infos$libagr," (",infos$agr,") zonage choisi: ",infos$picked_zonage," (cadre national: ",infos$CN,")"),collapse=" - "),
                              textAreaInput("justification_zonage",NULL,value=justification$txt[1],placeholder = "Insérer votre texte ici...",
                                            width = "800px",height = "400px",resize = "both"),easyClose = F,size="l",
                              footer=tagList(modalButton("Annuler"),actionButton("validation_justification","Valider",icon=icon("check")))))
        
      }
    }
  })
  
  observeEvent(input$validation_justification,{
    req(input$justification_zonage)
    if(input$justification_zonage!=""){
      save_justification = paste0("justification_",input$choix_millesime)
      drop_name = paste0(dropbox_ps_folder(),save_justification)
      local_name = paste0("data/",save_justification)
      justification=data.table(time=as.character(Sys.time()),txt=gsub('"','',input$justification_zonage))
      fwrite(justification,file=local_name)
      drop_clean_upload(filename = save_justification,drop_path = dropbox_ps_folder())
    }
    removeModal()
  })
  
  
  
  
  vals_reac=reactive({
    if(has_logged_in()){
      print("get vals_reac")
      my_agr=tableau_reg()[["agr"]]
      my_agr=as.character(my_agr)
      vals <- sapply(my_agr,function(i) input[[i]])
      
      
      vals=vals[sapply(vals,length)>0]
      if(length(vals)>0){
        vals=stack(vals)
        names(vals) <- c("picked_zonage","agr")
        vals <- vals %>% mutate_if(is.factor,as.character)
        print("get vals_reac OK")
        vals
      } else NULL
    } else NULL
  })
  
  source("utils/navigation.R",local=T,encoding = "UTF-8")
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
  
  output$ui_doc_dl = renderUI({
    if(!is.null(input$choix_ps)&!is.null(input$choix_reg)){
      if(input$choix_ps=="mg"){
        
        tagList(
          downloadButton("dl_faq_mg","FAQ", style = "width:230px;color:#000"),br(),
          downloadButton("dl_ref_zonage_med","Fichier réf. zonage médecin", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_tvs_com","Corres. TVS - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_tvs","Population jauges", style = "width:230px;color:#000"),br(),
          downloadButton("dl_reg_maj_tvs","Rég. maj TVS", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_mg","Zonages MG", style = "width:230px;color:#000"),br()
          ,tags$div(id="loading")
        )
      } else if(input$choix_ps=="sf"){
        
        tagList(
          downloadButton("dl_faq_hors_mg","FAQ", style = "width:230px;color:#000"),br(),
          downloadButton("dl_ref_zonage_sf","Fichier réf. zonage SF", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_bvcv_com","Corres. BVCV - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_bvcv_femmes","Population jauges (femmes)", style = "width:230px;color:#000"),br(),
          downloadButton("dl_reg_maj_bvcv","Rég. maj BVCV", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_sf","Zonages SF", style = "width:230px;color:#000"),br()
          ,tags$div(id="loading")
          
        )
      } else  if(input$choix_ps=="inf"){
        tagList(
          downloadButton("dl_faq_hors_mg","FAQ", style = "width:230px;color:#000"),br(),
          downloadButton("dl_ref_zonage_ide","Fichier réf. zonage IDE", style = "width:230px;color:#000"),br(),
          downloadButton("dl_corres_bvcv_com","Corres. BVCV - Communes", style = "width:230px;color:#000"),br(),
          downloadButton("dl_pop_bvcv_all","Population jauges", style = "width:230px;color:#000"),br(),
          downloadButton("dl_reg_maj_bvcv","Rég. maj BVCV", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_inf","Zonages IDE", style = "width:230px;color:#000"),br()
          ,tags$div(id="loading")
          
          
        )
      } else {
        tagList(
          downloadButton("dl_reg_maj_tvs","Rég. maj TVS", style = "width:230px;color:#000"),br(),
          downloadButton("dl_reg_maj_bvcv","Rég. maj BVCV", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_mg","Zonages MG", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_sf","Zonages SF", style = "width:230px;color:#000"),br(),
          downloadButton("dl_zonage_en_vigueur_inf","Zonages IDE", style = "width:230px;color:#000"),br()
          ,tags$div(id="loading")
        )
      }
      
    } else {
      tagList(
        downloadButton("dl_reg_maj_tvs","Rég. maj TVS", style = "width:230px;color:#000"),br(),
        downloadButton("dl_reg_maj_bvcv","Rég. maj BVCV", style = "width:230px;color:#000"),br(),
        downloadButton("dl_zonage_en_vigueur_mg","Zonages MG", style = "width:230px;color:#000"),br(),
        downloadButton("dl_zonage_en_vigueur_sf","Zonages SF", style = "width:230px;color:#000"),br(),
        downloadButton("dl_zonage_en_vigueur_inf","Zonages IDE", style = "width:230px;color:#000"),br()
        ,tags$div(id="loading")
      )
    }
    
    
  })
  
  
  
  
  output$dl_ref_zonage_med <- downloadHandler(
    filename = 'ref_zonage_medecin.xlsx',
    content = function(file) {
      showNotification("Ce fichier contient également la population des QPV")
      slack_log("ref_zonage_medecin.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      
      file.copy(paste0("data/",params[file=="zonage_mg"]$name), file, overwrite = T)
    }
  )
  
  
  output$dl_corres_tvs_com <- downloadHandler(
    filename = 'corrs_tvs_com.xlsx',
    content = function(file) {
      slack_log("corrs_tvs_com.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      corres = haven::read_sas(paste0("data/",params[file=="tvs"]$name))
      openxlsx::write.xlsx(corres,file)
    }
  )
  output$dl_corres_bvcv_com <- downloadHandler(
    filename = 'corres_bvcv_com.xlsx',
    content = function(file) {
      slack_log("corres_bvcv_com.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      corres = haven::read_sas(paste0("data/",params[file=="bvcv"]$name))
      openxlsx::write.xlsx(corres,file)
    }
  )
  output$dl_pop_tvs <- downloadHandler(
    filename = 'pop_tvs.xlsx',
    content = function(file) {
      slack_log("pop_tvs.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      pop_tvs = data.table(communes_TVS)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_tvs) <- c("Région","Département","TVS","Nom TVS","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_tvs,file)
    }
  )
  
  output$dl_ref_zonage_sf <- downloadHandler(
    filename = 'ref_zonage_sf.xlsx',
    content = function(file) {
      slack_log("ref_zonage_sf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      filename = params[file=="zonage_sf"]$name
      if(!filename%in%list.files("data/")){
        drop_download(paste0(dropbox_folder(),filename),local_path = "data/",overwrite = T)
      }
      tmp = haven::read_sas(paste0("data/",filename))
      openxlsx::write.xlsx(tmp,file)
    }
  )
  
  output$dl_ref_zonage_ide <- downloadHandler(
    filename = 'ref_zonage_ide.xlsx',
    content = function(file) {
      slack_log("ref_zonage_ide.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      filename = params[file=="zonage_inf"]$name
      if(!filename%in%list.files("data/")){
        drop_download(paste0(dropbox_folder(),filename),local_path = "data/",overwrite = T)
      }
      tmp = haven::read_sas(paste0("data/",filename))
      openxlsx::write.xlsx(tmp,file)
    }
  )
  
  
  
  
  output$dl_pop_bvcv_femmes <- downloadHandler(
    filename = 'pop_bvcv_femmes.xlsx',
    content = function(file) {
      slack_log("pop_bvcv_femmes.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(pop_femmes(),file)
    }
  )
  
  output$dl_pop_bvcv_all <- downloadHandler(
    filename = 'pop_bvcv.xlsx',
    content = function(file) {
      slack_log("pop_bvcv.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      pop_bvcv = data.table(communes_BVCV)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_bvcv) <- c("Région","Département","BVCV","Nom BVCV","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_bvcv,file)
      
    }
  )
  
  
  
  output$dl_reg_maj_tvs = downloadHandler(
    filename = 'region_majoritaire_TVS.xlsx',
    content = function(file) {
      slack_log("region_majoritaire_TVS.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(list("region_majoritaire_par_TVS" = tvs_reg_majoritaire,"codes_regions"=regions_reac()[,c("reg","libreg")]),file)
    }
  )
  
  output$dl_reg_maj_bvcv = downloadHandler(
    filename = 'region_majoritaire_BVCV.xlsx',
    content = function(file) {
      slack_log("region_majoritaire_BVCV.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(list("region_majoritaire_par_BVCV" = bvcv_reg_majoritaire,"codes_regions"=regions_reac()[,c("reg","libreg")]),file)
    }
  )
  
  output$dl_zonage_en_vigueur_mg <- downloadHandler(
    filename = 'zonages_en_vigueur_mg.xlsx',
    content = function(file) {
      if(enable_dl_zonage_en_vigueur()){
        source("utils/get_zonage_en_vigueur.R",local=T,encoding = "UTF-8")
        en_vigueur_agr = dl_zonage_en_vigueur_agr("mg",dropbox_ps_folder(),"")
        source("utils/get_qpv_zonage_en_vigueur.R",local=T,encoding = "UTF-8")
        en_vigueur_qpv = dl_zonage_en_vigueur_qpv("mg",dropbox_ps_folder(),"")
        if(nrow(en_vigueur_agr)>0){
          slack_log("zonages_en_vigueur_mg.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$reg)),type="message",duration=10)
          openxlsx::write.xlsx(list("AGR"=en_vigueur_agr,"QPV"=en_vigueur_qpv),file = file)
          
        } else {
          showNotification("Aucune ARS n'a validé son zonage pour les médecins généralistes sur l'application pour l'instant",type = "message",duration = 10)
        }
      } else {
        showModal(modalDialog(title="Identification requise",easyClose = F,
                              footer=tagList(actionButton("send_pwd2","Soumettre"),modalButton("Annuler")),
                              passwordInput("my_auth2",label = "",placeholder = "Clef d'identification")
        ))
        
      }
      
    }
  )
  
  
  output$dl_zonage_en_vigueur_sf <- downloadHandler(
    filename = 'zonages_en_vigueur_sf.xlsx',
    content = function(file) {
      if(enable_dl_zonage_en_vigueur()){
        source("utils/get_zonage_en_vigueur.R",local=T,encoding = "UTF-8")
        en_vigueur_agr = dl_zonage_en_vigueur_agr("sf",dropbox_ps_folder(),"")
        if(nrow(en_vigueur_agr)>0){
          slack_log("zonages_en_vigueur_sf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$reg)),type="message",duration=10)
          openxlsx::write.xlsx(list("AGR"=en_vigueur_agr),file = file)
          
        } else {
          showNotification("Aucune ARS n'a validé son zonage pour les sages-femmes sur l'application pour l'instant",type = "message",duration = 10)
        }
      } else {
        showModal(modalDialog(title="Identification requise",easyClose = F,
                              footer=tagList(actionButton("send_pwd2","Soumettre"),modalButton("Annuler")),
                              passwordInput("my_auth2",label = "",placeholder = "Clef d'identification")))
        
      }
      
    }
  )
  
  output$dl_zonage_en_vigueur_inf <- downloadHandler(
    filename = 'zonages_en_vigueur_inf.xlsx',
    content = function(file) {
      if(enable_dl_zonage_en_vigueur()){
        source("utils/get_zonage_en_vigueur.R",local=T,encoding = "UTF-8")
        en_vigueur_agr = dl_zonage_en_vigueur_agr("inf",dropbox_ps_folder(),"")
        if(nrow(en_vigueur_agr)>0){
          slack_log("zonages_en_vigueur_inf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$reg)),type="message",duration=10)
          openxlsx::write.xlsx(list("AGR"=en_vigueur_agr),file = file)
          
        } else {
          showNotification("Aucune ARS n'a validé son zonage pour les infirmiers sur l'application pour l'instant",type = "message",duration = 10)
        }
      } else {
        showModal(modalDialog(title="Identification requise",easyClose = F,
                              footer=tagList(actionButton("send_pwd2","Soumettre"),modalButton("Annuler")),
                              passwordInput("my_auth2",label = "",placeholder = "Clef d'identification")))
        
      }
      
    }
  )
  
  
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
  
  
  output$dl_faq_hors_mg <- downloadHandler(
    filename = 'FAQ_hors_mg.pdf',
    content = function(file) {
      file.copy("www/FAQ_hors_mg.pdf", file, overwrite = T)
      
    }
  )
  
  output$dl_faq_mg <- downloadHandler(
    filename = 'FAQ_mg.pdf',
    content = function(file) {
      file.copy("www/FAQ_mg.pdf", file, overwrite = T)
      
    }
  )
  
}