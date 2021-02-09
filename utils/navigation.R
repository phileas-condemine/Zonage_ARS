
###### LOGOUT  #######
observeEvent(input$logout,{
  req(input$logout)
  session$reload()
})
observeEvent(input$sidebarmenu,{
  if(has_logged_in()&input$sidebarmenu%in%c("accueil","my_params")){
    session$reload()
  }
})


###### LOGIN TO ZONAGE #####
observeEvent(input$go_zonage,{
  print("current mil selected")
  print(input$choix_millesime)
  
  if(input$choix_reg == 6 & input$choix_ps != "mg"){
    showNotification("Pour l'instant seule la profession de médecin généraliste est traitée dans cette application pour Mayotte")
  } else {
    
    if(is.null(input$choix_millesime)|input$choix_millesime==""){
      new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_cadre_national')
      updateSelectizeInput(session,'choix_millesime',
                           choices=c(millesimes(),setNames(new_mil,"cadre_national")),
                           selected=new_mil)
    }
    
    updateTabsetPanel(session,"sidebarmenu","zonage")
    
    showModal(modalDialog(title="Identification requise",easyClose = F,
                          footer=tagList(actionButton("send_pwd","Soumettre"),modalButton("Annuler")),
                          passwordInput("my_auth",label = "",placeholder = "Clef d'identification")))
  }
})

observeEvent(c(input$choix_reg,input$choix_ps,input$choix_millesime),{
  print("valeur par défaut import data model")
  print(input$import_data_model)
  output$auth=renderText({
    "KO"
  })
})





observeEvent(input$send_pwd,{
  req(input$my_auth)
  # path2auth = paste0("zonage/",params[name=="auth"]$file)
  auth = get_auth()
  auth = auth[key==input$my_auth & reg == input$choix_reg]
  if(nrow(auth)>0){
    # print("OK")
    output$auth=renderText({
      "OK"
    })
    
    if(any(grepl("phileas",auth$name))){
      log_is_admin(T)
      key = "clef universelle"
    } else {
      log_is_admin(F)
      key = paste0("région ",auth$name)
    }
    
    
    
    
    
    reg = ifelse(!is.null(input$choix_reg),input$choix_reg,"XX")
    ps = ifelse(!is.null(input$choix_ps),input$choix_ps,"XX")
    mil = ifelse(!is.null(input$choix_millesime),input$choix_millesime,"XX")
    
    if(session$clientData$url_pathname=="/Zonage_ARS/" & !log_is_admin()){
      message=sprintf("App:ZonageARS\nEvent: Connexion de la région %s pour la profession %s avec le projet %s",reg,ps,mil)
      try({
        slackr_setup(config_file = "www/slackr_config_log.txt",echo = F)
        slackr_bot(message)
      })
      send_mail_user_login(my_reg = reg,my_ps = ps)
      # email <- gm_mime() %>%
      #   gm_to(c("blandine.legendre@sante.gouv.fr","phileas.condemine@sante.gouv.fr")) %>%
      #   gm_subject("Envoi de mail via R") %>%
      #   gm_html_body(body = HTML("<p><b>Bonjour</b>,<br>",
      #                            sprintf("Une connexion a été réalisée à %s avec la %s sur l'app %s%s<br>",as.character(Sys.time()),key,session$clientData$url_hostname,session$clientData$url_pathname),
      #                            sprintf("L'utilisateur s'est connecté pour la région %s avec la profession %s.<br>",regions_reac()[reg==input$choix_reg]$libreg,names(list_PS)[list_PS==input$choix_ps]),
      #                            ifelse(is.null(IP()),"",sprintf("D'après les infos collectées, l'IP est dans la ville de %s, en %s (%s), organisation : %s.<br>",IP()$city,IP()$region,IP()$country,IP()$org)),
      #                            "A bientôt<br>",
      #                            "Philéas</p>"))
      # gm_send_message(email)
      
    }
    
    has_logged_in(T)
    enable_dl_zonage_en_vigueur(T)
    outputOptions(output, "auth", suspendWhenHidden=FALSE)
    removeModal()
    removeNotification("error_w_key",session)
    
  } else {
    
    removeNotification("error_w_key",session)
    showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL)
  }
  
})


observeEvent(input$send_pwd2,{
  req(input$my_auth2)
  # path2auth = paste0("zonage/",params[name=="auth"]$file)
  auth = get_auth()
  auth = auth[key==input$my_auth2]
  if(nrow(auth)>0){
    # print("OK")
    # output$auth2=renderText({
    #   "OK"
    # })
    
    if(any(grepl("phileas",auth$name))){
      log_is_admin(T)
      key = "clef universelle"
    } else {
      log_is_admin(F)
      key = paste0("région ",auth$name)
    }
    
    
    enable_dl_zonage_en_vigueur(T)
    # outputOptions(output, "auth2", suspendWhenHidden=FALSE)
    removeModal()
    removeNotification("error_w_key",session)
    showNotification("Identification validée, merci de renouveler la demande de téléchargement.",type = "message")
    
  } else {
    
    removeNotification("error_w_key",session)
    showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL)
  }
  
})

observeEvent(input$go_params,{
  updateTabsetPanel(session,"sidebarmenu","my_params")
})

observeEvent(input$choix_reg_map_shape_click,{
  print("got clicked in map shape")
  print(input$choix_reg_map_shape_click)
  click=input$choix_reg_map_shape_click
  updateSelectizeInput(session,"choix_reg",selected=click$id)
  
})

output$choix_reg_map=renderLeaflet({
  print("create map")
  bbox_reg <- st_bbox(reg_cont()[as.numeric(reg_cont$reg)>10,]) %>% 
    as.vector()
  leaflet(data = reg_cont(),options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")%>%
    addPolygons(label = ~ paste(libreg,"code :",reg),
                layerId = ~ as.numeric(reg),color=NULL)%>%
    fitBounds(bbox_reg[1]-1,bbox_reg[2],bbox_reg[3],bbox_reg[4])
})

dropbox_files = reactiveVal()

observeEvent(c(input$choix_reg,input$choix_ps),{
  req(input$choix_reg)
  req(input$choix_ps)
  
  dropbox_files(get_dropbox_files(input,dropbox_ps_folder = dropbox_ps_folder()))
  
})

# dropbox_files = reactive({
  # regex = paste0(input$choix_ps,'_',input$choix_reg,'_')
  # reg_files = drop_dir(dropbox_ps_folder())
  # reg_files = data.table(reg_files)
  # if(nrow(reg_files)>0){#premier filtre
  #   reg_files = reg_files[grepl(regex,name)]
  #   reg_files = reg_files[!grepl("qpv_",name)]
  # }
  # if(input$choix_ps == "mg"){
  #   regex = paste0("qpv_",input$choix_ps,'_',input$choix_reg,'_')
  #   qpv_files = drop_dir(dropbox_ps_folder())
  #   qpv_files = data.table(qpv_files)
  #   if(nrow(qpv_files)>0){
  #     qpv_files = qpv_files[grepl(regex,name)]
  #   }
  #   
  #   reg_files <- rbind(reg_files,qpv_files)
  # }
  # 
  # if(nrow(reg_files)>0){#s'il en reste
  #   print("found google files !")
  #   reg_files
  # } else NULL
# })



output$ui_params = renderUI({
  req(input$choix_ps)
  tagList(
    sliderInput("table_width","Ajuster la table",min=0,max=12,value=8),
    shinyWidgets::switchInput(inputId = "remove_alerte_jauge",
                              label = "Alertes Jauges",
                              value = F,
                              onLabel = "Désactivée",offLabel = "Activées",
                              labelWidth = "200",handleWidth = "100",
                              size = "normal",inline = T
    )
  )
  
  
})

outputOptions(output, "ui_params", suspendWhenHidden=FALSE)

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

observeEvent(c(input$feedback_send),{
  req(input$feedback_send)
  send_mail_and_slack_user_feedback(input,session)

})
