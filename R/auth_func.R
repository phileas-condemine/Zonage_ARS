

on_login_actions = function(input,output,session,log_is_admin_reac,has_logged_in_reac,enable_dl_zonage_en_vigueur_reac,auth){
  message("func : on_login_actions")
  
  req(input$my_auth)
  auth = auth[key==input$my_auth & reg == input$choix_reg]
  if(nrow(auth)>0){
    # print("OK")
    output$auth=renderText({
      "OK"
    })
    
    if(any(grepl("phileas",auth$name))){
      log_is_admin_reac(T)
      key = "clef universelle"
    } else {
      log_is_admin_reac(F)
      key = paste0("région ",auth$name)
    }
    
    reg = ifelse(!is.null(input$choix_reg),input$choix_reg,"XX")
    ps = ifelse(!is.null(input$choix_ps),input$choix_ps,"XX")
    mil = ifelse(!is.null(input$choix_millesime),input$choix_millesime,"XX")
    
    if(session$clientData$url_pathname=="/Zonage_ARS/" & !log_is_admin_reac()){
      message=sprintf("App:ZonageARS\nEvent: Connexion de la région %s pour la profession %s avec le projet %s",reg,ps,mil)
      try({
        slackr_setup(config_file = "www/slackr_config_log.txt",echo = F)
        slackr_bot(message)
      })
      send_mail_user_login(my_reg = reg,my_ps = ps)
    }
    
    has_logged_in_reac(T)
    enable_dl_zonage_en_vigueur_reac(T)
    outputOptions(output, "auth", suspendWhenHidden=FALSE)
    removeModal(session=session)
    removeNotification("error_w_key",session)
    
  } else {
    
    removeNotification("error_w_key",session)
    showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL,session=session)
  }
}



on_alt_login_actions = function(
  input,
  session,
  log_is_admin_reac,
  enable_dl_zonage_en_vigueur_reac,
  auth
) {
  message("func : on_alt_login_actions")
  
  req(input$my_auth2)
  auth = auth[key==input$my_auth2]
  if(nrow(auth)>0){
    
    if(any(grepl("phileas",auth$name))){
      log_is_admin_reac(T)
      key = "clef universelle"
    } else {
      log_is_admin_reac(F)
      key = paste0("région ",auth$name)
    }
    
    
    enable_dl_zonage_en_vigueur_reac(T)
    removeModal(session=session)
    removeNotification("error_w_key",session)
    showNotification("Identification validée, merci de renouveler la demande de téléchargement.",type = "message",session=session)
    
  } else {
    
    removeNotification("error_w_key",session)
    showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL,session=session)
  }
  
}