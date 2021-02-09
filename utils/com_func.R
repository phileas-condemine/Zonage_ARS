slack_log = function(filename,my_reg,my_ps,my_mil,session){
  filename = filename
  reg = ifelse(is.null(my_reg),"XX",my_reg)
  ps = ifelse(is.null(my_ps),"XX",my_ps)
  mil = ifelse(is.null(my_mil),"XX",my_mil)
  if(session$clientData$url_pathname=="/Zonage_ARS/"){
    message=sprintf("App:ZonageARS\nEvent: Téléchargement du fichier %s par la région %s pour la profession %s avec le projet %s",filename,reg,ps,mil)
    try({
      slackr_setup(config_file = "www/slackr_config_log.txt",echo = F)
      slackr_bot(message)
    })
  }
}


drop_clean_upload = function(filename, local_path = "data/",drop_path = "zonage/",message=NULL){
  local_name = paste0(local_path,filename)
  drop_name = paste0(drop_path,filename)
  # if(rdrop2::drop_exists(drop_name)){
  #   if(!is.null(message)){
  #     print(message)
  #   }
  #   rdrop2::drop_delete(path = drop_name)
  # }
  rdrop2::drop_upload(file = local_name,path = drop_path,autorename = F,mode="overwrite")
}

send_mail_user_login = function(my_reg=input$choix_reg,my_ps=input$choix_ps,session=session,IP=IP(),info_region = regions_reac()){
  
  email <- gm_mime() %>%
    gm_to(c("blandine.legendre@sante.gouv.fr","phileas.condemine@sante.gouv.fr")) %>%
    gm_subject("Envoi de mail via R") %>%
    gm_html_body(body = HTML("<p><b>Bonjour</b>,<br>",
                             sprintf("Une connexion a été réalisée à %s avec la %s sur l'app %s%s<br>",as.character(Sys.time()),key,session$clientData$url_hostname,session$clientData$url_pathname),
                             sprintf("L'utilisateur s'est connecté pour la région %s avec la profession %s.<br>",info_region[reg==my_reg]$libreg,names(list_PS)[list_PS==my_ps]),
                             ifelse(is.null(IP),"",sprintf("D'après les infos collectées, l'IP est dans la ville de %s, en %s (%s), organisation : %s.<br>",IP$city,IP$region,IP$country,IP$org)),
                             "A bientôt<br>",
                             "Philéas</p>"))
  gm_send_message(email)
  
}


send_mail_user_validate_zonage = function(my_reg=input$choix_reg,my_ps=input$choix_ps,info_region = regions_reac()){
  
  
  email <- gm_mime() %>%
    gm_to(c(correspondants_CNAM,correspondants_DGOS)) %>%
    gm_cc(correspondants_dev_drees)%>%
    gm_subject("[Message automatique] Validation d'un zonage par une ARS") %>%
    gm_html_body(body = HTML("<p>Bonjour à tous,<br>",
                             sprintf("L'ARS de la région %s vient de valider son zonage sur l'<a href=\"https://drees.shinyapps.io/Zonage_ARS/\">application DREES</a> avec la profession %s.<br>",info_region[reg==my_reg]$libreg,names(list_PS)[list_PS==my_ps]),
                             "Bien cordialement,<br>",
                             "Blandine et Philéas<br>",
                             "PS : Merci de ne pas répondre, il s'agit d'un mail automatique.</p>"))
  gm_send_message(email)
  
}


send_mail_and_slack_user_feedback = function(input,session){
  
  text_to_send=input$feedback_content
  print("comment to send to slack")
  print(text_to_send)
  if(text_to_send!=""){
    adresse_mail=ifelse(is.null(input$adresse_mail),"",input$adresse_mail)
    name_sender=ifelse(is.null(input$name_sender),"",input$name_sender)
    
    message=paste0("App:ZonageARS\n",
                   "Mail: ",adresse_mail,
                   "\nNom: ",name_sender,
                   "\nContenu: ",text_to_send)
    message=gsub("\"","*",message)
    
    
    # if(session$clientData$url_pathname=="/Zonage_ARS/"){
      updateTextAreaInput(session,"feedback_content",value = "")
      
      try({
        slackr_setup(config_file = "www/slackr_config.txt",echo = F)
        slackr_bot(message)
        
      })
      
      try({
        email <- gm_mime() %>%
          gm_to(c("blandine.legendre@sante.gouv.fr","phileas.condemine@sante.gouv.fr")) %>%
          gm_subject("Commentaire d'un utilisateur") %>%
          gm_text_body(paste0(text_to_send,"\n",name_sender,"\n",adresse_mail))
        gm_send_message(email)
      })
      
      
      showModal(modalDialog(title="Merci pour votre commentaire !",size="s",
                            footer=NULL,easyClose = T,
                            "Cliquer dans la zone grisée pour revenir à la liste des indicateurs de santé."),
                session = session)
      showNotification(ui="Merci pour votre commentaire !",duration = 5,session = session)
      shinyjs::runjs("$('.sidebar-menu > li:nth-child(5) > a').trigger('click');")
    # } else {
    #   showNotification(ui="Les commentaires peuvent uniquement être envoyés depuis le site en production https://drees.shinyapps.io/Zonage_ARS/",
    #                    duration = NULL,type = "error",session = session)
    #   
    # }
    
  } else if (text_to_send==""){
    showNotification(ui="Commentaire vide. Ecrivez quelque-chose, toute remarque est bonne à prendre !",
                     duration = 5,session = session)
    
  }
}



