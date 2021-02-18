# 
# ###### LOGOUT  #######
# observeEvent(input$logout,{
#   req(input$logout)
#   session$reload()
# })
# observeEvent(input$sidebarmenu,{
#   if(has_logged_in()&input$sidebarmenu%in%c("accueil","my_params")){
#     session$reload()
#   }
# })
# 
# 
# ###### LOGIN TO ZONAGE #####
# observeEvent(input$go_zonage,{
#   print("current mil selected")
#   print(input$choix_millesime)
#   login_w_key(input,session,millesimes())
# })
# 
# observeEvent(c(input$choix_reg,input$choix_ps,input$choix_millesime),{
#   print("valeur par défaut import data model")
#   print(input$import_data_model)
#   output$auth=renderText({
#     "KO"
#   })
# })
# 
# 
# 
# 
# 
# observeEvent(input$send_pwd,{
#   req(input$my_auth)
#   auth = get_auth()
#   auth = auth[key==input$my_auth & reg == input$choix_reg]
#   if(nrow(auth)>0){
#     # print("OK")
#     output$auth=renderText({
#       "OK"
#     })
#     
#     if(any(grepl("phileas",auth$name))){
#       log_is_admin(T)
#       key = "clef universelle"
#     } else {
#       log_is_admin(F)
#       key = paste0("région ",auth$name)
#     }
#     
#     
#     
#     
#     
#     reg = ifelse(!is.null(input$choix_reg),input$choix_reg,"XX")
#     ps = ifelse(!is.null(input$choix_ps),input$choix_ps,"XX")
#     mil = ifelse(!is.null(input$choix_millesime),input$choix_millesime,"XX")
#     
#     if(session$clientData$url_pathname=="/Zonage_ARS/" & !log_is_admin()){
#       message=sprintf("App:ZonageARS\nEvent: Connexion de la région %s pour la profession %s avec le projet %s",reg,ps,mil)
#       try({
#         slackr_setup(config_file = "www/slackr_config_log.txt",echo = F)
#         slackr_bot(message)
#       })
#       send_mail_user_login(my_reg = reg,my_ps = ps)
#     }
#     
#     has_logged_in(T)
#     enable_dl_zonage_en_vigueur(T)
#     outputOptions(output, "auth", suspendWhenHidden=FALSE)
#     removeModal()
#     removeNotification("error_w_key",session)
#     
#   } else {
#     
#     removeNotification("error_w_key",session)
#     showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL)
#   }
#   
# })
# 
# 
# observeEvent(input$send_pwd2,{
#   req(input$my_auth2)
#   auth = get_auth()
#   auth = auth[key==input$my_auth2]
#   if(nrow(auth)>0){
#     
#     if(any(grepl("phileas",auth$name))){
#       log_is_admin(T)
#       key = "clef universelle"
#     } else {
#       log_is_admin(F)
#       key = paste0("région ",auth$name)
#     }
#     
#     
#     enable_dl_zonage_en_vigueur(T)
#     removeModal()
#     removeNotification("error_w_key",session)
#     showNotification("Identification validée, merci de renouveler la demande de téléchargement.",type = "message")
#     
#   } else {
#     
#     removeNotification("error_w_key",session)
#     showNotification("Clef erronnée",type="error",id="error_w_key",duration=NULL)
#   }
#   
# })
# 
# observeEvent(input$go_params,{
#   updateTabsetPanel(session,"sidebarmenu","my_params")
# })
# 
# observeEvent(input$choix_reg_map_shape_click,{
#   print("got clicked in map shape")
#   print(input$choix_reg_map_shape_click)
#   click=input$choix_reg_map_shape_click
#   updateSelectizeInput(session,"choix_reg",selected=click$id)
#   
# })
# 
# 
# output$choix_reg_map=renderLeaflet({
#   print("create map")
#   
#   plot_region(reg_cont())
#   
# })
# 
# dropbox_files = reactiveVal()
# 
# observeEvent(c(input$choix_reg,input$choix_ps),{
#   req(input$choix_reg)
#   req(input$choix_ps)
#   
#   dropbox_files(get_dropbox_files(input,dropbox_ps_folder = dropbox_ps_folder()))
#   
# })
# 
# 
# 
# 
# output$ui_params = renderUI({
#   req(input$choix_ps)
#   tagList(
#     sliderInput("table_width","Ajuster la table",min=0,max=12,value=8),
#     shinyWidgets::switchInput(inputId = "remove_alerte_jauge",
#                               label = "Alertes Jauges",
#                               value = F,
#                               onLabel = "Désactivée",offLabel = "Activées",
#                               labelWidth = "200",handleWidth = "100",
#                               size = "normal",inline = T
#     )
#   )
#   
#   
# })
# 
# outputOptions(output, "ui_params", suspendWhenHidden=FALSE)
# 
# observeEvent(input$table_width,{
#   req(input$table_width)
#   adjust_width(input$table_width)
#   
# })
# 
# observeEvent(c(input$feedback_send),{
#   req(input$feedback_send)
#   send_mail_and_slack_user_feedback(input,session)
#   
# })
