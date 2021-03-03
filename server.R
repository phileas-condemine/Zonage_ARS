function(input, output,session) {
  drop_auth(rdstoken = "droptoken.rds")
  params = fread("params.csv",sep=":")
  
  
  # bvcv_reg_majoritaire
  # tvs_reg_majoritaire
  ##### DEFINE REACTIVES ######
  
  
  fond_de_carte=reactiveVal(F)
  map_coord=reactiveVal(c(0,45))
  timer=reactiveVal(Sys.time())
  timer_qpv=reactiveVal(Sys.time())
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
  default_vals <- reactiveVal(data.table("agr"="","picked_zonage"=""))
  current_mapped_data <- reactiveVal(data.table("agr"="","picked_zonage"=""))
  log_is_admin = reactiveVal(F)
  dropbox_folder = reactiveVal()
  dropbox_ps_folder = reactiveVal()
  TVS = reactiveVal()
  dep_reac = reactiveVal()
  mom_markers = reactiveVal()
  hist_qpv = reactiveVal()
  BVCV = reactiveVal()
  pop_femmes = reactiveVal()
  dep_contours = reactiveVal()
  reg_cont = reactiveVal()
  regions_reac = reactiveVal()
  TA = reactiveVal()
  IP <- reactiveVal()
  VZN_reac = reactiveVal()
  last_force_save = reactiveVal(-1)
  info_recap_reac = reactiveVal()
  dropbox_files = reactiveVal()
  importFile = reactiveVal(NULL)
  
  values = reactiveValues()
  
  
  observeEvent(input$getIP,{IP(input$getIP)})
  
  ##### INITIALIZE IMPORTANT REACTIVES ######
  
  observeEvent(session$clientData$url_pathname,{
    req(session$clientData$url_pathname)
    
    
    if(session$clientData$url_pathname=="/Zonage_ARS/"){
      dropbox_folder("zonage/")
      
    } else {
      dropbox_folder("zonage_dev/")
      showNotification(HTML("<p><b>Attention</b>, vous êtes sur une page de test.<br>",
                            "<a href=\"https://drees.shinyapps.io/Zonage_ARS/\">",
                            "Me rediriger vers la bonne adresse</a>.<br></p>"),duration = NULL,type = "warning")
    }
    
    TVS(get_TVS(dropbox_folder(),params[file=="tvs"]$name))
    dep_reac(unique(TVS()[,c("dep","reg","libdep")]))
    mom_markers(get_QPV(dropbox_folder(),params[file=="qpv"]$name))
    hist_qpv(get_hist_qpv(dropbox_folder(),params[file=="zonage_mg"]$name))
    BVCV(get_BVCV(dropbox_folder(),params[file=="bvcv"]$name))
    pop_femmes({get_pop_femmes(dropbox_folder(),params[file=="pop_femmes"]$name)})
    dep_contours({get_dep_contours(dropbox_folder(),params[file=="contours_dep"]$name)})
    reg_cont({get_reg_contours(dropbox_folder(),params[file=="contours_reg"]$name)})
    regions_reac({get_regions_seuils(dropbox_folder(),params[file=="seuils_arretes"]$name,TVS())})
    TA({get_TA(dropbox_folder(),params[file=="liste_tribunaux"]$name)})
    
    file = "agr_reg_majoritaire.RData"
    local_name = paste0("data/",file)
    drop_name = paste0(dropbox_folder(),file)
    if(drop_exists(drop_name)){
      print("recup régions majoritaires par AGR")
      drop_download(drop_name,local_path = "data",overwrite = T
                    # ,verbose = T
      )
    } else {
      print("construction du fichier des régions majoritaires par AGR from scratch")
      create_and_upload_reg_majo_per_agr(regions=regions_reac(),dep=dep_reac(),dropbox_folder=dropbox_folder(),TVS=TVS(),BVCV=BVCV(),params=params)
    }
    load(local_name)#bvcv_reg_majoritaire & tvs_reg_majoritaire
    setnames(bvcv_reg_majoritaire,"reg_majoritaire","reg")
    setnames(tvs_reg_majoritaire,"reg_majoritaire","reg")
    values$bvcv_reg_majoritaire = bvcv_reg_majoritaire
    values$tvs_reg_majoritaire = tvs_reg_majoritaire
  })
  
  ##### NAVIGATION TABS & LOGIN #####
  
  
  observeEvent(input$go_zonage,{
    print("current mil selected")
    print(input$choix_millesime)
    login_w_key(input,session,millesimes(),has_logged_in())
  })
  
  
  observeEvent(input$go_params,{
    updateTabsetPanel(session,"sidebarmenu","my_params")
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
  
  ##### TUTO #####
  
  observeEvent(input$tuto_params,{
    req(!is.null(input$tuto_params))
    session$sendCustomMessage(type = 'startTutoParams', message = list(""))
    
  })
  
  observeEvent(input$tuto_zonage,{
    req(!is.null(input$tuto_zonage))
    session$sendCustomMessage(type = 'startTutoZonage', message = list(""))
    
  })
  
  ##### CHOIX REG  ##### 
  
  
  output$ui_choix_reg = renderUI({
    regions = regions_reac()
    setorder(regions,libreg)
    selectizeInput('choix_reg','Sélectionner votre région',width = "100%",
                   choices=setNames(regions$reg,regions$libreg),multiple=T,
                   options = list(placeholder = 'Le nom de votre région',plugins= list('remove_button'),maxItems=1))%>%shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Choisissez la région dont vous souhaitez renseigner le zonage.")
                   )
  })
  
  output$ui_choix_reg_map = renderUI({
    if(is.null(input$choix_reg)){
      column(width = 12,shinycssloaders::withSpinner(
        leafletOutput("choixRegMap",height="600px"),
        type=5,size = 1)
      )
    }
  })
  
  output$choixRegMap=renderLeaflet({
    print("create map")
    plot_region(reg_cont())
    
  })
  
  observeEvent(input$choixRegMap_shape_click,{
    click=input$choixRegMap_shape_click
    message(sprintf("select from map reg : %s",click$id))
    updateSelectizeInput(session,"choix_reg",selected=click$id)
    
  })
  
  
  
  
  
  ##### IMPORT USER-DEFINED ZONAGE CSV/XLSX ######
  
  
  output$ui_import_data_model = renderUI({
    
    
    
    
    choix_format_data =   tags$div(HTML('
          <div id="import_data_model" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
             <label class="control-label" for="import_data_model">Format des données</label>
             <div class="shiny-options-group">
               <label class="radio-inline">
                 <input type="radio" name="import_data_model" value="cast"/>
                 <span><img src="https://gitlab.com/DREES_code/public/charte_drees/raw/master/img/data_cast2.PNG" alt="zones en colonnes avec 0-1" height="200px"/></span>
               </label>
               <br/>
               <label class="radio-inline">
                 <input type="radio" name="import_data_model" value="melt"/>
                 <span><img src="https://gitlab.com/DREES_code/public/charte_drees/raw/master/img/data_melt.PNG" alt="zones comme variable sur une colonne" height="200px"/></span>
               </label>
             </div>
          </div> '))
    
    
    tagList(
      uiOutput("ui_file_browser"),
      choix_format_data)
    
  })
  
  output$ui_file_browser = renderUI({
    if(!is.null(input$import_data_model) && length(input$import_data_model)>0){
      file_browser = fileInput("from_file","",buttonLabel = "Parcourir...",
                               placeholder = "Téléverser un fichier de zonage déjà rempli",accept = c(".xls",".xlsx",".csv"))
    } else NULL
    
  })
  
  
  observeEvent(input$from_file,{
    file_import_modal_ui(input,output,session,importFile_reac = importFile)
  })
  observeEvent(c(input$var_zonage,
                 input$import_data_model,
                 input$choix_ps,
                 input$mod_zip,input$mod_zac,input$mod_zv,input$mod_hv,
                 input$mod_tsd,input$mod_sod,input$mod_int,input$mod_td,input$mod_sud),{
                   
                   file_import_form_dynamic_update(input,session,importFile)
                   
                 })
  
  observeEvent(input$parse_file,{
    file_import_validate_join_update(input = input,
                                     output = output,
                                     session = session,
                                     importFile_reac = importFile,
                                     dropbox_ps_folder=dropbox_ps_folder(),
                                     millesimes_reac = millesimes)
  })
  
  ##### LOAD TABLEAU AGR ######
  tableau_reg = reactive({
    if (input$choix_ps == "mg"){
      maj = values$tvs_reg_majoritaire
    } else if (input$choix_ps %in% c("sf","inf")){
      maj = values$bvcv_reg_majoritaire
    }  
    if(has_logged_in()){
      
      tableau_reg_func(input = input,output=output,session = session,
                       dropbox_folder = dropbox_folder(),
                       dropbox_ps_folder = dropbox_ps_folder(),
                       dropbox_files = dropbox_files(),
                       regions=regions_reac(),
                       dep = dep_reac(),
                       fond_de_carte_reac = fond_de_carte,
                       TVS_reac = TVS,BVCV_reac = BVCV,
                       maj=maj,params=params
                       ,VZN_reac=VZN_reac,
                       pop_femmes=pop_femmes(),
                       default_vals=default_vals,
                       current_mapped_data=current_mapped_data)
    }
  })
  
  ##### CURR ZONAGE #####
  
  vals_reac=reactive({
    vals_zonage_func(input,tableau_reg(),has_logged_in())
  })
  
  #### RAPPEL PS & MILLESIME ####
  
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
  
  ##### DISPLAY TABLEAU AGR #####
  
  output$zonage_dt=DT::renderDataTable(server=F,{
    zonage_dt_func(input,tableau_reg())
  })
  
  
  ##### WARNING IF DISABLED AGR IS CLICKED #####
  
  observeEvent(input$zonage_dt_cell_clicked,{
    warning_zonage_clicked(input = input,
                           tableau_reg = tableau_reg(),
                           edition_forced_reac = edition_forced,
                           new_modifs_reac = new_modifs,
                           last_arg_clicked_reac=last_arg_clicked)
  })
  
  ##### USER CAN OVERRIDE CONSTRAINTS AGR-WISE #####
  
  observeEvent(input$last_forced_edition,{
    req(input$last_forced_edition)
    if(!is.null(last_arg_clicked()) && !last_arg_clicked()%in%edition_forced()){
      edition_forced(c(edition_forced(),last_arg_clicked()))
    }
  })
  
  #### MAPS ######
  
  
  
  
  ###### QPV COORD ######
  coord_qpv = reactive({
    
    req(input$choix_reg)
    my_reg=input$choix_reg
    reg_name=regions_reac()[reg==my_reg]$libreg
    my_deps=dep_reac()[reg==my_reg]$dep
    my_qpv <- mom_markers()
    my_qpv$dep = gsub("^0","",substr(my_qpv$CODE_QP,3,5))
    my_qpv = my_qpv[my_qpv$dep%in%my_deps,]
    my_qpv = merge(my_qpv,hist_qpv()[,c("cod","pop")],by.x="CODE_QP",by.y="cod")
    my_qpv = merge(my_qpv,zonage_qpv(),by.x="CODE_QP",by.y="cod")
    my_qpv
  })
  
  ###### QPV TOGGLE ######
  
  observeEvent(c(input$toggle_qpv),{
    if(!is.null(input$toggle_qpv)){
      add_qpv(input,session,coord_qpv())
    }
  })
  
  
  
  ###### QPV MODIF ######
  
  observeEvent(modif_zonage_qpv(),{
    if(!is.null(input$toggle_qpv)){
      update_qpv(input,session,coord_qpv(),modif_zonage_qpv())
    }
  })
  
  
  ###### COMMUNES #####
  
  output$communes_map=renderLeaflet({
    print("Carto")
    if(has_logged_in()){
      plot_communes(input,tableau_reg(),default_vals(),fond_de_carte())
    }
  })
  
  
  #### TABLE - MAP INTERACTION #######
  
  ######### TABLE2MAP #####
  observeEvent(input$last_btn,{
    update_table2map(input,session,vals_reac,current_mapped_data,tableau_reg(),fond_de_carte())
    
  })
  
  observeEvent(input$last_row_hovered,{
    hover_table2map(input,session,fond_de_carte())
  })  
  
  ######### MAP2TABLE #######
  observeEvent(input$communes_map_shape_click,{
    req(input$communes_map_shape_click)
    update_map2table(input$communes_map_shape_click$id,session)
    
  })
  
  
  
  
  observeEvent(input$update_contours,{
    if(!is.null(input$update_contours)&!is.null(input$choix_reg)){
      if(input$update_contours){
        my_reg=input$choix_reg
        showNotification("Une fois les données mises à jour, l'application va redémarrer et vous devrez vous reconnecter.",type = "message",duration = NULL)
        mailles_geo = ifelse(input$choix_ps == "mg","TVS","BVCV")
        prep_geo_data_from_scratch(my_reg = my_reg,
                                   regions = regions_reac(),
                                   dep = dep_reac(),
                                   dropbox_folder = dropbox_folder(),
                                   TVS = TVS(),
                                   BVCV = BVCV(),
                                   refresh_geojson=T,
                                   mailles_geo=mailles_geo,
                                   params=params)
        session$reload()
        
      }
    }
  })
  
  
  ####### EXPORT TABLE######
  output$download_table = downloadHandler(
    function() {
      req(input$choix_reg)
      reg_name=regions_reac()[reg%in%input$choix_reg]$libreg
      paste0(reg_name,".xlsx")
    },
    function(file) {
      
      if(input$choix_ps=="mg"){
        infos = prep_table_to_download(
          input,
          session,
          vals_reac,
          tableau_reg(),
          hist_qpv = hist_qpv(),
          zonage_qpv = zonage_qpv(),
          communes_TVS = communes_TVS
        )
      } else if(input$choix_ps=="sf"){
        infos = prep_table_to_download(input,
                                       session, 
                                       vals_reac, 
                                       tableau_reg(), 
                                       pop_femmes = pop_femmes(),
                                       communes_BVCV = communes_BVCV)
      } else {
        infos = prep_table_to_download(input,
                                       session,
                                       vals_reac,
                                       tableau_reg(),
                                       communes_BVCV = communes_BVCV)
      }
      
      write.xlsx(infos,file)
      
    }
  )
  
  ####### EXPORT MAP ######
  output$download_plot <- downloadHandler(
    function() {
      req(input$choix_reg)
      reg_name=regions_reac()[reg%in%input$choix_reg]$libreg
      paste0(reg_name,"_",input$choix_ps,".png")
    },
    function(file) {
      g = prep_map_to_download(input,session,vals_reac,regions_reac,tableau_reg(),fond_de_carte(),dep_contours())
      ggsave(g,filename = file,width = 16,height = 10.4)
    }
  )
  
  
  ####### EXPORT ARRETE######
  
  observeEvent(input$generate_arrete,{
    
    my_TAs=TA()[reg%in%input$choix_reg]$TA
    form_generate_arrete(input,session,my_TAs)
  })
  output$download_arrete <- downloadHandler(
    function() {
      my_reg=input$choix_reg
      reg_name=regions_reac()[reg%in%my_reg]$libreg
      paste0(input$choix_ps,"_",reg_name,"_model.docx")
    },
    function(file) {
      
      my_reg=input$choix_reg
      reg_name=regions_reac()[reg%in%my_reg]$libreg
      # showNotification("Merci de patienter pendant que nous générons l'arrêté contenant la carte et le(s) tableau(x).",type = "message",duration = NULL)
      
      withProgress(message = "Merci de patienter pendant que nous générons l'arrêté contenant la carte et le(s) tableau(x).", {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        temp_dir=tempdir()
        if(input$choix_ps=="mg"){
          md_params = prep_arrete_to_download(
            input,
            session,
            vals_reac,
            regions_reac,
            tableau_reg(),
            fond_de_carte(),
            temp_dir = temp_dir,
            communes_TVS = communes_TVS,
            hist_qpv = hist_qpv(),
            zonage_qpv = zonage_qpv()
          )
        } else if(input$choix_ps=="sf"){
          md_params = prep_arrete_to_download(
            input,
            session,
            vals_reac,
            regions_reac,
            tableau_reg(),
            fond_de_carte(),
            temp_dir = temp_dir,
            communes_BVCV = communes_BVCV,
            pop_femmes = pop_femmes()
          )
        } else {
          md_params = prep_arrete_to_download(
            input,
            session,
            vals_reac,
            regions_reac,
            tableau_reg(),
            fond_de_carte(),
            temp_dir = temp_dir,
            communes_BVCV = communes_BVCV
          )
        }
        
        
        
        temp_report <- file.path(temp_dir, paste0("create_arrete_",input$choix_ps,".Rmd"))
        
        rmarkdown::render(temp_report, output_file = file,
                          params = md_params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  
  ##### SAVE EN VIGUEUR ######
  
  observeEvent(input$save_envigueur_confirm,{
    
    save_upload_en_vigueur(
      input,
      session,
      info_recap_reac,
      regions_reac,
      vals_reac,
      timer_qpv,
      new_modifs_qpv,
      tableau_reg(),
      log_is_admin(),
      dropbox_ps_folder()
    )
    
    
    
  })
  
  observeEvent(input$save_envigueur,{
    req(!is.null(input$save_envigueur))
    confirm_save_envigueur(session)
  })
  
  observeEvent(c(autorefresh(),input$force_save),{
    
    save_zonage(
      input,
      session,
      regions_reac,
      vals_reac,
      timer,
      timer_qpv,
      last_force_save,
      new_modifs,
      new_modifs_qpv,
      tableau_reg(),
      dropbox_ps_folder()
    )
    
    
    
  })
  
  
  
  ####### JAUGES #####
  
  
  output$gauges = renderUI({
    
    gen_ui_jauges(input)
    
  })  
  
  zonage_pop_reac=reactive({
    
    zonage_pop_func(input,session,vals_reac,hist_qpv,zonage_qpv,tableau_reg())
    
    
  })
  
  zonage_pop_reac_md=reactive({
    
    zonage_pop_md_func(input,session,vals_reac,tableau_reg())
    
  })
  
  output$threshold_ZIP=flexdashboard::renderGauge({
    
    jauge_threshold_ZIP(input,zonage_pop_reac,regions_reac)
    
  })
  
  
  output$threshold_ZAC=flexdashboard::renderGauge({
    
    jauge_threshold_ZAC(input,zonage_pop_reac,regions_reac)
    
  })
  
  
  output$threshold_MD=flexdashboard::renderGauge({ #changer pour que ça soit en % du vivier, pas de la pop régionale
    
    jauge_threshold_MD(input,zonage_pop_reac_md,regions_reac)
    
  })
  
  output$threshold_UD=flexdashboard::renderGauge({ #spécifier les zones d'échange
    
    jauge_threshold_UD(input,zonage_pop_reac,regions_reac)
  })
  
  output$threshold_OD=flexdashboard::renderGauge({ #spécifier les zones d'échange
    
    jauge_threshold_OD(input,zonage_pop_reac,regions_reac)
    
  })  
  
  ##### PIE-DISTR POP #####
  
  
  
  output$dist_zonages = renderPlotly({
    
    pie_plot_distr_pop(input,vals_reac,tableau_reg())
    
  })
  
  
  #### QPV ####
  
  
  
  output$ui_toggle_qpv = renderUI({
    gen_ui_toggle_qpv(input)
  })
  
  output$ui_search_qpv = renderUI({
    gen_ui_search_qpv(input)
    
  })
  
  observeEvent(c(input$search_qpv,input$communes_map_marker_click),{
    
    gen_ui_modif_qpv(input,session,zonage_qpv)
    
  })
  
  output$edit_qpv_options = renderUI({
    
    gen_edit_qpv_options(input,vals_reac,regions_reac,hist_qpv(),zonage_qpv(),tableau_reg(),TVS())
    
  })
  
  observeEvent(c(input$save_zonage_qpv),{
    
    save_qpv_func(input, 
                  session, 
                  new_modifs_qpv, 
                  modif_zonage_qpv, 
                  zonage_qpv())
    
  })
  
  zonage_qpv_en_vigueur = reactive({
    
    dl_zonage_en_vigueur_qpv(input$choix_ps, 
                             dropbox_ps_folder(), 
                             input$choix_reg)
    
  })
  
  zonage_qpv = reactive({
    
    create_table_zonage_qpv(input,
                            modif_zonage_qpv(),
                            hist_qpv(),
                            zonage_qpv_en_vigueur(),
                            dropbox_ps_folder())
    
    
  })
  
  observeEvent(c(vals_reac()),{
    
    warning_qpv_in_tvs(input, 
                       session, 
                       vals_reac, 
                       last_zonage_tvs, 
                       hist_qpv())
    
  })
  
  
  ##### RECAP #####
  
  
  output$recap_dt = renderDataTable({
    
    gen_table_recap_dt(input,
                       session,
                       vals_reac,
                       VZN_reac,
                       info_recap_reac,
                       tableau_reg(),
                       hist_qpv(),
                       zonage_qpv(),
                       communes_TVS,
                       communes_BVCV)
    
  })
  
  ##### JUSTIFICATION #####
  
  output$ui_open_form_justification = renderUI({
    
    gen_ui_open_form_justification(info_recap_reac)
    
  })
  
  observeEvent(input$open_form_justification,{
    
    gen_form_justification(input,session,info_recap_reac,dropbox_ps_folder())  
    
    
  })
  
  observeEvent(input$validation_justification,{
    
    save_justification(input,dropbox_ps_folder())
    
  })
  
  
  
  
  
  ##### LOGIN TO ZONAGE #####
  
  
  
  output$auth=renderText({
    "KO"
  })
  outputOptions(output, "auth", suspendWhenHidden=FALSE)
  observeEvent(c(input$choix_reg),{
    output$auth=renderText({
      "KO"
    })
  })
  
  
  observeEvent(input$send_pwd,{
    
    on_login_actions(
      input,
      output,
      session,
      log_is_admin,
      has_logged_in,
      enable_dl_zonage_en_vigueur,
      get_auth(),
      IP=IP(),info_region = regions_reac()
    ) 
    
    
  })
  
  
  observeEvent(input$send_pwd2,{
    
    on_alt_login_actions(
      input,
      session,
      log_is_admin,
      enable_dl_zonage_en_vigueur,
      get_auth()
    ) 
    
    
    
  })
  
  
  
  
  
  
  
  observeEvent(c(input$choix_reg,input$choix_ps),{
    req(input$choix_reg)
    req(input$choix_ps)
    dropbox_ps_folder(paste0(dropbox_folder(),input$choix_ps,"/"))
    dropbox_files(get_dropbox_files(input,dropbox_ps_folder = dropbox_ps_folder()))
    session$sendCustomMessage(type = 'scrollToBottom', message = list(""))
    
    
  })
  
  
  
  
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
    adjust_width(input$table_width)
    
  })
  
  observeEvent(c(input$feedback_send),{
    req(input$feedback_send)
    send_mail_and_slack_user_feedback(input,session)
    
  })
  
  
  observeEvent(input$save_current_view,{
    req(input$save_current_view)
    my_reg=input$choix_reg
    removeUI(selector = "#forbidden_name",immediate = T,session=session)
    
    if (sum(grepl("(vigueur)|(justification)",input$millesime_name))==0){
      
      if(input$millesime_name == ""){
        insertUI(selector = "#millesime_name",where = "beforeBegin",session = session,immediate = T,
                 ui=tags$div(id="forbidden_name",
                             tags$p(style="color:#f00;",
                                    tags$b("Merci d'indiquer un nouveau nom à donner au projet de zonage sélectionné."))))
        
      } else {
        
        nm = input$millesime_name
        new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_',nm)
        updateSelectizeInput(session,'choix_millesime',
                             choices=c(millesimes(),setNames(new_mil,nm)),
                             selected=new_mil)
        removeModal()
      }
    } else {
      insertUI(selector = "#millesime_name",where = "beforeBegin",session = session,immediate = T,
               ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b('Merci de choisir un autre nom, les mots clefs "en vigueur" et "justification" sont réservés.'))))
    }
  })
  
  observeEvent(input$modal_save_current,{
    showModal(modalDialog(title="Enregistrer le zonage actuel",footer=NULL,easyClose = T,size="m",
                          fluidRow(column(9,textInput("millesime_name","Nom de l'enregistrement",placeholder = "Zonage_pour_arrêté_2021")),
                                   column(3,tags$label("Enregistrer"),br(),
                                          actionButton('save_current_view',"",icon=icon("save")))),
                          helpText("Le nom de la région sera ajouté automatiquement.\nPensez à préciser l'année dans le nom de l'enregistrement.\nL'année est particulièrement importante s'il s'agit d'un zonage arrêté.")))
  })
  
  output$ui_millesime=renderUI({
    
    
    
    get_millesimes(input,session,millesimes_reac = millesimes,
                   info_region = regions_reac(),
                   dropbox_ps_folder = dropbox_ps_folder())
    
    if(length(millesimes())>0){
      tagList(selectizeInput('choix_millesime',"",width="100%",
                             choices=millesimes(),selected=millesimes()[1],
                             options = list(placeholder = 'Dernier arrêté ou saisie en cours',
                                            plugins= list('remove_button'))),
              actionBttn("rename_millesime","Renommer le projet",icon = icon("edit"),size = "sm",block=T,
                         style = "pill",color = "warning"))
    } else {
      HTML("<p>Aucun projet n'a été trouvé.</p>",
           "<p>Merci de créer un projet en utilisant le menu situé à droite.</p>")
    }
  })
  
  observeEvent(input$rename_millesime,{
    req(input$rename_millesime)
    mil_list = millesimes()
    mil_name = names(mil_list[mil_list==input$choix_millesime])
    showModal(modalDialog(title="Renommer le projet sélectionner",
                          HTML(sprintf("<p>Le nom actuel du projet est %s.</p>",mil_name)),
                          textInput("new_name_millesime","Nouveau nom",value = "",placeholder =  mil_name),
                          passwordInput("key_to_validate_rename_millesime",label = "Identification requise",placeholder = "Clef d'identification"),
                          footer = tagList(modalButton("Annuler"),
                                           actionButton("validate_rename_millesime","Valider",icon=icon("check")))),
              session=session)
    
  })
  
  
  observeEvent(input$validate_rename_millesime,{
    req(input$validate_rename_millesime)
    auth = get_auth()
    auth = auth[key==input$key_to_validate_rename_millesime & reg == input$choix_reg]
    
    if(nrow(auth)==0){
      if(input$key_to_validate_rename_millesime == ""){
        removeUI(selector = "#invalid_key",immediate = T,session=session)
        insertUI(selector = "#key_to_validate_rename_millesime",
                 where = "beforeBegin",session = session,immediate = T,
                 ui=tags$div(id="invalid_key",
                             tags$p(style="color:#f00;",
                                    tags$b(
                                      "Merci d'entrer la clef d'identification de votre région pour valider l'action"))))
        
        
      } else {
        removeUI(selector = "#invalid_key",immediate = T,session=session)
        insertUI(selector = "#key_to_validate_rename_millesime",
                 where = "beforeBegin",session = session,immediate = T,
                 ui=tags$div(id="invalid_key",
                             tags$p(style="color:#f00;",
                                    tags$b(sprintf(
                                      "Clef d'identification erronée, vous avez écrit \"%s\".",
                                      input$key_to_validate_rename_millesime)))))
        
        
      }
    } else {
      removeUI(selector = "#invalid_key",immediate = T,session=session)
      removeUI(selector = "#forbidden_name",immediate = T,session=session)
      
      old_mil = input$choix_millesime
      mil_list = millesimes()
      old_mil_name = names(mil_list[mil_list==old_mil])
      dropbox_files = drop_dir(dropbox_ps_folder())
      dropbox_files = data.table(dropbox_files)
      reg_ps = paste0("^",input$choix_ps,"_",input$choix_reg,"_")
      proj_in_reg_ps = dropbox_files[grep(reg_ps,name)]$name
      proj_in_reg_ps = gsub(reg_ps,"",proj_in_reg_ps)
      
      if (sum(grepl("(vigueur)|(justification)",input$new_name_millesime))==0){
        if(input$new_name_millesime == ""){
          insertUI(selector = "#new_name_millesime",where = "beforeBegin",session = session,immediate = T,
                   ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b("Merci d'indiquer un nouveau nom à donner au projet de zonage sélectionné."))))
          
        } else if (input$new_name_millesime == old_mil_name) {
          insertUI(selector = "#new_name_millesime",where = "beforeBegin",session = session,immediate = T,
                   ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b("Merci d'indiquer un nom différent du nom actuel du projet sélectionné."))))
        } else if (input$new_name_millesime %in% proj_in_reg_ps){
          
          insertUI(selector = "#new_name_millesime",where = "beforeBegin",session = session,immediate = T,
                   ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b("Un projet portant ce nom existe déjà. Merci de choisir un autre nom."))))
          
          
        } else {
          
          nm = input$new_name_millesime
          new_mil = paste0(input$choix_ps,'_',input$choix_reg,'_',nm)
          

          
          

          to_rename = dropbox_files[grepl(sprintf("(%s$)|(%s.csv$)",old_mil,old_mil),name)]$name
          start_warning = Sys.time()
          insertUI(selector = ".modal-body",where = "beforeEnd",session = session,immediate = T,
                   ui=tags$div(id="rename_and_reload",
                               tags$p(style="color:#f00;",
                                      tags$b(sprintf(
                                        "Les fichiers [%s] vont être renommés puis l'application sera relancée afin que vous puissiez charger le projet sous son nouveau nom.",paste(to_rename,collapse=", "))))))
          
          for (old_file_name in to_rename){
            
            new_file_name = gsub(old_mil,new_mil,old_file_name)
            new_file_name = gsub(" ","_",new_file_name)
            path = dropbox_ps_folder()
            drop_move(from_path = paste0(path,old_file_name),to_path = paste0(path,new_file_name))
            
          }
          
          end_time = difftime(Sys.time(),start_warning,units = "secs")
          time_to_wait = 5 - end_time
          if (time_to_wait>0){
            Sys.sleep(time_to_wait)
          }
          session$reload()
          
        }
      } else {
        insertUI(selector = "#new_name_millesime",where = "beforeBegin",session = session,immediate = T,
                 ui=tags$div(id="forbidden_name",tags$p(style="color:#f00;",tags$b('Merci de choisir un autre nom, les mots clefs "en vigueur" et "justification" sont réservés.'))))
      }
      
      
      
      
      
      
      
    }
    
  })
  
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
  ##### LOGOUT  #######
  
  observeEvent(input$logout,{
    req(input$logout)
    session$reload()
  })
  
  
  ##### DL BUTTONS #####
  
  output$ui_doc_dl = renderUI({
    gen_buttons_sidebar_dl(input)
  })
  
  
  ##### DL REF ZONAGE #####
  
  output$dl_ref_zonage_med <- downloadHandler(
    filename = 'ref_zonage_mg.xlsx',
    function(file) {
      showNotification("Ce fichier contient également la population des QPV",session=session)
      
      slack_dropdl_userdl(input,
                          session,
                          file,
                          params,
                          my_file = "zonage_mg",
                          txt_slack = "[ref_zonage_mg.xlsx]",
                          log_is_admin(),
                          dropbox_folder())
      
    }
  )
  
  output$dl_ref_zonage_sf <- downloadHandler(
    filename = 'ref_zonage_sf.xlsx',
    function(file) {
      slack_dropdl_userdl(input,
                          session,
                          file,
                          params,
                          my_file = "zonage_sf",
                          txt_slack = "[zonage_sf -> ref_zonage_mg.xlsx]",
                          log_is_admin(),
                          dropbox_folder())
    }
  )
  
  output$dl_ref_zonage_ide <- downloadHandler(
    filename = 'ref_zonage_ide.xlsx',
    function(file) {
      slack_dropdl_userdl(input,
                          session,
                          file,
                          params,
                          my_file = "zonage_inf",
                          txt_slack = "[zonage_inf -> ref_zonage_ide.xlsx]",
                          log_is_admin(),
                          dropbox_folder())
      
    }
  )
  ##### DL CORRES AGR <-> COM #####
  
  output$dl_corres_tvs_com <- downloadHandler(
    filename = 'corres_tvs_com.xlsx',
    function(file) {
      slack_dropdl_userdl(input,
                          session,
                          file,
                          params,
                          my_file = "tvs",
                          txt_slack = "[tvs -> corres_tvs_com.xlsx]",
                          log_is_admin(),
                          dropbox_folder())
      
    }
  )
  output$dl_corres_bvcv_com <- downloadHandler(
    filename = 'corres_bvcv_com.xlsx',
    function(file) {
      slack_dropdl_userdl(input,
                          session,
                          file,
                          params,
                          my_file = "bvcv",
                          txt_slack = "[bvcv -> corres_bvcv_com.xlsx]",
                          log_is_admin(),
                          dropbox_folder())
    }
  )
  
  
  ##### DL POP #####
  
  
  
  output$dl_pop_bvcv_femmes <- downloadHandler(
    filename = 'pop_bvcv_femmes.xlsx',
    function(file) {
      if(!log_is_admin())
        slack_log("pop_bvcv_femmes.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(pop_femmes(),file)
    }
  )
  
  output$dl_pop_tvs <- downloadHandler(
    filename = 'pop_tvs.xlsx',
    function(file) {
      if(!log_is_admin())
        slack_log("[req_preprocessed_TVS.RData -> pop_tvs.xlsx]",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      load(paste0("data/",input$choix_reg,"_preprocessed_TVS.RData"))
      pop_tvs = data.table(communes_TVS)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_tvs) <- c("Région","Département","TVS","Nom TVS","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_tvs,file)
    }
  )
  
  output$dl_pop_bvcv_all <- downloadHandler(
    filename = 'pop_bvcv.xlsx',
    function(file) {
      if(!log_is_admin())
        slack_log("pop_bvcv.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      load(paste0("data/",input$choix_reg,"_preprocessed_BVCV.RData"))
      pop_bvcv = data.table(communes_BVCV)[,c("reg","dep","agr","libagr","depcom","libcom","population")]
      names(pop_bvcv) <- c("Région","Département","BVCV","Nom BVCV","Commune","Nom commune","Population")
      openxlsx::write.xlsx(pop_bvcv,file)
      
    }
  )
  
  ##### DL REG MAJO AGR #####
  
  output$dl_reg_maj_tvs = downloadHandler(
    filename = 'region_majoritaire_TVS.xlsx',
    function(file) {
      if(!log_is_admin())
        slack_log("region_majoritaire_TVS.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(list("region_majoritaire_par_TVS" = values$tvs_reg_majoritaire,
                                "codes_regions"=regions_reac()[,c("reg","libreg")]),file)
    }
  )
  
  output$dl_reg_maj_bvcv = downloadHandler(
    filename = 'region_majoritaire_BVCV.xlsx',
    function(file) {
      if(!log_is_admin())
        slack_log("region_majoritaire_BVCV.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
      openxlsx::write.xlsx(list("region_majoritaire_par_BVCV" = values$bvcv_reg_majoritaire,
                                "codes_regions"=regions_reac()[,c("reg","libreg")]),file)
    }
  )
  
  
  ##### DL EN VIGUEUR #####
  
  output$dl_zonage_en_vigueur_mg <- downloadHandler(
    filename = 'Zonage_en_vigueur_mg.xlsx',
    function(file) {
      if(enable_dl_zonage_en_vigueur()){
        en_vigueur_agr = dl_zonage_en_vigueur_agr("mg",paste0(dropbox_folder(),"mg/"),"",maj=values$tvs_reg_majoritaire)
        en_vigueur_agr = prepare_zonage_en_vigueur_for_export(en_vigueur_agr,"mg",maj=values$tvs_reg_majoritaire,TVS = TVS())
        en_vigueur_com = prepare_zonage_en_vigueur_com_for_export(en_vigueur_agr,"mg",AGR=TVS())
        en_vigueur_qpv = dl_zonage_en_vigueur_qpv("mg",paste0(dropbox_folder(),"mg/"),"")
        
        if(nrow(en_vigueur_agr)>0){
          if(!log_is_admin())
            slack_log("Zonage_en_vigueur_mg.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$region)),type="message",duration=10)
          openxlsx::write.xlsx(list("TVS"=en_vigueur_agr,"COM"=en_vigueur_com,"QPV"=en_vigueur_qpv),file = file)
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
    filename = 'Zonage_en_vigueur_sf.xlsx',
    function(file) {
      if(enable_dl_zonage_en_vigueur()){
        en_vigueur_agr = dl_zonage_en_vigueur_agr("sf",paste0(dropbox_folder(),"sf/"),"",maj=values$bvcv_reg_majoritaire)
        en_vigueur_agr = prepare_zonage_en_vigueur_for_export(en_vigueur_agr,"sf",maj=values$bvcv_reg_majoritaire,TVS = TVS(),BVCV = BVCV())
        en_vigueur_com = prepare_zonage_en_vigueur_com_for_export(en_vigueur_agr,"sf",AGR=BVCV())
        if(nrow(en_vigueur_agr)>0){
          if(!log_is_admin())
            slack_log("Zonage_en_vigueur_sf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$region)),type="message",duration=10)
          openxlsx::write.xlsx(list("BVCV"=en_vigueur_agr,"COM"=en_vigueur_com),file = file)
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
    filename = 'Zonage_en_vigueur_inf.xlsx',
    function(file) {
      if(enable_dl_zonage_en_vigueur()){
        en_vigueur_agr = dl_zonage_en_vigueur_agr("inf",paste0(dropbox_folder(),"inf/"),"",maj=values$bvcv_reg_majoritaire)
        en_vigueur_agr = prepare_zonage_en_vigueur_for_export(en_vigueur_agr,"inf",maj=values$bvcv_reg_majoritaire,TVS = TVS(),BVCV=BVCV())
        en_vigueur_com = prepare_zonage_en_vigueur_com_for_export(en_vigueur_agr,"inf",AGR=BVCV())
        if(nrow(en_vigueur_agr)>0){
          if(!log_is_admin())
            slack_log("Zonage_en_vigueur_inf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          showNotification(sprintf("Actuellement %s ARS ont validé leur zonage sur l'application",uniqueN(en_vigueur_agr$region)),type="message",duration=10)
          openxlsx::write.xlsx(list("BVCV"=en_vigueur_agr,"COM"=en_vigueur_com),file = file)
          
          print("is admin ?")
          print(log_is_admin())
          if(!log_is_admin())
            slack_log("zonages_en_vigueur_inf.xlsx",input$choix_reg,input$choix_ps,input$choix_millesime,session)
          
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
  ##### DL FAQ #####
  
  output$dl_faq_hors_mg <- downloadHandler(
    filename = 'FAQ_hors_mg.pdf',
    function(file) {
      file.copy("www/FAQ_hors_mg.pdf", file, overwrite = T)
      
    }
  )
  
  output$dl_faq_mg <- downloadHandler(
    filename = 'FAQ_mg.pdf',
    function(file) {
      file.copy("www/FAQ_mg.pdf", file, overwrite = T)
      
    }
  )
  
  ##### A CLASSER #####
  
  
  
  
  
  
  
  
}

