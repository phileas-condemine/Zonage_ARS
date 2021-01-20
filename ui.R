tagList(
  
  
  
  dashboardPage(
    tags$header(class = "main-header", span(class = "logo",style="background: #1263b3;", "Zonage ARS"),
                tags$nav(class = "navbar navbar-static-top",style="background: #0253a3;",
                         role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                         a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                           role = "button", span(class = "sr-only", "Toggle navigation")),
                         div(class = "navbar-custom-menu",
                             tags$ul(class = "nav navbar-nav",
                                     # tags$li(downloadLink("dl_faq_hors_mg",HTML("<span><i class=\"fa fa-download\"></i> FAQ IDE-SF-MK-Ortho</span>"),style="color:#fff;"),
                                     #          class= 'dropdown'),
                                     tags$li(id="logo_ministere",
                                             a(tags$i(class="fa icon_ministere text-success vert_center"),"Solidarités Santé",href="http://solidarites-sante.gouv.fr/",
                                               target="_blank",  rel="noopener noreferrer",style="padding-top:5px;padding-bottom:5px")),
                                     tags$li(id="logo_drees",
                                             a(tags$i(class="fa icon_drees text-success vert_center"),"Développeur",href="http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/",
                                               target="_blank",  rel="noopener noreferrer",style="padding-top:5px;padding-bottom:5px")),
                                     # https://resizeimage.net/
                                     
                                     tags$li(id="Github",
                                             a(tags$i(class="fa icon_github text-success vert_center"),"Code Source",href="https://gitlab.com/DREES_code/formulaire_zonage_ars",
                                               target="_blank",  rel="noopener noreferrer",style="padding-top:5px;padding-bottom:5px")),
                                     tags$li(actionLink("logout","Déconnexion",icon=icon("sign-out-alt")),class= 'dropdown')
                             )))
                
                ,includeCSS("www/my_styles.css")
                # https://stackoverflow.com/questions/17966089/how-to-replace-and-with-lt-and-gt-with-jquery-or-js
                ,includeScript("www/custom_scripts.js")
                ,includeScript("www/get_ip.js")
                
                ,useShinyalert()  # Set up shinyalert
                ,useShinyjs()
                
                
                
                
    ),
    
    dashboardSidebar(collapsed = F,
                     sidebarMenu(id="sidebarmenu",
                                 menuItem("Accueil",icon = shiny::icon("home"),tabName="accueil"),
                                 
                                 menuItem("Choix de la profession",icon = shiny::icon("home"),tabName="my_params"),
                                 
                                 menuItem("Élaboration du zonage",icon = shiny::icon("tasks"),tabName="zonage"),
                                 menuItem(text = "Paramétrage",icon = shiny::icon("gear"),
                                          # ,prettySwitch("display_non_modifiable","Avec zones hors-cadre",value=F)
                                          conditionalPanel("input.choix_ps !== null",
                                                           fluidRow(
                                                             div(style="margin-left:15px",
                                                                 uiOutput("ui_params"))
                                                           )
                                                           # fluidRow(
                                                           #   div(style="margin-left:15px",
                                                           #       sliderInput("table_width","Ajuster la table",min=0,max=12,value=8))
                                                           # )
                                                           
                                          )
                                 ),
                                 menuItem(text="Documents",icon = icon("download"),
                                          conditionalPanel("input.choix_ps !== null",
                                                           fluidRow(
                                                             div(#style="margin-left:15px",
                                                               uiOutput("ui_doc_dl"))
                                                           )
                                          )
                                 ),
                                 
                                 menuItem(text="Nous contacter",icon=icon("question"),
                                          textAreaInput("feedback_content","Message",placeholder="Bonjour\nJ'ai trouvé dans le site une incohérence\nJ'ai une suggestion...\nMerci\nSignature ou anonyme",height = "200px"),
                                          textInput("adresse_mail","Adresse e-mail",placeholder = "Albert.Dupont@sante.gouv.fr"),
                                          textInput("name_sender","NOM et Prénom",placeholder ="Albert Dupont, ARS IdF"),
                                          actionButton("feedback_send","Nous contacter",icon=icon("feather")),
                                          tags$br(),
                                          tags$br()
                                 ))
                     # ,includeHTML("www/logos.html")
                     # ,tags$img(src="Logo_Drees.jpg")
    ),
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "accueil",
                includeHTML("www/accueil_1.html"),
                fluidRow(div(style="margin-left:40px",actionButton("go_params","Choix de la région, de la profession de santé"))),
                tags$br(),
                includeHTML("www/accueil_2.html"),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br()
                # ,includeHTML("www/footer_accueil.html")
        ),
        tabItem(tabName = "my_params",
                fluidRow(id="choix_region_millesime",
                         div(class="col-sm-12 inbody_selector",
                             uiOutput("ui_choix_reg")
                         )),
                fluidRow(
                  conditionalPanel("input.choix_reg === null",
                                   column(12,shinycssloaders::withSpinner(
                                     leafletOutput("choix_reg_map",height="600px"),type=5,size = 1))
                  )),
                fluidRow(id="choix_profession_sante",
                         div(class="col-sm-12 inbody_selector",
                             selectizeInput('choix_ps','Sélectionner une profession de santé',width = "100%",
                                            choices=list_PS,multiple=T,selected="",
                                            options = list(plugins= list('remove_button')
                                                           ,placeholder = 'Une profession de santé'
                                                           ,maxItems=1))%>%shinyInput_label_embed(
                                                             icon("question-circle") %>%
                                                               bs_embed_tooltip(title = "Choisissez la profession de santé dont vous souhaitez renseigner le zonage.")
                                                           ))),
                # conditionalPanel("input.choix_ps !== null",
                #                  fluidRow(
                #                    div(style="margin-left:15px",
                #                        uiOutput("ui_params"))
                #                    
                #                  )
                # ),
                conditionalPanel("input.choix_reg !== null",
                                 box(width = 3,title = "Charger un projet de zonage existant",
                                     column(11,uiOutput("ui_millesime")),
                                     actionButton("refresh_millesime","",shiny::icon("redo"))),
                                 box(width = 3,title = "Nouveau projet de zonage",
                                     actionButton('modal_save_current',"",icon=icon("save"))),
                                 div(id = "file_import_box",box(width = 6, 
                                                                title = "Importer un projet de zonage local", 
                                                                collapsible = T, collapsed = T, 
                                                                conditionalPanel("input.choix_ps !== null",
                                                                                 
                                                                                 conditionalPanel("(typeof input.import_data_model !== 'undefined' && input.import_data_model.length > 0)",
                                                                                                  fileInput("from_file","",buttonLabel = "Parcourir...",
                                                                                                            placeholder = "Fichier de zonage déjà rempli",accept = c(".xls",".xlsx",".csv"))),
                                                                                 tags$div(HTML('
                                                                                              <div id="import_data_model" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                                                                                <label class="control-label" for="import_data_model">Format des données</label>
                                                                                                <div class="shiny-options-group">
                                                                                                  <label class="radio-inline">
                                                                                                    <input type="radio" name="import_data_model" value="cast"/>
                                                                                                    <span><img src="https://gitlab.com/DREES_code/public/charte_drees/raw/master/img/data_cast2.PNG" alt="zones en colonnes avec 0-1" height="200px"/></span>
                                                                                                  </label>
                                                                                                  <label class="radio-inline">
                                                                                                    <input type="radio" name="import_data_model" value="melt"/>
                                                                                                    <span><img src="https://gitlab.com/DREES_code/public/charte_drees/raw/master/img/data_melt.PNG" alt="zones comme variable sur une colonne" height="200px"/></span>
                                                                                                  </label>
                                                                                                </div>
                                                                                              </div> '))
                                                                ),
                                                                conditionalPanel("!(input.choix_ps !== null)",
                                                                                 tags$h3("Vous devez d'abord sélectionner une profession de santé"))
                                 )),
                                 uiOutput("ui_go_zonage")
                                 # conditionalPanel("input.choix_millesime !== null",
                                 #                  box(width = 12, 
                                 #                      actionBttn(
                                 #                        inputId = "go_zonage",
                                 #                        label = "Accéder au formulaire de zonage",
                                 #                        color = "success",size = "lg",
                                 #                        style = "material-flat",
                                 #                        icon = icon("door-open"),
                                 #                        block = TRUE
                                 #                      ))
                                 #                  )
                                 # actionButton("go_zonage","Accéder au formulaire de zonage",icon=shiny::icon("door-open")))
                                 
                                 
                ),
                tags$br(),
                tags$br()
                
                # ,includeHTML("www/footer_catalogue.html")
        ),
        tabItem(tabName = "zonage",
                conditionalPanel(condition="output.auth=='OK'",
                                 fluidRow(
                                   # uiOutput("box_tableau"),
                                   # uiOutput("box_carte_jauges")
                                   div(id="box_tableau",box(width = 8,
                                                            uiOutput("rappel_ps"),
                                                            uiOutput("ui_toggle_qpv"),
                                                            uiOutput("ui_search_qpv"),
                                                            uiOutput("ui_open_form_justification"),
                                                            # uiOutput("ui_zonage_dt"),
                                                            DTOutput("zonage_dt"),
                                                            tags$br(),
                                                            fluidRow(
                                                              column(2,actionButton("force_save","Sauvegarder",icon=shiny::icon("save"))),
                                                              column(6,textOutput("nb_modif_unsaved")),
                                                              column(4,tags$div(id="loading"))),
                                                            actionBttn(
                                                              inputId = "save_latest",
                                                              label = "Valider ce zonage",
                                                              color = "success",size = "lg",
                                                              style = "material-flat",
                                                              icon = icon("check"),
                                                              block = TRUE
                                                            ),
                                                            tags$br(),
                                                            DTOutput("recap_dt")
                                   )),
                                   div(id="box_carte_jauges",box(width = 4,
                                                                 conditionalPanel("input.choix_reg !== null",
                                                                                  fluidRow(
                                                                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
                                                                                                 downloadButton(outputId="download_plot",label="Carte"))),
                                                                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
                                                                                                 downloadButton(outputId="download_table",label="Tableau"))),
                                                                                    column(4,div(style="text-align: center;margin-bottom: 10px;",
                                                                                                 actionButton("generate_arrete","Arrêté",icon=icon("edit"))
                                                                                    ))
                                                                                  ),
                                                                                  
                                                                                  fluidRow(
                                                                                    column(12,
                                                                                           leafletOutput("communes_map",width = "auto"))
                                                                                  ),
                                                                                  uiOutput("gauges"),
                                                                                  tags$br(),
                                                                                  fluidRow(
                                                                                    column(6,textOutput("date_contours_update")),
                                                                                    column(6,actionButton("update_contours","Mettre à jour Géo.",icon=icon("cogs")))
                                                                                  ),
                                                                                  tags$br(),
                                                                                  fluidRow(
                                                                                    column(12,plotlyOutput("dist_zonages",width="auto"))
                                                                                  ),
                                                                                  tags$br()
                                                                 )
                                   )
                                   )
                                 )
                )             
                # ,includeHTML("www/footer_catalogue.html"))
        )
      )
    )
  ),
  includeHTML("www/footer_accueil.html")
)
  