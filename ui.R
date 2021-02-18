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
      includeCSS("www/bootstrap-tour-standalone.min.css")
      ,includeScript("www/bootstrap-tour-standalone.min.js")
      ,includeScript("www/integration_bootstrap_tour.js")
      ,tabItems(
        tabItem(tabName = "accueil",
                includeHTML("www/accueil_1.html"),
                fluidRow(div(style="margin-left:40px",
                             # actionButton("go_params","Choix de la région, de la profession de santé")
                             actionBttn(
                               "go_params",
                               "Accéder à la page suivante : choix de la région, de la profession de santé",
                               style = "material-flat",
                               color = "primary",
                               size = "md",
                               block = T,          
                               icon = icon("door-open")
                             )
                )),
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
                fluidRow(
                  actionBttn(
                    "tuto_params",
                    "Lancer la viste guidée",
                    style = "pill",
                    color = "default",
                    size = "md",
                    block = T,
                    icon = icon("help")
                    
                  )
                ),
                fluidRow(
                  column(width = 12,
                         uiOutput("ui_choix_reg")
                  )),
                fluidRow(
                  uiOutput("ui_choix_reg_map")),
                fluidRow(id = "row_choix_ps",
                         column(width = 12,
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
                # conditionalPanel("input.choix_reg !== null",
                fluidRow(
                  div(id="load_existing_proj",box(width = 3,title = "Charger un projet de zonage existant",height = "600px",
                                                  column(11,uiOutput("ui_millesime"),
                                                         # actionButton("refresh_millesime","",icon("redo"))),
                                                         actionBttn(
                                                           inputId = "refresh_millesime",
                                                           label = "Rafraîchir la liste",
                                                           icon = icon("redo"),
                                                           size = "sm",
                                                           block = T
                                                         )))),
                  div(id="create_new_proj",box(width = 3,title = "Nouveau projet de zonage",height = "600px",
                                               actionBttn(
                                                 inputId = "modal_save_current",
                                                 label = "Créer un nouveau projet",
                                                 icon = icon("save"),
                                                 size = "sm",
                                                 block = T
                                               ))),
                  
                  div(id = "file_import_box",box(width = 6,height = "600px",
                                                 title = "Importer un projet de zonage local", 
                                                 collapsible = T, collapsed = F, 
                                                 uiOutput("ui_import_data_model")
                                                 
                  )),
                  
                  
                  
                ),
                fluidRow(
                  column(12,
                         div(id="box_go_zonage",
                             box(width=12,
                                 uiOutput("ui_go_zonage")
                                 
                             ))
                  )
                ),
                br(),
                br(),
                br()
                
                # ,includeHTML("www/footer_catalogue.html")
        ),
        tabItem(tabName = "zonage",
                conditionalPanel(condition="output.auth=='KO'",
                                 fluidRow(box(width=12,title="Connexion nécessaire",
                                              "<p>Merci de retourner sur l'onglet \"Choix de la profession\" pour vous identifier.</p>",
                                              "<p>Si le chargement du zonage est en cours (notification en bas à droite de la page) vous pouvez ignorer ce message, le tableau apparaîtra bientôt sur l'écran.</p>"))),
                conditionalPanel(condition="output.auth=='OK'",
                                 fluidRow(
                                   # uiOutput("box_tableau"),
                                   # uiOutput("box_carte_jauges")
                                   div(id="box_tableau",
                                       box(width = 8,
                                           fluidRow(
                                           column(7,uiOutput("rappel_ps")),
                                           column(5,br(),actionBttn(
                                             "tuto_zonage",
                                             "Lancer la viste guidée",
                                             style = "pill",
                                             color = "default",
                                             size = "md",
                                             block = T,
                                             icon = icon("help")
                                             
                                           ))),  
                                           fluidRow(column(12,
                                             
                                           uiOutput("ui_toggle_qpv"))),
                                           fluidRow(column(12,
                                             uiOutput("ui_search_qpv"))),
                                           fluidRow(column(12,
                                             uiOutput("ui_open_form_justification"))),
                                           # uiOutput("ui_zonage_dt"),
                                           fluidRow(column(12,
                                             DTOutput("zonage_dt"))),
                                           tags$br(),
                                           fluidRow(column(12,
                                             column(2,actionButton("force_save","Sauvegarder",icon=shiny::icon("save"))),
                                             column(6,textOutput("nb_modif_unsaved")),
                                             column(4,tags$div(id="loading")))),
                                           fluidRow(column(12,
                                             actionBttn(
                                             inputId = "save_envigueur",
                                             label = "Valider ce zonage",
                                             color = "success",size = "lg",
                                             style = "material-flat",
                                             icon = icon("check"),
                                             block = TRUE
                                           ))),
                                           tags$br(),
                                           fluidRow(column(12,
                                             DTOutput("recap_dt")))
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
