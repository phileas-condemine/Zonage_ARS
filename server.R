function(input, output,session) {
  
  # init_gs=reactiveVal(T)
  # zonage_reactif=reactiveVal("")
  fond_de_carte=reactiveVal(F)
  map_coord=reactiveVal(c(0,45))
  timer=reactiveVal(Sys.time())
  # no_archive=reactiveVal(T)
  millesimes=reactiveVal(NULL)
  autorefresh <- reactiveTimer(5000)
  new_modifs <- reactiveVal(0)
  # google_files <- reactiveVal(NULL)
  default_vals <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  current_mapped_data <- reactiveVal(
    data.table("agr"="","picked_zonage"="")
  )
  
  
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
  
  vals_reac=reactive({
    print("get vals_reac")
    my_agr=tableau_reg()[["agr"]]
    my_agr=as.character(my_agr)
    vals <- sapply(my_agr,function(i) input[[i]])
    
    
    vals=vals[sapply(vals,length)>0]
    req(length(vals)>0)
    vals=stack(vals)
    names(vals) <- c("picked_zonage","agr")
    vals <- vals %>% mutate_if(is.factor,as.character)
    print("get vals_reac OK")
    vals
  })

  source("utils/navigation.R",local=T,encoding = "UTF-8")
  
  output$nb_modif_unsaved = renderText({
    ifelse(new_modifs()==0,
           "Aucune modification non enregistrée",
           ifelse(new_modifs()==1, "Une modification non enregistrée",
                  paste0(new_modifs()," modifications non enregistrées")))
  })
  
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


}