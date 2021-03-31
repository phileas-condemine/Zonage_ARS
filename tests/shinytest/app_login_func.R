
login_app = function(path_to_app,my_reg,my_ps,auth){
  message("func : login_app")
  
  #### Start the app ####
  print("Init the Shiny Driver...")
  app <- ShinyDriver$new(path_to_app,loadTimeout = 20E+3,phantomTimeout = 20E+3)
  print("...Done !")
  
  
  print("move to params")
  # app$getAllValues()$input$choix_reg
  app$setInputs(go_params = "click")
  curr_choix_ps = app$getValue("choix_ps")
  if(!"curr_choix_ps"%in%ls()){
    curr_choix_ps = app$waitForValue("choix_ps",ignore = list())
  }
  print(sprintf("Valeur choix_ps actuelle : %s",curr_choix_ps))
  
  curr_choix_reg = app$getValue("choix_reg")
  if(!"curr_choix_reg"%in%ls()){
    curr_choix_reg = app$waitForValue("choix_reg",ignore = list())
  }
  print(sprintf("Valeur choix_reg actuelle : %s",curr_choix_reg))
  
  inputs = app$getAllValues()$input
  print("choix_ps & choix_reg inputs should exist")
  expect_true(all(c("choix_ps","choix_reg")%in%names(inputs)),"choix_ps & choix_reg should exist as input (but NULL, see next test)")
  print("No region should be picked yet")
  expect_null(inputs$choix_reg,label = "No region should be picked for now, but ")
  print("No PS should be picked yet")
  expect_null(inputs$choix_ps,label = "No ps should be picked for now")
  
  #### Reg & PS ####
  
  print(sprintf("pick reg : %s",my_reg))
  app$setInputs(choix_reg = my_reg)
  expect_identical(app$getAllValues()$input$choix_reg,my_reg,label = "Region n° should be as chosen")
  print("Done!")
  
  print(sprintf("pick ps : %s",my_ps))
  app$setInputs(choix_ps = my_ps)
  print("curr value for choix_ps")
  print(app$getAllValues()$input$choix_ps)
  expect_identical(app$getAllValues()$input$choix_ps,my_ps,label = "PS should be as chosen")
  print("Done!")
  
  #### Millesime ####
  
  start_to_wait = Sys.time()
  timeout = 20
  timed_out = F
  while(!"choix_millesime" %in% names(app$getAllValues()$input) & !timed_out){
    Sys.sleep(0.2)
    if(difftime(Sys.time(),start_to_wait,units = "secs")>timeout){
      print("timed out !")
      timed_out = T
    }
  }
  print(sprintf("waited for %s secs before the millesime input appeared !",difftime(Sys.time(),start_to_wait,units = "secs")))
  
  expect_true("choix_millesime" %in% names(app$getAllValues()$input),
              label = "choix_millesime input should exist")
  print(names(app$getAllValues()$input))
  expect_true("modal_save_current" %in% names(app$getAllValues()$input),
              label = "there should be a button modal_save_current to create a new millesime")
  
  millesime = app$getAllValues()$input$choix_millesime
  
  
  names(app$getAllValues()$input)
  if (millesime == ""){
    print("create a new projet as millesime is empty")
    app$setInputs(modal_save_current = "click")
    expect_true("millesime_name" %in% names(app$getAllValues()$input),
                label = "there should be an input millesime_name to set the name of the new millesime")
    
    nm_default = "test_auto"
    print(sprintf("set name to %s",nm_default))
    app$setInputs(millesime_name = nm_default)
    expect_identical(app$getAllValues()$input$millesime_name,nm_default,label="millesime name should be as chosen")
    
    print("validate the new millesime name")
    app$setInputs(save_current_view = "click")
    my_millesime = paste0(my_ps,"_",my_reg,"_test_auto")
  } else {
    print(sprintf("reusing existing project : %s",millesime))
    my_millesime = millesime
  }
  
  millesime = app$getAllValues()$input$choix_millesime
  # print(millesime)
  
  expect_equal(millesime,my_millesime,label = "the millesime is not the one automatically created - the test is only consistent when a new project was created")
  
  #### Main interface ####
  
  print("Moving to main interface")
  expect_true("go_zonage" %in% names(app$getAllValues()$input),
              label = "go_zonage button should exist !")
  app$setInputs(go_zonage = "click")
  # app$setInputs(go_zonage = "click",wait_ = F,values_ = F)
  print("Clicked on go_zonage button - expecting the password modal to appear")
  # app$getValue("sidebarmenu")
  # app$getValue("go_zonage")
  # app$getValue("my_auth")
  # app$waitForValue("my_auth")
  # app$findElement("#my_auth")
  empty_pwd_field = app$waitForValue("my_auth", ignore = NULL,timeout = 20*1E+3)
  expect_true("my_auth" %in% names(app$getAllValues()$input),
              label = "There should be a field to enter the password")
  print("Entering password")
  
  app$setInputs(my_auth = key)
  app$setInputs(send_pwd = "click",wait_ = F,values_ = F)
  
  print("wait for the \"force save button\" to appear...")
  Sys.sleep(1)
  iter = 0
  force_save_value = app$getValue("force_save")
  if (!"force_save_value"%in%ls()){
    print("getValue didn't work => waitForValue")
  }
  while(!"force_save_value" %in% ls() && iter <10){
    force_save_value = app$waitForValue("force_save", ignore = NULL,timeout = 5*1E+3)
    iter = iter +1
  }
  print("... done !")
  
  # source_code = app$getSource()
  # page = read_html(source_code)
  # page = page %>% html_nodes("#shiny-tab-zonage")
  # page %>% html_text() %>% gsub(pattern = "\n",replacement = "")
  # page %>% html_node("table")
  # page %>% html_table()
  
  # e = app$findElement('a[href*="#shiny-tab-my_params"]')
  # e$getText()
  # e$getName()
  # e$click()
  # app$getValue("sidebarmenu")
  
  
  
  
  inputs = app$getAllValues(output = F,export = F)$input
  expect_true("force_save"%in%names(inputs))
  print("force_save %in% names(inputs) was checked - now check if its value is 0")
  
  expect_equal(as.integer(force_save_value),0,
               label = "Force save button input value should be 0 for now, but it's ")
  
  print("wait for Shiny")
  app$waitForShiny()
  print("page zonage is now loaded !")
  
  
  app
}