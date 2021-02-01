library(shinytest)
library(testthat)
library(rdrop2)
library(data.table)
library(magrittr)
# path_to_app="./"
path_to_app = "../../"
drop_auth(rdstoken = paste0(path_to_app,"droptoken.rds"))
auth <- fread(paste0(path_to_app,"data/auth.txt"))
my_reg <- sample(auth[reg>0]$reg,1) %>% as.character()
key <- sample(auth[reg==my_reg]$key,1)
my_ps <- sample(c("mg","sf","inf"),1)

files = drop_dir(path = "zonage_dev",recursive = T)
files = data.table(files)
files = files[!grepl("en_vigueur",name),.(name,path_lower)]

files[,ps:=stringr::str_extract(path_lower,"(/inf/)|(/sf/)|(/mg/)")]
files[,ps:=gsub("/","",ps)]
files[,reg:=stringr::str_extract(path_lower,"(_[0-9]*_)")]
files[,reg:=gsub("_","",reg)]

files = files[ps==my_ps & reg == my_reg]
if(my_ps=="mg"){
  files = files[!grepl("^qpv_",name)]
}


context("Log in the app")



test_that("Login the app",{
  #### Start the app ####
  print("Init the Shiny Driver...")
  app <- ShinyDriver$new(path_to_app)
  print("...Done !")
  
  
  print("move to params")
  app$getAllValues()$input$choix_reg
  app$setInputs(go_params = "click")
  
  expect_true(all(c("choix_ps","choix_reg")%in%names(app$getAllValues()$input)),"choix_ps & choix_reg should exist as input (but NULL, see next test)")
  expect_null(app$getAllValues()$input$choix_reg,label = "No region should be picked for now")
  expect_null(app$getAllValues()$input$choix_ps,label = "No ps should be picked for now")
  
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
  print("Clicked on go_zonage button - expecting the password modal to appear")
  
  empty_pwd_field = app$waitForValue("my_auth", ignore = NULL,timeout = 20*1E+3)
  expect_true("my_auth" %in% names(app$getAllValues()$input),
              label = "There should be a field to enter the password")
  print("Entering password")
  
  app$setInputs(my_auth = key)
  app$setInputs(send_pwd = "click",wait_ = F,values_ = F)
  
  print("wait for the \"force save button\" to appear...")
  force_save_value = app$waitForValue("force_save", ignore = NULL,timeout = 20*1E+3)
  print("... done !")

  inputs = app$getAllValues(output = F,export = F)$input
  expect_true("force_save"%in%names(inputs))
  print("force_save %in% names(inputs) was checked - now check if its value is 0")
  
  expect_equal(as.integer(force_save_value),0,
              label = "Force save button input value should be 0 for now, but it's ")

  print("wait for Shiny")
  app$waitForShiny()
  print("page zonage is now loaded !")
  
  #### Interaction with the table ####

  print("retrieve list of inputs to check the list of AGR")
  inputs = app$getAllValues(output = F,export = F)$input
  
  nm = names(inputs)
  agr_list = nm[grepl("(^[0-9]{4}_$)|(^[0-9]{5}$)",nm)]
  picked_one_ok = F
  i = 1
  while(!picked_one_ok){
    print(sprintf("tentative de tirage n° : %s",i))
    one_agr = sample(agr_list,1)
    inputs[setdiff(names(inputs),agr_list)]
    cur_val = inputs[[one_agr]]
    print(sprintf("get radio buttons for agr %s",one_agr))
    elems = app$findElements(sprintf(".zonage_radio_button[name=\"%s\"]",one_agr))
    disabled = sapply(elems,function(e){
      att = e$getAttribute("disabled")
      if(is.null(att)){
        FALSE
      } else {
        TRUE
      }
    })
    
    nb_disabled = sum(disabled=="true")
    if(length(disabled)-nb_disabled >= 2){
      picked_one_ok = T
    }
    i=i+1
  }
  
  all_options = sapply(elems,function(e)e$getValue())
  names(elems) <- all_options
  
  sapply(elems,function(e)e$getAttribute("checked"))
  alt_val = setdiff(all_options[!disabled],cur_val)
  new_val = sample(alt_val,1)
  print(sprintf("change zonage from %s to %s for agr %s",cur_val,new_val,one_agr))
  e = elems[[new_val]]
  e$click()
  expect_equal(new_val,app$getAllValues()$input[[one_agr]],label = "The clicked value should be set now")
  
  print("save the modif")
  print(app$findElement("#nb_modif_unsaved")$getText())
  app$setInputs(force_save = "click",timeout_=10000)
  print(app$findElement("#nb_modif_unsaved")$getText())
  
  print("reload the app and check if modif was recorded using chosen the ps, reg & agr")
  
  app <- ShinyDriver$new(path_to_app)
  app$setInputs(go_params = "click")
  app$setInputs(choix_reg = my_reg)
  app$setInputs(choix_ps = my_ps)
  app$setInputs(choix_millesime = my_millesime)
  app$setInputs(go_zonage = "click")
  app$setInputs(my_auth = key)
  app$setInputs(send_pwd = "click",wait_ = F,values_ = F)
  recorded_val = app$waitForValue(one_agr,timeout = 10000,iotype = "input")
  expect_equal(new_val,recorded_val,label = "Clicked + saved zonage should have been recorded !")
  
  app$stop()
  
})



