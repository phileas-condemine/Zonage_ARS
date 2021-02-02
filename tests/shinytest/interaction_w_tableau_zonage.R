library(shinytest)
library(testthat)
library(rdrop2)
library(data.table)
library(magrittr)
# path_to_app="./"
# path_to_tests = "tests/shinytest/"
path_to_app = "../../"
path_to_tests = "./"
drop_auth(rdstoken = paste0(path_to_app,"droptoken.rds"))
auth <- fread(paste0(path_to_app,"data/auth.txt"))
reg_excluded = c(1,2,6)
my_reg <- sample(auth[reg>0&!reg%in%reg_excluded]$reg,1) %>% as.character()
reg_name = auth[reg==my_reg & !grepl("phileas",name)]$name
key <- sample(auth[reg==my_reg]$key,1)
my_ps <- sample(c("mg","sf","inf"),1)

source(paste0(path_to_tests,"app_login_func.R"),local = T)

# files = drop_dir(path = "zonage_dev",recursive = T)
# files = data.table(files)
# files = files[!grepl("en_vigueur",name),.(name,path_lower)]
# files[,ps:=stringr::str_extract(path_lower,"(/inf/)|(/sf/)|(/mg/)")]
# files[,ps:=gsub("/","",ps)]
# files[,reg:=stringr::str_extract(path_lower,"(_[0-9]*_)")]
# files[,reg:=gsub("_","",reg)]
# files = files[ps==my_ps & reg == my_reg]
# files = files[!grepl("^qpv_",name)]


context("Log in the app & basic interaction with the table")



test_that("Log in the app & basic interaction with the table",{
  print(sprintf("Launch test with REG %s (%s) & PS %s",reg_name,my_reg,my_ps))
  
  app <- login_app(path_to_app,my_reg,my_ps,auth)
  
  #### Interaction with the table ####
  app$waitForShiny()
  
  agr_list_ready = F
  i = 1
  max_iter = 30
  while(!agr_list_ready & (i < max_iter)){
    
    print(sprintf("iter %s : retrieve list of inputs to check the list of AGR",i))
    inputs = app$getAllValues(output = F,export = F)$input
    nm = names(inputs)
    agr_list = nm[grepl("(^[0-9AB]{4}_$)|(^[0-9AB]{5}$)",nm)]
    
    if(length(agr_list)>0){
      agr_list_ready=T
    } else {
      Sys.sleep(0.5)
      i=i+1
    }
  }
  
  
  print(sprintf("Pour la région %s et la PS %s on dispose de %s zones (agr)",reg_name,my_ps,length(agr_list)))
  picked_one_ok = F
  i = 1
  len = length(agr_list)
  while(!picked_one_ok & i <= len){
    print(sprintf("tentative de tirage n° : %s",i))
    one_agr = sample(agr_list,1)
    agr_list = setdiff(agr_list,one_agr)#remove the item from the list for next iteration
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
    
    nb_disabled = sum(disabled)
    if(length(disabled)-nb_disabled >= 2){
      print(sprintf("agr %s has %s options including the current_value %s",one_agr,length(disabled)-nb_disabled,cur_val))
      picked_one_ok = T
    }
    i=i+1
  }
  if(i > len){
    stop(sprintf("No AGR could be picked for reg %s (%s) with PS %s, rerun to test another REG/PS",reg_name,my_reg,my_ps))
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
  app$waitForShiny()
  print(app$findElement("#nb_modif_unsaved")$getText())
  
  
  print("reload the app and check if modif was recorded using chosen the ps, reg & agr")
  
  app <- login_app(path_to_app,my_reg,my_ps,auth)
  
  app$waitForShiny()
  recorded_val = app$waitForValue(one_agr,timeout = 2E+4)
  print("check the recorded value is the one we just picked")
  expect_equal(new_val,recorded_val,label = "Clicked + saved zonage should have been recorded !")
  print(sprintf("Tests done for REG %s (%s) & PS %s with AGR %s",reg_name,my_reg,my_ps,one_agr))
  app$stop()
  
})



