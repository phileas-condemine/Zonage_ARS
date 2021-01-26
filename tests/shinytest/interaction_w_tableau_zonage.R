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
if(ps=="mg"){
  files = files[!grepl("^qpv_",name)]
}


context("Log in the app")

print("Init the Shiny Driver...")
app <- ShinyDriver$new(path_to_app)
print("...Done !")

test_that("Setting params before login",{
  print("move to params")
  app$getAllValues()$input$choix_reg
  app$setInputs(go_params = "click")
  
  expect_true(all(c("choix_ps","choix_reg")%in%names(app$getAllValues()$input)),"choix_ps & choix_reg should exist as input (but NULL, see next test)")
  expect_null(app$getAllValues()$input$choix_reg,label = "No region should be picked for now")
  expect_null(app$getAllValues()$input$choix_ps,label = "No ps should be picked for now")
  
  
  print(sprintf("pick reg : %s",my_reg))
  app$setInputs(choix_reg = my_reg)
  
  expect_identical(app$getAllValues()$input$choix_reg,my_reg,label = "Region n° should be as chosen")
  
  
  print(sprintf("pick ps : %s",my_ps))
  app$setInputs(choix_ps = my_ps)
  print("curr value for choix_ps")
  print(app$getAllValues()$input$choix_ps)
  expect_identical(app$getAllValues()$input$choix_ps,my_ps,label = "PS should be as chosen")
})

testthat::test_that("choose millesime or create one",{
  expect_true("choix_millesime" %in% names(app$getAllValues()$input),
              label = "choix_millesime input should exist")
  expect_true("modal_save_current" %in% names(app$getAllValues()$input),
              label = "there should be a button modal_save_current to create a new millesime")
  
  millesime = app$getAllValues()$input$choix_millesime
  modal_save_current
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
  } 
  
  millesime = app$getAllValues()$input$choix_millesime
  expect_true("go_zonage" %in% names(app$getAllValues()$input),
              label = "go_zonage button should exist !")
  app$setInputs(go_zonage = "click")
  
  expect_true("my_auth" %in% names(app$getAllValues()$input),
              label = "There should be a field to enter the password")
  
  app$setInputs(my_auth = key)
  app$setInputs(send_pwd = "click")
  
  print("wait for the \"force save button\" to appear...")
  app$waitForValue("force_save", ignore = NULL)
  print("... done !")
  
  expect_equal(as.integer(app$getAllValues()$input$force_save),0,
              label = "Force save button input value should be 0 for now, but it's ")
  
  print("page zonage is now loaded !")
})

# test_that("Interact with the zonage table",{
#   
# })



app$stop()