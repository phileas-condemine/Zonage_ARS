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

context("login, download map, table & arrete")

test_that("login, download map, table & arrete",{
  print(sprintf("Launch test with REG %s (%s) & PS %s",reg_name,my_reg,my_ps))
  
  app <- login_app(path_to_app,my_reg,my_ps,auth)
  
  #### DOWNLOAD TABLE OF THE CURRENT ZONAGE ####
  print("download the table of zonage")
  print("wait for the DL button to appear")
  e = app$findElement("a#download_table")
  expect_true(length(e)>0,"there should be a button to DL the table of the current zonage")
  print("DL as snapshot")
  nm = "table_zonage_reg.xlsx"
  app$snapshotDownload("download_table",filename = nm)
  print("read the xlsx")
  dt = readxl::read_excel(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))
  print("done !")
  expect_is(dt,"tbl",label="reg_zonage should be read as a tbl by readxl")
  expect_gt(nrow(dt),0,label = "reg_zonage should be a non-empty df")
  
  
  
  #### DOWNLOAD THE MAP OF THE CURRENT ZONAGE ####
  print("download the map of zonage")
  print("wait for the DL button to appear")
  e = app$findElement("a#download_plot")
  expect_true(length(e)>0,"there should be a button to DL the map of the current zonage")
  print("DL as snapshot")
  nm = "map_zonage_reg.png"
  app$snapshotDownload("download_plot",filename = nm)
  print("map is downloaded, now read it !")
  map_from_png = png::readPNG(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))
  print("check ratio")
  map_dim = dim(map_from_png)
  ratio = 16/10.4#check the ggsave in the output$download_plot defined in boutons_export.R
  observed_ratio = map_dim[2]/map_dim[1]
  expect_equal(map_dim[3],3,label="L'image devrait être en RGB ie sur 3 couches")
  print("check the img is RGB ie 3 channels")
  expect_equal(observed_ratio,ratio,label = "Les proportions de l'image devraient être identiques à celles demandées dans ggsave")
  
  #### DOWNLOAD ARRETE ####
  
  print("download the arrete of zonage")
  print("wait for the generate_arrete button to appear")
  e = app$findElement("button#generate_arrete")
  expect_true(length(e)>0,"there should be a button to generate the arrete")
  print("click to open form to generate arrete")
  app$setInputs(generate_arrete="click")
  print("wait for one form (date) to appear")
  print(app$waitForValue("DATE_UNION_REG_PS"))
  print("look for the DL button")
  e = app$findElement("a#download_arrete")
  print("test DL button presence")
  expect_true(length(e)>0,"there should be a button to DL the arrete")
  print("by default, map & table will be added to the arrete")
  print(app$waitForValue("add_annexes"))
  app$setInputs(add_annexes="Cartes")
  expect_equal(app$waitForValue("add_annexes"),"Cartes",label="Now only Cartes should be added to the Doc")
  print("download the arrete")
  nm = "zonage_arrete.docx"
  app$snapshotDownload("download_arrete",filename = nm)
  print("read the docx with textreadr")
  arrete_parsed = textreadr::read_docx(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))
  expect_is(arrete_parsed,"character","The docx should have been parsed as a character vector")
  
  print("Maintenant on telecharge l'arrete avec table & map, ça devrait prendre du temps")
  print(app$waitForValue("add_annexes"))
  app$setInputs(add_annexes=c("Cartes","Tableaux"))
  expect_true(all(c("Cartes","Tableaux")%in%app$waitForValue("add_annexes")),label="Now both map & table should be created")
  print("download the arrete w map & table - could be long 1-2 mins")
  nm = "zonage_arrete_full.docx"
  start_time = Sys.time()
  app$snapshotDownload("download_arrete",filename = nm)
  print(sprintf("Done in %s secs !",round(difftime(Sys.time(),start_time,units = "secs"))))
  print("read the docx with textreadr - this could be long too !")
  start_time = Sys.time()
  # arrete_parsed = textreadr::read_docx(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))## TOO LONG !
  arrete_parsed = docxtractr::read_docx(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))
  print(sprintf("Done in %s secs !",difftime(Sys.time(),start_time,units = "secs")))
  # expect_is(arrete_parsed,"character","The docx should have been parsed as a character vector")
  expect_is(arrete_parsed,"docx","The docx should have been parsed correctly")
  
  app$stop()
})
