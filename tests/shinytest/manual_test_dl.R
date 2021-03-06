library(shinytest)
library(testthat)
library(rdrop2)
library(data.table)
# path_to_app = ifelse(interactive(),"./","../../")
# path_to_app="./"
path_to_app = "../../"
drop_auth(rdstoken = paste0(path_to_app,"droptoken.rds"))

print("Init the Shiny Driver...")
app <- ShinyDriver$new(path_to_app,loadTimeout = 20E+3,phantomTimeout = 20E+3)
print("...Done !")
source(paste0(path_to_app,"R/load_files.R"),encoding = "UTF-8")
get_auth("zonage_dev/",paste0(path_to_app,"data"))

auth <- fread(paste0(path_to_app,"data/auth.txt"))
key <- auth[grepl("phileas",name)]$key[1]
files = drop_dir(path = "zonage_dev",recursive = T)
files = data.table(files)
files = files[grepl("en_vigueur",name),.(name,path_lower)]
files[,ps:=stringr::str_extract(path_lower,"(/inf/)|(/sf/)|(/mg/)")]
files[,ps:=gsub("/","",ps)]
ps_has_en_vigueur = unique(files$ps)

context("DL zonage en vigueur in a Shiny App")


test_dl_ps=function(ps){
  if (ps %in% ps_has_en_vigueur){
    print("get Excel file")
    fnm = paste0(ps,"_en_vigueur.xlsx")
    print(fnm)
    app$snapshotDownload(paste0("dl_zonage_en_vigueur_",ps),filename = fnm)
    print("the downloaded file should be readable by readxl")
    dt = readxl::read_excel(paste0(path_to_app,"tests/shinytest/snapshot-current/",fnm))
    print("success !")
    expect_true(inherits(dt,"tbl"),label = "downloaded file should be read as a tibble")
    expect_gt(nrow(dt),0,label = "zonage en vigueur should be non-empty")
    
  } else {
    print("nothing to DL")
    expect_error(
      app$snapshotDownload(paste0("dl_zonage_en_vigueur_",ps)),
                           regexp = "Unable request data from server",
                           label = "the dlButton should trigger an error")
  }
}




test_that("DL en_vigueur is blocked",{
  app$setInputs(sidebarItemExpanded = "Documents")
  expect_error(app$snapshotDownload("dl_zonage_en_vigueur_inf"),"Unable request data from server")
  print("dl zonage en vigueur check")
})


test_that("DL en_vigueur can be unlocked w. password",{
  expect_true(dir.exists(paste0(path_to_app,"tests/shinytest/snapshot-current/")),
              label = "the folder snapshot-current should have been created manually")
  print("dir exists check")
  app$setInputs(send_pwd2 = "click")
  expect_equal(app$getAllValues()$input$my_auth2,"",label = "The key should be empty for now")
  print("empty key check")
  app$setInputs(my_auth2 = key)
  app$setInputs(send_pwd2 = "click")
  
  files = list.files(paste0(path_to_app,"tests/shinytest/snapshot-current/"),full.names = T)
  print("remove existing snapshots")
  unlink(files)

  test_dl_ps("inf")
  print("inf check")
  test_dl_ps("sf")
  print("sf check")
  test_dl_ps("mg")
  print("mg check")
})
test_that("reg_maj_tvs can be downloaded",{
  nm = "reg_maj_tvs.xlsx"
  app$snapshotDownload("dl_reg_maj_tvs",filename = nm)
  print("read the xlsx")
  dt = readxl::read_excel(paste0(path_to_app,"tests/shinytest/snapshot-current/",nm))
  print("done !")
  expect_true(inherits(dt,"tbl"),label="reg_maj_tvs should be read as a tbl by readxl")
  print("is tbl check")
  expect_gt(nrow(dt),0,label = "reg_maj_tvs should be a non-empty df")
  print("non empty check")
})

app$stop()
