usethis::use_gitlab_ci()

usethis::use_description(check_name = F)#Things to fill manually in the DRESCRIPTION file
usethis::use_mit_license(copyright_holder = "DREES")

pkgs = c("shiny","data.table","DT","leaflet","ggplot2","ggrepel","sp","sf","shinyalert"
         ,"dplyr","readxl","flexdashboard","jsonlite","curl","colourvalues","bsplus","rdrop2"
         ,"rgdal","rgeos","htmlwidgets","lubridate","openxlsx","knitr","kableExtra","flextable"
         ,"ggsn","shinydashboard","shinyWidgets","plotly","slackr","shinyjs","gmailr","purrr"
         ,"assertthat","shinytest","testthat")
pkgs %>% map(usethis::use_package) %>% invisible()

usethis::use_package("webshot")
usethis::use_package("docxtractr")#for download_map_zonage_arrete.R
usethis::use_package("textreadr")#for download_map_zonage_arrete.R
usethis::use_package("sas7bdat")
usethis::use_package("haven")
usethis::use_package("ggspatial")
