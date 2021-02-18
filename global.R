# devtools::install_github("rstudio/DT")
try({
  download.file(url = "https://api64.ipify.org/",mode = "wb",
                destfile = "current_ip.txt")
  
  server_ip=readLines("current_ip.txt")
  
  
  if(server_ip %in% c("54.204.34.9","164.131.131.193","54.204.36.75","54.204.37.78",
                      "34.203.76.245","3.217.214.132","34.197.152.155")){
    options(encoding = 'UTF-8')
  } else {
    # options(shiny.fullstacktrace = TRUE)
    # options(shiny.trace = TRUE)
    options(shiny.error = browser)
    # options(shiny.error = recover)
  }
  
})

library(shiny)
library(data.table)
library(DT)
library(leaflet)
library(ggplot2)
library(ggrepel)
library(sp)
library(sf)
library(shinyalert)
library(dplyr)
library(readxl)
library(flexdashboard)
library(jsonlite)
library(curl)
library(colourvalues)
library(bsplus)
library(rdrop2)
library(rgdal)
library(rgeos)
library(htmlwidgets)
library(lubridate)
library(openxlsx)
library(knitr)
library(kableExtra)
library(flextable)
library(ggsn)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
# devtools::install_github("mrkaye97/slackr")
library(slackr)
library(shinyjs)
library(gmailr,exclude = "message")
library(purrr)
library(assertthat)
gm_auth_configure(path = "credentials.json")
gm_auth(email = "drees.zonage.ars@gmail.com",cache = ".secrets/") # ça marche ! il fallait suivre les instructions de récup des credentials pour Python "quickstart.py"


# Then pass the token to each drop_ function
drop_auth(rdstoken = "droptoken.rds")
### LOAD FUNCTIONS ###
source("R/misc.R",encoding = "UTF-8")
source("R/load_files.R",encoding = "UTF-8")
source("R/plots_func.R",encoding = "UTF-8")
source("R/maps_func.R",encoding = "UTF-8")
source("R/com_func.R",encoding = "UTF-8")
source("R/ui_func.R",encoding = "UTF-8")
source("R/handle_geo_data.R",encoding = "UTF-8")
source("R/tableau_agr_func.R",encoding = "UTF-8")  
source("R/table_recap.R",encoding = "UTF-8")  
source("R/get_zonage_en_vigueur.R",encoding = "UTF-8")
source("R/get_qpv_zonage_en_vigueur.R",encoding = "UTF-8")
source("R/prep_zonage_mg.R",encoding = "UTF-8")
source("R/prep_zonage_hors_mg.R",encoding = "UTF-8")
source("R/export_dl_func.R",encoding = "UTF-8")
source("R/persistance_func.R",encoding = "UTF-8")
source("R/jauges_func.R",encoding = "UTF-8")
source("R/qpv_func.R",encoding = "UTF-8")
source("R/justification_func.R",encoding = "UTF-8")
source("R/auth_func.R",encoding = "UTF-8")

vars_to_choose_from = list(mg = c("Code TVS"="agr","Nom TVS"="libagr",
                                  "Liste des départements"="departements",
                                  "Liste communes"="communes",
                                  "Hors Vivier (HV)"="HV",
                                  "Zone de Vigilance (ZV)"="ZV","ZAC","ZIP",
                                  "Population (communes de la région)"="population",
                                  # "APL 2017"="apl",
                                  "Ma région est majoritaire"="is_majoritaire",
                                  "Cadre National (CN)"="CN",
                                  "Zone d'échange"="degre_liberte"),
                           sf = c("Code BCVC"="agr","Nom BVCV"="libagr",
                                  "Liste des départements"="departements",
                                  "Liste communes"="communes",
                                  "VUD","UD","Int","VD","OD",
                                  "Population (BVCV total)"="population",
                                  "APL 2017"="apl",
                                  "Ma région est majoritaire"="is_majoritaire",
                                  "Zone d'échange intermédiaire/sous-dotée"="ZE_UD",
                                  "Zone d'échange très/sur-dotée"="ZE_OD","Cadre National (CN)"="libCN",
                                  "Zone d'échange"="degre_liberte","Code région majoritaire"="reg_majoritaire"),
                           inf = c("Code BCVC"="agr","Nom BVCV"="libagr",
                                   "Liste des départements"="departements",
                                   "Liste communes"="communes",
                                   "VUD","UD","Int","VD","OD",
                                   "Population (BVCV total)"="population",
                                   "APL 2017"="apl",
                                   "Ma région est majoritaire"="is_majoritaire",
                                   "Zone d'échange intermédiaire/sous-dotée"="ZE_UD",
                                   "Zone d'échange très/sur-dotée"="ZE_OD","Cadre National (CN)"="libCN",
                                   "Zone d'échange"="degre_liberte","Code région majoritaire"="reg_majoritaire"))
vars_to_show_list = list(mg = c("agr","libagr","communes","HV","ZV","ZAC","ZIP","CN","population"),
                         sf = c("agr","libagr","communes","VUD","UD","Int","VD","OD","libCN","population","apl","reg_majoritaire"),
                         inf = c("agr","libagr","communes","VUD","UD","Int","VD","OD","libCN","population","apl","reg_majoritaire"))


regions_derogatoires = c("84","11","93","32")

shiny_running = function () {
  # Look for `runApp` call somewhere in the call stack.
  frames = sys.frames()
  calls = lapply(sys.calls(), `[[`, 1)
  call_name = function (call)
    if (is.function(call)) '<closure>' else deparse(call)
  call_names = vapply(calls, call_name, character(1))
  target_call = grep('^runApp$', call_names)
  if (length(target_call) == 0)
    return(FALSE)
  # Found a function called `runApp`, verify that it’s Shiny’s.
  target_frame = frames[[target_call]]
  namespace_frame = parent.env(target_frame)
  isNamespace(namespace_frame) && environmentName(namespace_frame) == 'shiny'
}



mois_noms = c("janvier","février","mars","avril","mai","juin",
              "juillet","août","septembre","octobre","novembre","décembre")

list_PS = c("Médecins"="mg","Sages-femmes"="sf",
            # "Masseurs-kinésithérapeutes"="mk",
            "Infirmiers"="inf")


vswitch_zonage_mg = function(v){
  sapply(v,function(x)switch(x,
                             ZIP = 4,
                             ZAC = 3,
                             ZV = 2,
                             HV = 1))
}



correspondants_CNAM = c(
  "EMIN.AGAMALIYEV@assurance-maladie.fr",
  "anne.du-castel@assurance-maladie.fr",
  "AQUILINO.FRANCISCO@assurance-maladie.fr",
  "claire.traon@assurance-maladie.fr",
  "agnes.guerin@assurance-maladie.fr",
  "christelle.capdepon@assurance-maladie.fr",
  "laurent.plard@assurance-maladie.fr")


correspondants_DGOS = c(
  "clemence.lamoril@sante.gouv.fr",
  "carole.merle@sante.gouv.fr"
)

correspondants_dev_drees = c(
  "phileas.condemine@sante.gouv.fr",
  "blandine.legendre@sante.gouv.fr"
)











