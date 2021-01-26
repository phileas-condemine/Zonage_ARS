# devtools::install_github("rstudio/DT")
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
}

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
library(slackr)
library(shinyjs)
library(gmailr)
gm_auth_configure(path = "credentials.json")
gm_auth(email = "drees.zonage.ars@gmail.com",cache = ".secrets/") # ça marche ! il fallait suivre les instructions de récup des credentials pour Python "quickstart.py"


# Then pass the token to each drop_ function
drop_auth(rdstoken = "droptoken.rds")

drop_clean_upload = function(filename, local_path = "data/",drop_path = "zonage/",message=NULL){
  local_name = paste0(local_path,filename)
  drop_name = paste0(drop_path,filename)
  # if(rdrop2::drop_exists(drop_name)){
  #   if(!is.null(message)){
  #     print(message)
  #   }
  #   rdrop2::drop_delete(path = drop_name)
  # }
  rdrop2::drop_upload(file = local_name,path = drop_path,autorename = F,mode="overwrite")
}

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




slack_log = function(filename,my_reg,my_ps,my_mil,session){
  filename = filename
  reg = ifelse(is.null(my_reg),"XX",my_reg)
  ps = ifelse(is.null(my_ps),"XX",my_ps)
  mil = ifelse(is.null(my_mil),"XX",my_mil)
  if(session$clientData$url_pathname=="/Zonage_ARS/"){
    message=sprintf("App:ZonageARS\nEvent: Téléchargement du fichier %s par la région %s pour la profession %s avec le projet %s",filename,reg,ps,mil)
    slackr_setup(config_file = "www/slackr_config_log.txt",echo = F)
    slackr_bot(message)
  }
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

