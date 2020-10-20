# devtools::install_github("rstudio/DT")
download.file(url = "http://ipinfo.io/ip",mode = "wb",
              destfile = "current_ip.txt")
server_ip=readLines("current_ip.txt")
if(server_ip %in% c("54.204.34.9","164.131.131.193","54.204.36.75","54.204.37.78",
                    "34.203.76.245","3.217.214.132","34.197.152.155")){
  options(encoding = 'UTF-8')
} else {
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
# library(googlesheets)
library(googledrive)
# library(googlesheets4)
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
#library(remotes)
#install_github('oswaldosantos/ggsn')
library(ggsn)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(slackr)
library(shinyjs)



# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
slackr_setup(config_file = "www/slackr_config.txt",echo = F)

rdrop2::drop_download(path = paste0("zonage/auth.txt"),overwrite = T,
                      dtoken = token,verbose = T,
                      local_path = "data")
options(gargle_oauth_cache = ".secrets")

drive_auth(email = "drees-zonage-ars@sante.gouv.fr",
           token=".secrets/a924da283165ded31d5fb98542b0974f_drees-zonage-ars@sante.gouv.fr")
# drive_find(type = "csv",q="name contains '^sf_' or name contains '^inf_' or name contains '^mg_' or name contains '^mk_'")

vars_to_toggle = c("agr","libagr","communes","population","is_majoritaire","CN","libCN")
vars_to_choose_from = list(mg = c("Code TVS"="agr","Nom TVS"="libagr",
                                  "Liste des départements"="departements",
                                  "Liste communes"="communes",
                                  "Population (communes de la région)"="population",
                                  # "APL 2017"="apl",
                                  "Ma région est majoritaire"="is_majoritaire",
                                  "Hors Vivier (HV)"="HV","Zone de Vigilance (ZV)"="ZV","ZAC","ZIP","Cadre National (CN)"="CN",
                                  "Zone d'échange"="degre_liberte"),
                           sf = c("Code BCVC"="agr","Nom BVCV"="libagr",
                                  "Liste des départements"="departements",
                                  "Liste communes"="communes",
                                  "Population (BVCV total)"="population",
                                  "APL 2017"="apl",
                                  "Ma région est majoritaire"="is_majoritaire",
                                  "VUD","UD","Int","VD","OD",
                                  "Zone d'échange intermédiaire/sous-dotée"="ZE_UD",
                                  "Zone d'échange très/sur-dotée"="ZE_OD","Cadre National (CN)"="libCN",
                                  "Zone d'échange"="degre_liberte","Code région majoritaire"="reg_majoritaire"),
                           inf = c("Code BCVC"="agr","Nom BVCV"="libagr",
                                   "Liste des départements"="departements",
                                   "Liste communes"="communes",
                                   "Population (BVCV total)"="population",
                                   "APL 2017"="apl",
                                   "Ma région est majoritaire"="is_majoritaire",
                                   "VUD","UD","Int","VD","OD",
                                   "Zone d'échange intermédiaire/sous-dotée"="ZE_UD",
                                   "Zone d'échange très/sur-dotée"="ZE_OD","Cadre National (CN)"="libCN",
                                   "Zone d'échange"="degre_liberte","Code région majoritaire"="reg_majoritaire"))
vars_to_show_list = list(mg = c("agr","libagr","communes","HV","ZV","ZAC","ZIP","CN","population"),
                         sf = c("agr","libagr","communes","VUD","UD","Int","VD","OD","libCN","population","apl","reg_majoritaire"),
                         inf = c("agr","libagr","communes","VUD","UD","Int","VD","OD","libCN","population","apl","reg_majoritaire"))


# com to agr
# drop_delete(dtoken = token,path = "zonage/bvcv2019.sas7bdat")
# drop_upload(dtoken=token,file = "data/tvs2019.sas7bdat",path = "zonage",mode = "overwrite",autorename = F)
if(!"tvs2019.sas7bdat"%in%list.files("data")){
  rdrop2::drop_download(path = "zonage/tvs2019.sas7bdat",overwrite = T,
                        dtoken = token,verbose = T,
                        local_path = "data")
}
TVS = haven::read_sas("data/tvs2019.sas7bdat")
names(TVS) <- c("depcom","libcom","agr","libagr","reg","dep","libdep","libreg")
TVS=data.table(TVS)
# TVS$agr = stringi::stri_pad_left(TVS$agr,5,"0")
TVS$agr = stringi::stri_pad_right(TVS$agr,5,"_")
table(nchar(TVS$agr))

# QPV
if(!"qpv_markers_pop.RData"%in%list.files("data")){
  rdrop2::drop_download(path = "zonage/qpv_markers_pop.RData",overwrite = T,
                        dtoken = token,verbose = T,
                        local_path = "data")
}
load("data/qpv_markers_pop.RData")




# drop_upload(dtoken=token,file = "data/bvcv2019.sas7bdat",path = "zonage",mode = "overwrite",autorename = F)
if(!"bvcv2019.sas7bdat"%in%list.files("data")){
  rdrop2::drop_download(path = "zonage/bvcv2019.sas7bdat",overwrite = T,
                        dtoken = token,verbose = T,
                        local_path = "data")
}


BVCV = haven::read_sas("data/bvcv2019.sas7bdat") %>% 
  select(-type_zone,-taille_pole,-bv2012,-libbv,-cv,-libcv)
BVCV=data.table(BVCV)
setnames(BVCV,c('bvcv','libbvcv'),c('agr','libagr'))
BVCV$agr = stringi::stri_pad_right(BVCV$agr,5,"_")
table(nchar(BVCV$agr))

# drop_upload(dtoken=token,file = "data/Zonage_medecin_20191231.xlsx",
# path = "zonage",mode = "overwrite",autorename = F)

if(!"Zonage_medecin_20191231.xlsx"%in%list.files("data")){
  rdrop2::drop_download(path = "zonage/Zonage_medecin_20191231.xlsx",overwrite = T,
                        dtoken = token,verbose = T,
                        local_path = "data")
}

hist_qpv <- readxl::read_excel("data/Zonage_medecin_20191231.xlsx",sheet = "Zonage_QPV")[,c(1,3,5,6,10,12)]
hist_qpv = data.table(hist_qpv)
names(hist_qpv) <- c("reg","agr","cod","libqpv","zonage_ars","pop")
hist_qpv[zonage_ars=="Zone de vigilance",zonage_ars:="ZV"]
hist_qpv[zonage_ars=="Hors vivier",zonage_ars:="HV"]
hist_qpv=hist_qpv%>%mutate_if(is.factor,as.character)%>%data.table%>%unique
hist_qpv$agr = stringi::stri_pad_right(hist_qpv$agr,5,"_")

# source("utils/handle_insee_pop.R")
if(!"pop_femme2016.RData"%in%list.files("data"))
  drop_download("zonage/pop_femme2016.RData",local_path = "data/",overwrite = T,dtoken = token,verbose = T)
load("data/pop_femme2016.RData")

dep = unique(TVS[,c("dep","reg","libdep")])

# geo reg
if(!"contours_dep.RData"%in%list.files("data"))
  drop_download("zonage/contours_dep.RData",local_path = "data/",overwrite = T,dtoken = token,verbose = T)

# rdrop2::drop_upload(file="data/reg_cont.RData",dtoken=token,path="zonage/",autorename = F)
if(!"reg_cont.RData"%in%list.files("data"))
  drop_download("zonage/reg_cont.RData",local_path = "data/",overwrite = T,dtoken = token,verbose = T)

load("data/contours_dep.RData")
load("data/reg_cont.RData")
names(reg_cont)[which(names(reg_cont)=='code_insee')]<-'reg'
names(reg_cont)[which(names(reg_cont)=='nom')]<-'libreg'
# nm reg
regions = unique(TVS[,c("reg","libreg")])

# rdrop2::drop_upload(file="data/Seuils_arretes.xlsx",dtoken=token,path="zonage/",autorename = F)
if(!"Seuils_arretes.xlsx"%in%list.files("data"))
  drop_download("zonage/Seuils_arretes.xlsx",local_path = "data/",overwrite = T,dtoken = token,verbose = T)
seuils_reg_mg=read_xlsx("data/Seuils_arretes.xlsx",sheet="mg")
seuils_reg_sf=read_xlsx("data/Seuils_arretes.xlsx",sheet="sf")%>%rename(check_sf=check)
seuils_reg_inf=read_xlsx("data/Seuils_arretes.xlsx",sheet="inf")%>%rename(check_inf=check)
regions=merge(regions,
              seuils_reg_mg %>% select(-libreg),
              by="reg")
regions=merge(regions,
              seuils_reg_sf %>% select(-libreg),
              by="reg")
regions=merge(regions,
              seuils_reg_inf %>% select(-libreg),
              by="reg")




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

# source("utils/prep_liste_TribunauxAdministratifs.R",echo = T)
# get_TA()
# rdrop2::drop_upload(file="data/liste_tribunaux_administratifs.RData",dtoken=token,path="zonage/",autorename = F)
if(!"liste_tribunaux_administratifs.RData"%in%list.files("data"))
  drop_download("zonage/liste_tribunaux_administratifs.RData",local_path = "data/",overwrite = T,dtoken = token,verbose = T)

load("data/liste_tribunaux_administratifs.RData")

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
