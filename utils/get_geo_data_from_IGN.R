source("global.R")
# my_reg = regions$reg[1]
drop_auth(rdstoken = "droptoken.rds")

# remotes::install_github("InseeFrLab/inseeLocalData")
# library(inseeLocalData)
# library(curl)
library(rgdal)
library(sf)
library(tidyr)
library(magrittr)
library(leaflet)

##### RECUP DES FONDS DE CARTE #####

# https://professionnels.ign.fr/contoursiris
ftp2file = "ftp://Contours_IRIS_ext:ao6Phu5ohJ4jaeji@ftp3.ign.fr/CONTOURS-IRIS_2-1__SHP__FRA_2021-01-01.7z"
# téléchargement depuis le site en passant par IE (le proxy ministère bloque download.file)
# dézippage via 7zip à la main => data/IGN_CONTOURS_IRIS
code_crs = data.table(CRS = c("LAMBERT_93","UTM_N20","UTM_N21","UTM_N22","UTM_S38","UTM_S40"),
                      EPSG = c(2154,4559,4467,2972,4471,2975))

FRA = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
FRA@data = data.frame(INSEE_COM = FRA@data$INSEE_COM)
FRA = FRA %>% st_as_sf(coords=c("coordxet","coordyet"),crs=2154) %>% st_transform(4326)

GLP = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_RGAF09UTM20_GLP-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
GLP@data = data.frame(INSEE_COM = GLP@data$INSEE_COM)
GLP = GLP %>% st_as_sf(coords=c("coordxet","coordyet"),crs=4559) %>% st_transform(4326)
leaflet(GLP)%>%addTiles()%>%addPolygons()

MTQ = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_RGAF09UTM20_MTQ-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
MTQ@data = data.frame(INSEE_COM = MTQ@data$INSEE_COM)
MTQ = MTQ %>% st_as_sf(coords=c("coordxet","coordyet"),crs=4559) %>% st_transform(4326)
leaflet(MTQ)%>%addTiles()%>%addPolygons()

GUF = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_UTM22RGFG95_GUF-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
GUF@data = data.frame(INSEE_COM = GUF@data$INSEE_COM)
GUF = GUF %>% st_as_sf(coords=c("coordxet","coordyet"),crs=2972) %>% st_transform(4326)
leaflet(GUF)%>%addTiles()%>%addPolygons()

REU = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_RGR92UTM40S_REU-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
REU@data = data.frame(INSEE_COM = REU@data$INSEE_COM)
REU = REU %>% st_as_sf(coords=c("coordxet","coordyet"),crs=2975) %>% st_transform(4326)
leaflet(REU)%>%addTiles()%>%addPolygons()

MYT = readOGR("data/IGN_CONTOURS_IRIS/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_RGM04UTM38S_MYT-2021/CONTOURS-IRIS.shp",encoding = "UTF-8")
MYT@data = data.frame(INSEE_COM = MYT@data$INSEE_COM)
MYT = MYT %>% st_as_sf(coords=c("coordxet","coordyet"),crs=4471) %>% st_transform(4326)
leaflet(MYT)%>%addTiles()%>%addPolygons()

FRANCE = rbind(FRA, GLP, MTQ, GUF, REU, MYT)

##### POPULATION #####
# https://www.insee.fr/fr/statistiques/4989724?sommaire=4989761 
# télécharger le format csv, 
# c'est un fichier compressé (zip) contenant un CSV 
# récupérer le fichier Communes.csv et le mettre dans data/
# Pour 2021 on utilise la population légale 2018 et on renomme le fichier pop_insee_legales2018_noMYT.csv parce qu'il n'y a pas Mayotte
pop_fra = fread("data/pop_insee_legales2018_noMYT.csv",encoding = "UTF-8",colClasses = "character")
path = "zonage_dev/"
rdrop2::drop_upload(file="data/pop_insee_legales2018_noMYT.csv",path=path,autorename = F)

# on a juste besoin de 3 variables pour reproduire ce qu'on récupérait avec l'API GEO: 
  # code , population & nom
pop_fra[,code:=paste0(substr(CODDEP,1,2),CODCOM)]
pop_fra = pop_fra[,.(code,population=PTOT,nom=COM)]
communes=aggregate(x = FRANCE,
                   by = list("code"=FRANCE$INSEE_COM),
                   FUN = function(x)x[1])
communes$INSEE_COM=NULL
communes = merge(communes,pop_fra,by="code",all.x=T)

saveRDS(communes,"data/IGN_CONTOURS_IRIS/FRANCE_FULL_WGS84.RDS")
path = "zonage_dev/"
rdrop2::drop_upload(file="data/IGN_CONTOURS_IRIS/FRANCE_FULL_WGS84.RDS",path=path,autorename = F)

