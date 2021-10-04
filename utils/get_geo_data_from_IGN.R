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

saveRDS(FRANCE,"data/IGN_CONTOURS_IRIS/FRANCE_FULL_WGS84.RDS")
path = "zonage_dev/"
rdrop2::drop_upload(file="data/IGN_CONTOURS_IRIS/FRANCE_FULL_WGS84.RDS",path=path,autorename = F)

