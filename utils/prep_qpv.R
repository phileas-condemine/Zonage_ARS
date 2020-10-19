#https://sig.ville.gouv.fr/atlas/QP
library(sp)
library(sf)
library(rgdal)
library(leaflet)
# https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
library(lwgeom)
library(data.table)
library(rdrop2)
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

mom = read_sf("data/qpv/QP_METROPOLEOUTREMER_WGS84_EPSG4326.shp")

m <- leaflet()%>%
  addTiles()%>%
  addPolygons(data=mom,popup = ~ paste0("POLY<br>code QPV : ",CODE_QP,"<br>nom QPV : ",NOM_QP," <br> commune : ", COMMUNE_QP))
poly=mom
st_centroid_within_poly <- function (poly) {
  # check if centroid is in polygon
  ctrd <- st_centroid(poly, of_largest_polygon = TRUE)
  # matching = sp::over(as(st_centroid(ctrd),'Spatial'),as(st_centroid(poly),'Spatial'))
  matching = sp::over(as(ctrd,'Spatial'),as(poly,'Spatial'))
  in_poly = matching$CODE_QP==poly$CODE_QP
  in_poly[is.na(in_poly)]=F
  
  # in_poly <- diag(st_within(ctrd, poly, sparse = F))
  
  # replace geometries that are not within polygon with st_point_on_surface()
  st_geometry(ctrd[!in_poly,]) <- st_geometry(st_point_on_surface(poly[!in_poly,]))
  
  ctrd
}

mom_markers = st_centroid_within_poly(mom)

m <- m %>% addMarkers(data=mom_markers,popup = ~ paste0("MARKER<br>code QPV : ",CODE_QP,"<br>nom QPV : ",NOM_QP," <br> commune : ", COMMUNE_QP))

demo = readxl::read_excel("data/qpv/DEMO_2017_V7.xls","QP",skip = 5)

# afficher NOTE_CNAF sur la carte et utiliser POP_MUN
# 2017 latest ? plus récent ? seulement metro & reu ?

mom_markers = merge(mom_markers,demo[,c("CODGEO","POP_MUN")],by.x="CODE_QP",by.y="CODGEO",all.x=T)
check_missing_pop = data.table(mom_markers)[,.(nb=.N,miss=sum(is.na(POP_MUN))),by=substr(CODE_QP,3,5)]

mom_markers = mom_markers[!is.na(mom_markers$POP_MUN),]

pal = colorNumeric("Blues",domain=log(mom_markers$POP_MUN))
m2 <- leaflet()%>%addTiles()%>%addCircleMarkers(data=mom_markers,
                                          popup = ~ paste0("MARKER<br>code QPV : ",CODE_QP,
                                                           "<br>nom QPV : ",NOM_QP,
                                                           "<br>commune : ", COMMUNE_QP,
                                                           "<br>pop : ",POP_MUN),
                                          fillColor = ~pal(log(POP_MUN)),color=~pal(log(POP_MUN)))


save(mom_markers,file = "data/qpv_markers_pop.RData")
drop_upload("data/qpv_markers_pop.RData",dtoken = token,path = "zonage",mode = "overwrite",autorename = F)
