library(curl)
library(pbapply)
library(geojsonsf)
dep=fread("data/departement2019.csv")

communes=pblapply(dep$dep,function(my_dep){
  print(my_dep)
  URL=sprintf(
    "https://geo.api.gouv.fr/departements/%s/communes?fields=nom,code,population,contour&format=geojson&geometry=contour"
    ,my_dep)
  download.file(URL,destfile = paste0("data/geojson/",my_dep,".json"),mode = "wb")
})

communes=pblapply(dep$dep,function(my_dep){
  print(my_dep)
  info_com_sf=geojsonsf::geojson_sf(paste0("data/geojson/",my_dep,".json"))
  info_com_sf$nom=iconv(info_com_sf$nom,"UTF-8","latin1")
  info_com_sf$nom=iconv(info_com_sf$nom,"latin1","UTF-8")
  info_com_sf
})
