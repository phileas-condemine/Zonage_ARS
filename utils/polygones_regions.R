download.file("http://osm13.openstreetmap.fr/~cquest/openfla/export/regions-20180101-shp.zip",
              destfile = "data/regions-20180101-shp.zip")
unzip("data/regions-20180101-shp.zip",exdir = "data/regions_contours")

reg_cont=sf::read_sf("data/regions_contours/regions-20180101.shp")
reg_cont=sf::st_simplify(preserveTopology = T,dTolerance = .01,x = reg_cont)
library(leaflet)
leaflet(data = reg_cont)%>%
  addTiles()%>%
  addPolygons(popup = ~ paste("nom:",nom,"code insee:",code_insee))
