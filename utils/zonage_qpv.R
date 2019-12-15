# Shape files QPPV

library(dplyr)

metrop <- sf::st_read("data/Shape_files_qppv/QP_METROPOLEOUTREMER_WGS84_EPSG4326.shp")
centroids <- metrop %>% sf::st_transform(29101) %>% sf::st_centroid() %>% sf::st_transform(4326)
plot(centroids)

qppv <- readxl::read_excel("data/Zonage_médecin_20190703.xlsx",
                           sheet="Zonage_QPV")

qppv_manquants <- anti_join(centroids %>% select(CODE_QP,geometry) %>% 
                    rename('Code du quartier prioritaire'=CODE_QP),
                    qppv,
                  by='Code du quartier prioritaire')

qppv <- left_join(qppv,centroids %>% select(CODE_QP,geometry) %>% 
                    rename('Code du quartier prioritaire'=CODE_QP),
                  by='Code du quartier prioritaire')



coords <- sf::st_coordinates(qppv$geometry) %>% as.data.frame()

qppv <- cbind(qppv,coords)

qppv$`Qualification attribuée au QPV par l'arrêté régional` <- ifelse(qppv$`Code du quartier prioritaire` =='QP013013',
                                                                      'ZAC',
                                                                      qppv$`Qualification attribuée au QPV par l'arrêté régional`)

save(qppv,file="zonage_qppv.RData")


library(leaflet)

pal <- colorFactor(c('red','orange','yellow','yellow'),
                   domain=c('ZIP','ZAC','Hors vivier','Zone de vigilance'),
                   ordered=TRUE)

leaflet(qppv) %>% addTiles() %>% 
  setView(lng = 1.7, lat = 46.8167, zoom = 5) %>%
  addCircleMarkers(lng=~X,lat=~Y,
                   label=~`Qualification attribuée au QPV par l'arrêté régional`,
             color=~pal(`Qualification attribuée au QPV par l'arrêté régional`),
             radius=2)

zonage_manquant <- qppv %>% filter(`Qualification attribuée au QPV par l'arrêté régional`=='0')                 
