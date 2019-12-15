# brouillon global

#<<<<<<< HEAD

# "05061" %in% zonage_historique$tvs
# zonage_historique[tvs%in%"05061"]
# table(zonage_historique$zonage_ars)
# zonage_historique[,list(count=.N),by=c("zonage_ars","code_reg")]%>%
# dcast(region~zonage_ars,value.var="count",fill=0)


# if(!"my_gadm.RData"%in%list.files("data")){
#   gadm=readRDS("data/gadm36_FRA_5_sf.rds")
#   load("data/poly_com_simplif.RData")
#   gadm_nom=gadm$NAME_5
#   nom2=poly_com[["2015"]]@data$NOM_COM.y
#   sum(is.na(nom2))
#   sum(!gadm_nom%in%nom2)
#   data=poly_com[["2015"]]@data
#   data=data.table(data)
#   data$nb_NA=rowSums(is.na(data))
#   setorder(data,nb_NA)
#   data=data[,.SD[1],by="NOM_COM.y"]
#   gadm=merge(gadm,data,by.x="NAME_5",by.y="NOM_COM.y")
#   gadm=sf::st_cast(gadm,"POLYGON")
#   gadm=gadm[!is.na(gadm$Z_MOYEN),]
#   
#   save(gadm,file = "data/my_gadm.RData")
# } 
# 
# load("data/my_gadm.RData")
# gadm$NAME_5=iconv(gadm$NAME_5,"UTF-8","latin1")
# 
# m = leaflet() %>%
#   addTiles()%>%
#   addGlPolygons(data = gadm,
#                 color = cols,
#                 popup = "popup",
#                 group = "Zonage ARS")%>%
#   setView(lng = 4, lat = 45, zoom = 6)
# 
# 
# 
# options(viewer = NULL)
#=======
#>>>>>>> 5e68e10007171c4b15add91c89fe7ccdc5037b34


# Brouillon handle geo


for(a in c("TVS","BVCV")){
  AGR <- get(a)
  b <- ifelse(a=="TVS","BVCV","TVS")
  communes_pertinentes=AGR_pertinents$depcom
  other_communes_tmp=other_communes[other_communes$code%in%communes_pertinentes,]
  other_communes_tmp[,paste("my_reg",a,sep="_")]=F
  other_communes_tmp[,paste("my_reg",b,sep="_")]=NA
  assign(paste("other_communes",a,sep="_"),other_communes_tmp)
}
remove(other_communes_tmp)


other_communes <- full_join(other_communes_TVS %>% as.data.frame(), 
                            other_communes_BVCV %>% as.data.frame(), 
                            by = "code")
other_communes$nom <- ifelse(!is.na(other_communes$nom.x),other_communes$nom.x,other_communes$nom.y)
other_communes$population <- ifelse(!is.na(other_communes$population.x),other_communes$population.x,other_communes$population.y)
other_communes$geometry <- ifelse(!is.na(other_communes$geometry.x),other_communes$geometry.x,other_communes$geometry.y)
other_communes <- other_communes %>% 
  select(-nom.x,-nom.y,-population.x,-population.y,
         -geometry.x,-geometry.y,-my_reg_TVS.y,-my_reg_BVCV.x) %>%
  rename(my_reg_TVS=my_reg_TVS.x,my_reg_BVCV=my_reg_BVCV.y) %>%
  st_sf(geometry)



# zonage_historique=readxl::read_xlsx("data/Zonage_medecin_20190703.xlsx",
#                                     sheet="Zonage_communes")
# zonage_historique <- zonage_historique %>% 
#   rename(id=`Identifiant unique`,
#          zonage_ars=`Qualification attribuée par l'arrêté régional`,
#          zonage_nat=`Qualification nationale issue de la concertation de 2017`,
#          reg=`Code de la région`,
#          libreg=`Libellé de la région`,
#          tvs=`Code du territoire de vie-santé`,
#          libtvs=`Libellé du territoire de vie-santé`,
#          depcom=`Code de la commune`,
#          libcom=`Libellé de la commune`,
#          poptot_2016=`Population totale (2016)`)
# zonage_historique$zonage_ars=factor(zonage_historique$zonage_ars)
# levels(zonage_historique$zonage_ars) <- c("HV","ZAC","ZIP","ZV")
# zonage_historique=zonage_historique%>%
#   mutate_if(is.factor,as.character)%>%
#   select(reg,tvs,zonage_ars,zonage_nat)
# zonage_historique=data.table(zonage_historique)
# zonage_historique=unique(zonage_historique)
