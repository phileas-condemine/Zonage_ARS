


#SI LES DONNEES NE SONT PAS DEJA PRE-PROCESSED#
###############################################

# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
# token <- readRDS("droptoken.rds")
# drop_acc(dtoken = token)

prep_geo_data_from_scratch <- function(my_reg,regions = regions_reac(),dep = dep_reac(),
                                       dropbox_folder = dropbox_folder(),
                                       refresh_geojson = F,mailles_geo = c("TVS","BVCV")
                                       ,TVS = NULL#TVS()
                                       ,BVCV= NULL#BVCV()
                                       ,params
                                       ){
  reg_name=regions[reg==my_reg]$libreg
  my_deps=dep[reg==my_reg]$dep
  nb_deps=length(my_deps)
  communes=lapply(my_deps,function(my_dep){
    if (refresh_geojson | !paste0(my_dep,".json")%in%list.files("data/geojson/")){
      download.file(paste0("https://geo.api.gouv.fr/departements/",my_dep,"/communes?fields=nom,code,population,contour&format=geojson&geometry=contour"),
                    destfile = paste0("data/geojson/",my_dep,".json"))
      
    }
    info_com_sf=geojsonsf::geojson_sf(paste0("data/geojson/",my_dep,".json"))
    info_com_sf$nom=iconv(info_com_sf$nom,"UTF-8","latin1")
    info_com_sf$nom=iconv(info_com_sf$nom,"latin1","UTF-8")
    info_com_sf
  })
  communes=do.call("rbind",communes)
  communes=unique(communes)
  
  
  names(communes)[which(names(communes)=="nom")]<-"libcom"
  names(communes)[which(names(communes)=="code")]<-"depcom"
  
  if(my_reg %in% c(11,84,93)){
    filename = params[file=="polygones_arrondissements_PLM"]$name
    if(!filename%in%list.files("data/")){
      drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
    }
    load(paste0("data/",filename))
    
    filename = params[file=="pop_plm"]$name
    if(!filename%in%list.files("data/")){
      drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
    }
    load(paste0("data/",filename))
    z_pop <- pop_plm
    print(head(z_pop))
    arr = lapply(arr,function(x){
      x@data$depcom = as.character(x@data$depcom)
      tmp = x@data
      tmp$ordre = 1:nrow(tmp)
      tmp = merge(tmp,z_pop,by="depcom")
      setorder(tmp,"ordre")
      tmp$ordre=NULL
      x@data = tmp
      x
    })
    print(head(arr))
    
  }
  
  
  if(my_reg == 11){
    communes <- rbind(communes,
                      arr[["paris"]] %>% sf::st_as_sf()%>%select(libcom,depcom,population)) 
  }
  if(my_reg == 84){
    communes <- rbind(communes,
                      arr[["lyon"]] %>% sf::st_as_sf()%>%select(libcom,depcom,population)) 
  }
  if(my_reg == 93){
    communes <- rbind(communes,
                      arr[["marseille"]] %>% sf::st_as_sf()%>%select(libcom,depcom,population)) 
  }
  
  
  
  communes$my_reg_TVS=T
  communes$my_reg_BVCV=T
  
  # Pour savoir si la région est "majoritaire" dans le TVS ou BVCV, 
  # on va récupérer les données des communes des régions adjacentes. 
  
  other_deps <- c()
  for(a in mailles_geo){
    AGR <- get(a)#()
    AGR_pertinents=AGR[agr%in%unique(AGR[depcom%in%communes$depcom]$agr)]
    other_deps_one=unique(AGR_pertinents$dep)
    other_deps_one=other_deps_one[!other_deps_one%in%my_deps]
    other_deps <- c(other_deps,other_deps_one)
    # assign(paste("other_deps",a,sep="_"),other_deps)
  }
  # other_deps <- unique(c(other_deps_BVCV,other_deps_TVS))
  # remove(other_deps_BVCV,other_deps_TVS)
  other_deps = unique(other_deps)
  
  if (length(other_deps)>0){
    nb_deps=length(other_deps)
    
    other_communes=lapply(other_deps,function(my_dep){
      # info_com=jsonlite::fromJSON(sprintf("https://geo.api.gouv.fr/communes?codeDepartement=%s&fields=nom,code,population&format=json&geometry=centre",my_dep))
      if (refresh_geojson | !paste0(my_dep,".json")%in%list.files("data/geojson/")){
        download.file(sprintf(
          "https://geo.api.gouv.fr/departements/%s/communes?fields=nom,code,population,contour&format=geojson&geometry=contour"
          ,my_dep),paste0("data/geojson/",my_dep,".json"))            
      } 
      
      info_com_sf=geojsonsf::geojson_sf(paste0("data/geojson/",my_dep,".json"))
      # leaflet(info_com_sf)%>%addTiles()%>%addPolygons()
      info_com_sf$nom=iconv(info_com_sf$nom,"UTF-8","latin1")
      info_com_sf$nom=iconv(info_com_sf$nom,"latin1","UTF-8")
      info_com_sf
      
    })
    
    
    other_communes=do.call("rbind",other_communes)
    communes_pertinentes_TVS = ""#init
    communes_pertinentes_BVCV = ""#init
    for(a in mailles_geo){
      AGR <- get(a)#()
      communes_pertinentes <- AGR_pertinents$depcom
      assign(paste("communes_pertinentes",a,sep="_"),communes_pertinentes)
    }
    communes_pertinentes <- unique(c(communes_pertinentes_BVCV,communes_pertinentes_TVS))
    other_communes=other_communes[other_communes$code%in%communes_pertinentes,]
    other_communes$my_reg_TVS <- ifelse(other_communes$code%in%communes_pertinentes_TVS,F,NA)
    other_communes$my_reg_BVCV <- ifelse(other_communes$code%in%communes_pertinentes_BVCV,F,NA)
    print("correspondance des communes des régions voisines entre TVS et BVCV")
    print(table(other_communes$my_reg_BVCV,other_communes$my_reg_TVS))
    names(other_communes)[which(names(other_communes)=="nom")]<-"libcom"
    names(other_communes)[which(names(other_communes)=="code")]<-"depcom"
    
    communes=rbind(communes,other_communes)
  }
  communes2 <- communes
  
  for(a in mailles_geo){
    AGR <- get(a)#()
    
    communes=merge(communes2,AGR %>% select(-libcom),by="depcom")
    
    
    if(my_reg == 4 & a =="TVS"){
      
      filename = params[file=="polygones_grands_quartiers_reunion"]$name
      if(!filename%in%list.files("data/")){
        drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
      }
      load(paste0("data/",filename))
      filename = params[file=="zonage_mg"]$name
      if(!filename%in%list.files("data/")){
        drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
      }
      z_pop = readxl::read_xlsx(paste0("data/",filename),
                                sheet="Zonage_TVS")[,c(2,1,11)]
      names(z_pop) <- c("reg","agr","population")
      z_pop = z_pop[z_pop$reg==4,]
      z_pop$reg=NULL
      z_pop$agr = substr(z_pop$agr,7,13)
      grdquart_reu$my_reg_TVS=T
      grdquart_reu$my_reg_BVCV=T
      names(grdquart_reu@data)
      grdquart_reu@data = merge(grdquart_reu@data,z_pop,by.x="depcom",by.y="agr")
      
      communes = grdquart_reu%>% 
        sf::st_as_sf() %>% 
        select(depcom = depcom,
               libcom = libcom,
               population,
               my_reg_TVS,
               my_reg_BVCV,
               reg,
               dep,
               libdep,
               libreg,
               geometry)%>%
        mutate(
          agr=depcom,
          libagr=libcom
        )
      
      
      # carte <- rbind(carte[-which(substr(carte$agr,1,3)=="974"),],
      #                grdquart_reu %>% sf::st_as_sf() %>%
      #                  select(agr=depcom,libagr=libcom,geometry))
    }
    if(my_reg == 6 & a == "TVS"){
      #### LA POPULATION N'EST PAS FOURNIE PAR L'API GEO POUR MAYOTTE
      
      filename = params[file=="zonage_mg"]$name
      if(!filename%in%list.files("data/")){
        drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
      }
      z_pop = readxl::read_xlsx(paste0("data/",filename),
                                sheet="Zonage_TVS")[,c(2,5,11)]
      names(z_pop) <- c("reg","agr","population")
      z_pop = z_pop[z_pop$reg==6,]
      z_pop$reg=NULL
      communes = merge(communes,z_pop,by="agr",all.x=T)
      
    }
    
    
    
    
    # Nettoyage des polygones pour éviter les erreurs
    print("Fusion des polygones (regroupement des communes en AGR). Ca peut prendre 1 minute la première fois.")
    print(system.time(communes$geometry <-
                        communes$geometry %>% 
                        st_transform(2154) %>%
                        as('Spatial')%>%
                        # sp::spTransform( CRS( "+init=epsg:2154" ) ) %>%
                        rgeos::gBuffer(byid=TRUE, width=0)%>%
                        # sp::spTransform( CRS( "+init=epsg:4326" ) ) %>%
                        # sp::spTransform( CRS( "+no_defs +datum=WGS84 +proj=longlat" ) ) %>%
                        st_as_sf()%>%
                        st_transform(4326) %>%
                        .$geometry))
    
    
    
    communes_dissolved=aggregate(x = communes,
                                 by = list("code_agr"=communes$agr),
                                 FUN = function(x)x[1])
    
    
    
    
    # leaflet()%>%
    #   addPolygons(data=communes_dissolved)
    print("polygons reduction")
    print(format(object.size(communes_dissolved), units = "Mb"))
    print(system.time(communes_dissolved$geometry <-communes_dissolved$geometry %>%
                        st_transform(2154) %>%
                        as('Spatial')%>%
                        # sp::spTransform( CRS( "+init=epsg:2154" ) ) %>%
                        rgeos::gSimplify(topologyPreserve = T,tol=200) %>%
                        # rmapshaper::ms_simplify()%>%
                        rgeos::gBuffer(byid=TRUE, width=0)%>%
                        # sp::spTransform( CRS( "+init=epsg:4326" ) ) %>%
                        # sp::spTransform( CRS( "+no_defs +datum=WGS84 +proj=longlat" ) ) %>%
                        st_as_sf()%>%
                        st_transform(4326) %>%
                        .$geometry))
    
    print(format(object.size(communes_dissolved), units = "Mb"))
    # Génère des problèmes avec les multipolygones au moment de l'affichage. Exemple Agde, Gigean en Occitanie.
    carte=communes_dissolved[,c("agr","libagr","geometry")]
    
    names(st_geometry(carte)) = NULL #https://github.com/rstudio/leaflet/issues/595
    
    assign(paste("communes",a,sep="_"),communes)
    assign(paste("carte",a,sep="_"),carte)
  }
  if("TVS"%in%mailles_geo){
    save(communes_TVS,carte_TVS,
         file=paste0("data/",my_reg,"_preprocessed_TVS.RData")) 
  }
  if("BVCV"%in%mailles_geo){
    save(communes_BVCV,carte_BVCV,
         file=paste0("data/",my_reg,"_preprocessed_BVCV.RData"))
  }
  print("upload to dropbox")
  
  if("TVS"%in%mailles_geo){
    
    file = paste0(my_reg,"_preprocessed_TVS.RData")
    
    drop_clean_upload(filename = file,message="rm tvs",drop_path=dropbox_folder)
    
  }
  
  if("BVCV"%in%mailles_geo){
    
    file = paste0(my_reg,"_preprocessed_BVCV.RData")
    drop_clean_upload(filename = file,message="rm bvcv",drop_path=dropbox_folder)
  }
}


#SELON SI LES DONNEES SONT DEJA PRE-PROCESSED OU NON#
#####################################################

#' @param ... les paramètres à transmettre à la fonction prep_geo_data_from_scratch
get_geo_data <- function(my_reg,path,env,my_ps,nom_fichier_dropbox,...){
  print("ask update contours ?")
  if (!paste0(my_reg,nom_fichier_dropbox)%in%list.files("data/")){
    if(rdrop2::drop_exists(paste0(path,my_reg,nom_fichier_dropbox))){
      print("récupération de l'historique dropbox")
      rdrop2::drop_download(path = paste0(path,my_reg,nom_fichier_dropbox),overwrite = T,local_path = "data")
    } else {
      print("construction fonds géo from scratch")
      if(my_ps=="mg"){
        mailles_geo = "TVS"
      } else {
        mailles_geo = "BVCV"
      }
      prep_geo_data_from_scratch(my_reg = my_reg,mailles_geo=mailles_geo,...)
    }
  } else {
    print("fichier fonds géo déjà présent")
  }
  print(paste0("chargement du fonds géo ",my_reg,nom_fichier_dropbox))
  load(paste0("data/",my_reg,nom_fichier_dropbox),envir = env)
}





create_and_upload_reg_majo_per_agr = function(regions=regions_reac(),dep=dep_reac(),
                                              dropbox_folder=dropbox_folder(),TVS=TVS(),BVCV=BVCV(),params=params){
  #### TVS        
  
  all_com_tvs = rbindlist(use.names = T,lapply(regions$reg,function(my_reg){
    file = paste0(my_reg,"_preprocessed_TVS.RData")
    if(!file%in%list.files("data/")){
      rdrop2::drop_download(path = paste0(dropbox_folder,my_reg,"_preprocessed_TVS.RData"),overwrite = T,local_path = "data")
    }
    load(paste0("data/",file))
    communes_TVS
  }))
  
  tvs=data.table(all_com_tvs)
  tvs = tvs[,.("pop_tvs_per_reg"=sum(population)),by=c("agr","reg","libagr")]
  tvs[,reg:=gsub("^0","",reg)]
  setorder(tvs,-pop_tvs_per_reg)
  tvs_reg_majoritaire=tvs[,list(reg_majoritaire=reg[1],
                                prop_pop_pct = round(100*pop_tvs_per_reg[1]/sum(pop_tvs_per_reg),1),
                                distr = paste(paste0("reg ",reg," (",round(100*pop_tvs_per_reg/sum(pop_tvs_per_reg),1),"%)"),collapse = ", ")),
                          by=c("agr","libagr")]
  
  # uniqueN(TVS[reg!="4"]$agr) == nrow(tvs_reg_majoritaire[reg_majoritaire!="4"])
  # sum(TVS$agr%in%tvs_reg_majoritaire$agr)
  # unique(TVS[!agr%in%tvs_reg_majoritaire$agr,c("agr","libagr","reg")])
  # unique(tvs_reg_majoritaire[!agr%in%TVS$agr])
  # les fichiers diffèrent pour la Réunion où on utilise les GQ et non les TVS
  
  
  #### BVCV
  
  
  
  all_com_bvcv = rbindlist(use.names = T,lapply(setdiff(regions$reg,"6"),function(my_reg){
    print(my_reg)
    file = paste0(my_reg,"_preprocessed_BVCV.RData")
    nom_fichier_dropbox ="_preprocessed_BVCV.RData"
    if(!file%in%list.files("data/")){
      
      if(rdrop2::drop_exists(paste0(dropbox_folder,my_reg,nom_fichier_dropbox))){
        print("récupération de l'historique dropbox")
        rdrop2::drop_download(path = paste0(dropbox_folder,my_reg,nom_fichier_dropbox),overwrite = T,local_path = "data")
      } else {
        print("construction fonds géo from scratch")
        prep_geo_data_from_scratch(my_reg = my_reg,
                                   regions = regions,
                                   dep = dep,
                                   dropbox_folder = dropbox_folder,
                                   TVS = TVS,
                                   BVCV = BVCV,params=params)
      }
    }
    load(paste0("data/",file))
    communes_BVCV
  }))
  
  bvcv=data.table(all_com_bvcv)
  bvcv = bvcv[,.("pop_bvcv_per_reg"=sum(population)),by=c("agr","reg","libagr")]
  bvcv[,reg:=gsub("^0","",reg)]
  setorder(bvcv,-pop_bvcv_per_reg)
  bvcv_reg_majoritaire=bvcv[,list(reg_majoritaire=reg[1],
                                  prop_pop_pct = round(100*pop_bvcv_per_reg[1]/sum(pop_bvcv_per_reg),1),
                                  distr = paste(paste0("reg ",reg," (",round(100*pop_bvcv_per_reg/sum(pop_bvcv_per_reg),1),"%)"),collapse = ", ")),
                            by=c("agr","libagr")]
  
  uniqueN(BVCV$agr) == nrow(bvcv_reg_majoritaire)
  
  filename = "agr_reg_majoritaire.RData"
  local_name = paste0("data/",filename)
  
  tvs_reg_majoritaire[,reg_majoritaire:=as.numeric(reg_majoritaire)]
  bvcv_reg_majoritaire[,reg_majoritaire:=as.numeric(reg_majoritaire)]
  save(bvcv_reg_majoritaire,tvs_reg_majoritaire,file = local_name)
  
  drop_clean_upload(filename = filename,drop_path=dropbox_folder)
}

