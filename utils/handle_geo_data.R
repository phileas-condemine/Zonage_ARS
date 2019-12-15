reg_name=regions[reg==my_reg]$libreg
my_deps=dep[reg==my_reg]$dep
# 
# my_google_files <- gs_ls()
# relevant_sheets=sapply(regions$libreg,stringr::str_which,string = my_google_files$sheet_title)
# relevant_sheets=unname(unlist(relevant_sheets))
# my_google_files <- my_google_files[relevant_sheets,]
# # my_google_files$sheet_title
# 
# reg_google_files <- my_google_files%>%
#   filter(grepl(reg_name,sheet_title))%>%
#   arrange(desc(updated))
# 
# 
# 
# 
# print("update choix millésimes")
# millesimes <- setNames(reg_google_files$sheet_title,
#                        gsub(reg_name,"",reg_google_files$sheet_title))
# 
# 

#SI LES DONNEES NE SONT PAS DEJA PRE-PROCESSED#
###############################################

# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

prep_geo_data_from_scratch <- function(my_reg){
  nb_deps=length(my_deps)
  communes=lapply(my_deps,function(my_dep){
    if (!paste0(my_dep,".json")%in%list.files("data/geojson/")){
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
  communes$my_reg_TVS=T
  communes$my_reg_BVCV=T
  
  # Pour savoir si la région est "majoritaire" dans le TVS ou BVCV, 
  # on va récupérer les données des communes des régions adjacentes. 
  
  for(a in c("TVS","BVCV")){
    AGR <- get(a)
    AGR_pertinents=AGR[agr%in%unique(AGR[depcom%in%communes$depcom]$agr)]
    other_deps=unique(AGR_pertinents$dep)
    other_deps=other_deps[!other_deps%in%my_deps]
    assign(paste("other_deps",a,sep="_"),other_deps)
  }
  other_deps <- unique(c(other_deps_BVCV,other_deps_TVS))
  remove(other_deps_BVCV,other_deps_TVS)
  
  if (length(other_deps)>0){
    nb_deps=length(other_deps)
    
    other_communes=lapply(other_deps,function(my_dep){
      # info_com=jsonlite::fromJSON(sprintf("https://geo.api.gouv.fr/communes?codeDepartement=%s&fields=nom,code,population&format=json&geometry=centre",my_dep))
      if (!paste0(my_dep,".json")%in%list.files("data/geojson/")){
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
    
    for(a in c("TVS","BVCV")){
      AGR <- get(a)
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

  for(a in c("TVS","BVCV")){
    AGR <- get(a)
    
    communes=merge(communes2,AGR %>% select(-libcom),by="depcom")
    
    # Nettoyage des polygones pour éviter les erreurs
    print("Fusion des polygones (regroupement des communes en AGR). Ca peut prendre 1 minute la première fois.")
    print(system.time(communes$geometry <-
                        as(communes$geometry, 'Spatial')%>%
                        sp::spTransform( CRS( "+init=epsg:2154" ) ) %>%
                        rgeos::gBuffer(byid=TRUE, width=0)%>%
                        sp::spTransform( CRS( "+init=epsg:4326" ) ) %>%
                        st_as_sf()%>%.$geometry))
    
    
    communes_dissolved=aggregate(x = communes,
                                 by = list("code_agr"=communes$agr),
                                 FUN = function(x)x[1])
    # leaflet()%>%
    #   addPolygons(data=communes_dissolved)
    print("polygons reduction")
    print(format(object.size(communes_dissolved), units = "Mb"))
    print(system.time(communes_dissolved$geometry <-
                        as(communes_dissolved$geometry, 'Spatial')%>%
                        sp::spTransform( CRS( "+init=epsg:2154" ) ) %>%
                        rgeos::gSimplify(topologyPreserve = T,tol=200) %>%
                        # rmapshaper::ms_simplify()%>%
                        rgeos::gBuffer(byid=TRUE, width=0)%>%
                        sp::spTransform( CRS( "+init=epsg:4326" ) ) %>%
                        st_as_sf()%>%.$geometry))
    
    print(format(object.size(communes_dissolved), units = "Mb"))
    # Génère des problèmes avec les multipolygones au moment de l'affichage. Exemple Agde, Gigean en Occitanie.
    carte=communes_dissolved[,c("agr","libagr","geometry")]
    
    names(st_geometry(carte)) = NULL #https://github.com/rstudio/leaflet/issues/595
  
    assign(paste("communes",a,sep="_"),communes)
    assign(paste("carte",a,sep="_"),carte)
  }
  # carte_TVS$agr = gsub(" ","", carte_TVS$agr)
  # carte_TVS$agr = sprintf("%05s", carte_TVS$agr)
  # carte_TVS$agr = gsub(" ","0", carte_TVS$agr)
  # communes_TVS$agr = gsub(" ","", communes_TVS$agr)
  # communes_TVS$agr = sprintf("%05s", communes_TVS$agr)
  # communes_TVS$agr = gsub(" ","0", communes_TVS$agr)
  save(communes_TVS,carte_TVS,
       file=paste0("data/",my_reg,"_preprocessed_TVS.RData")) 
  # carte_BVCV$agr = gsub(" ","", carte_BVCV$agr)
  # carte_BVCV$agr = sprintf("%05s", carte_BVCV$agr)
  # carte_BVCV$agr = gsub(" ","0", carte_BVCV$agr)
  # communes_BVCV$agr = gsub(" ","", communes_BVCV$agr)
  # communes_BVCV$agr = sprintf("%05s", communes_BVCV$agr)
  # communes_BVCV$agr = gsub(" ","0", communes_BVCV$agr)
  save(communes_BVCV,carte_BVCV,
       file=paste0("data/",my_reg,"_preprocessed_BVCV.RData"))
  print("upload to dropbox")
  rdrop2::drop_upload(file = paste0("data/",my_reg,"_preprocessed_TVS.RData"),
                      dtoken = token,path = "zonage/")
  rdrop2::drop_upload(file = paste0("data/",my_reg,"_preprocessed_BVCV.RData"),
                      dtoken = token,path = "zonage/")
  }


#SELON SI LES DONNEES SONT DEJA PRE-PROCESSED OU NON#
#####################################################


get_geo_data <- function(env){
  print("ask update contours ?")
  if (!paste0(my_reg,nom_fichier_dropbox)%in%list.files("data/")){
    if(rdrop2::drop_exists(paste0("zonage/",my_reg,nom_fichier_dropbox),dtoken = token)){
      print("récupération de l'historique dropbox")
      rdrop2::drop_download(path = paste0("zonage/",my_reg,nom_fichier_dropbox),overwrite = T,
                            dtoken = token,verbose = T,
                            local_path = "data")
    } else {
      print("construction fonds géo from scratch")
      prep_geo_data_from_scratch(my_reg)
    }
  } else {
    print("fichier fonds géo déjà présent")
  }
  print(paste0("chargement du fonds géo ",my_reg,nom_fichier_dropbox))
  load(paste0("data/",my_reg,nom_fichier_dropbox),envir = env)
}




