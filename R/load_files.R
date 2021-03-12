
get_auth = function(dropbox_folder,local_dir="data"){
  message("func : get_auth")
  #created in utils/keygen.R
  filename = "auth.txt"
  if(!filename%in%list.files(local_dir)){
    rdrop2::drop_download(path = paste0(dropbox_folder,filename),overwrite = T,local_path = local_dir)
  }
  auth = fread(paste0(local_dir,"/auth.txt"))
  
  auth
}


get_TVS = function(dropbox_folder,filename){
  message("func : get_TVS")
  # drop_upload(file = paste0("data/",filename),path = "zonage/",mode = "overwrite",autorename = F)
  if(!filename%in%list.files("data")){
    rdrop2::drop_download(path = paste0(dropbox_folder,filename),overwrite = T,local_path = "data")
  }
  TVS = fread(paste0("data/",filename))
  TVS=data.table(TVS)
  setnames(TVS,c("depcom","libcom","tv","libtv","code_reg","code_dep","libdep","libreg"),
           c("depcom","libcom","agr","libagr","reg","dep","libdep","libreg"))
  TVS$agr = stringi::stri_pad_right(TVS$agr,5,"_")
  # TESTS ICI
  
  TVS
}


get_QPV = function(dropbox_folder,filename){
  message("func : get_QPV")
  # QPV
  # création dans utils/prep_qpv.R
  if(!filename%in%list.files("data")){
    rdrop2::drop_download(path = paste0(dropbox_folder,filename),overwrite = T,local_path = "data")
  }
  load(paste0("data/",filename))
  # TESTS ICI
  
  mom_markers
}



get_BVCV = function(dropbox_folder,filename){
  message("func : get_BVCV")
  # drop_upload(file = "data/bvcv2020.sas7bdat",path = "zonage",mode = "overwrite",autorename = F)
  if(!filename%in%list.files("data")){
    rdrop2::drop_download(path = paste0(dropbox_folder,filename),overwrite = T,local_path = "data")
  }
  
  BVCV = fread(paste0("data/",filename)) #%>%
  # select(-type_zone,-taille_pole,-bv2012,-libbv,-cv,-libcv)
  BVCV=data.table(BVCV)
  setnames(BVCV,c('bvcv','libbvcv'),c('agr','libagr'))
  BVCV$agr = stringi::stri_pad_right(BVCV$agr,5,"_")
  # TESTS ICI
  uniqueN(BVCV$agr)
  uniqueN(BVCV$libagr)
  uniqueN(BVCV[,.(agr,libagr)])
  
  BVCV
}

get_hist_qpv = function(dropbox_folder,filename){
  message("func : get_hist_qpv")
  # drop_upload(file = paste0("data/",filename),
  # path = "zonage",mode = "overwrite",autorename = F)
  if(!filename%in%list.files("data")){
    rdrop2::drop_download(path = paste0(dropbox_folder,filename),overwrite = T,local_path = "data")
  }
  
  hist_qpv <- readxl::read_excel(paste0("data/",filename),sheet = "Zonage_QPV")[,c(1,3,5,6,10,12)]
  hist_qpv = data.table(hist_qpv)
  names(hist_qpv) <- c("reg","agr","cod","libqpv","zonage_ars","pop")
  hist_qpv[zonage_ars=="Zone de vigilance",zonage_ars:="ZV"]
  hist_qpv[zonage_ars=="Hors vivier",zonage_ars:="HV"]
  hist_qpv=hist_qpv%>%mutate_if(is.factor,as.character)%>%data.table%>%unique
  hist_qpv$agr = stringi::stri_pad_right(hist_qpv$agr,5,"_")
  hist_qpv
}

get_regions_seuils = function(dropbox_folder,seuils_filename,TVS){
  # rdrop2::drop_upload(file=paste0("data/",seuils_filename),path="zonage/",autorename = F)
  message("func : get_regions_seuils")
  if(!seuils_filename%in%list.files("data"))
    drop_download(paste0(dropbox_folder,seuils_filename),local_path = "data/",overwrite = T)
  
  # browser()
  seuils_reg_mg=read_xlsx(paste0("data/",seuils_filename),sheet="mg")
  seuils_reg_sf=read_xlsx(paste0("data/",seuils_filename),sheet="sf")%>%rename(check_sf=check)
  seuils_reg_inf=read_xlsx(paste0("data/",seuils_filename),sheet="inf")%>%rename(check_inf=check)
  regions = unique(TVS[,c("reg","libreg")])
  regions=merge(regions,
                seuils_reg_mg %>% select(-libreg),
                by="reg")
  regions=merge(regions,
                seuils_reg_sf %>% select(-libreg),
                by="reg")
  regions=merge(regions,
                seuils_reg_inf %>% select(-libreg),
                by="reg")
  # validUTF8(regions$libreg)
  regions[!validUTF8(libreg),libreg:=iconv(libreg,"latin1","UTF-8")]
  regions
}

get_TA = function(dropbox_folder,filename){
  message("func : get_TA")
  # get_TA()
  # filename = "liste_tribunaux_administratifs.RData"
  # rdrop2::drop_upload(file=paste0("data/",filename),path="zonage_dev/",autorename = F,mode = "overwrite")
  print("get TA")
  if(!filename%in%list.files("data")){
    drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
  }
  load(paste0("data/",filename))
  TA
}

get_pop_femmes = function(dropbox_folder,filename){
  message("func : get_pop_femmes")
  if(!filename%in%list.files("data"))
    drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
  load(paste0("data/",filename))
  pop_femmes
}

get_dep_contours = function(dropbox_folder,filename){
  message("func : get_dep_contours")
  # geo reg
  # le fichier est créé dans utils/initiate_geo_data.R
  print("get dep cont")
  if(!filename%in%list.files("data"))
    drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
  load(paste0("data/",filename))
  st_crs(dep_contours) <- 4326
  dep_contours
}

get_reg_contours = function(dropbox_folder,filename){
  message("func : get_reg_contours")
  #source utils/polygones_regions.R ??
  # rdrop2::drop_upload(file="data/reg_cont.RData",path="zonage/",autorename = F)
  print("get reg cont")
  if(!filename%in%list.files("data"))
    drop_download(paste0(dropbox_folder,filename),local_path = "data/",overwrite = T)
  load(paste0("data/",filename))
  names(reg_cont)[which(names(reg_cont)=='code_insee')]<-'reg'
  names(reg_cont)[which(names(reg_cont)=='nom')]<-'libreg'
  reg_cont
}







