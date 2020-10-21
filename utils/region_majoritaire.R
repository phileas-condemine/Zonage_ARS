

#### TVS

# source("global.R")

all_com_tvs = rbindlist(use.names = T,lapply(regions$reg,function(my_reg){
  file = paste0(my_reg,"_preprocessed_TVS.RData")
  if(!file%in%list.files("data/")){
    rdrop2::drop_download(path = paste0("zonage/",my_reg,"_preprocessed_TVS.RData"),overwrite = T,local_path = "data")
  }
  load(paste0("data/",file))
  communes_TVS
}))

tvs=data.table(all_com_tvs)
tvs = tvs[,.("pop_tvs_per_reg"=sum(population)),by=c("agr","reg","libagr")]
tvs[,reg:=gsub("^0","",reg)]
setorder(tvs,-pop_tvs_per_reg)
tvs_reg_majoritaire=tvs[,list(reg_majoritaire=reg[1],
                              prop_pop_pct = 100*pop_tvs_per_reg[1]/sum(pop_tvs_per_reg),
                              distr = paste(paste0("reg n°: ",reg," (",100*pop_tvs_per_reg/sum(pop_tvs_per_reg),"%)"),collapse = ", ")),
                        by=c("agr","libagr")]

uniqueN(TVS[reg!="4"]$agr) == nrow(tvs_reg_majoritaire[reg_majoritaire!="4"])
sum(TVS$agr%in%tvs_reg_majoritaire$agr)
unique(TVS[!agr%in%tvs_reg_majoritaire$agr,c("agr","libagr","reg")])
unique(tvs_reg_majoritaire[!agr%in%TVS$agr])
# les fichiers diffèrent pour la Réunion où on utilise les GQ et non les TVS


#### BVCV
source("utils/handle_geo_data.R",local = T)
all_com_bvcv = rbindlist(use.names = T,lapply(setdiff(regions$reg,"6"),function(my_reg){
  print(my_reg)
  file = paste0(my_reg,"_preprocessed_BVCV.RData")
  nom_fichier_dropbox ="_preprocessed_BVCV.RData"
  if(!file%in%list.files("data/")){

    if(rdrop2::drop_exists(paste0("zonage/",my_reg,nom_fichier_dropbox))){
      print("récupération de l'historique dropbox")
      rdrop2::drop_download(path = paste0("zonage/",my_reg,nom_fichier_dropbox),overwrite = T,local_path = "data")
    } else {
      print("construction fonds géo from scratch")
      prep_geo_data_from_scratch(my_reg)
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
                                prop_pop_pct = 100*pop_bvcv_per_reg[1]/sum(pop_bvcv_per_reg),
                                distr = paste(paste0("reg n°: ",reg," (",100*pop_bvcv_per_reg/sum(pop_bvcv_per_reg),"%)"),collapse = ", ")),
                          by=c("agr","libagr")]

uniqueN(BVCV$agr) == nrow(bvcv_reg_majoritaire)

file = "agr_reg_majoritaire.RData"
local_name = paste0("data/",file)
drop_name = paste0("zonage/",file)

tvs_reg_majoritaire[,reg_majoritaire:=as.numeric(reg_majoritaire)]
bvcv_reg_majoritaire[,reg_majoritaire:=as.numeric(reg_majoritaire)]
save(bvcv_reg_majoritaire,tvs_reg_majoritaire,file = local_name)
if(rdrop2::drop_exists(drop_name)){
  print("rm fichier régions majoritaires")
  rdrop2::drop_delete(path = drop_name)
}
rdrop2::drop_upload(file = local_name,path = "zonage/",autorename = F)
