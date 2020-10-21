

#### TVS

source("global.R")

all_com_tvs = rbindlist(lapply(regions$reg,function(my_reg){
  file = paste0(my_reg,"_preprocessed_TVS.RData")
  if(!file%in%list.files("data/")){
    rdrop2::drop_download(path = paste0("zonage/",my_reg,"_preprocessed_TVS.RData"),overwrite = T,
                          dtoken = token,verbose = T,
                          local_path = "data")
  }
  load(paste0("data/",file))
  communes_TVS
}))
