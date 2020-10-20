source("global.R")
# my_reg = regions$reg[1]
my_reg = 6
my_reg = 28
my_reg = 84 # ARA
my_reg = 4 #La Réunion
todo = c(11,84,93)#PLM - Réu
todo = setdiff(regions$reg,6)
for (my_reg in todo){#Mayotte est exclue !
  print(my_reg)
  reg_name=regions[reg==my_reg]$libreg
  my_deps=dep[reg==my_reg]$dep
  source("utils/handle_geo_data.R",local = T)
  prep_geo_data_from_scratch(my_reg)
  
}


rerun_dep_contours = F

if(rerun_dep_contours){
  files = list.files("data/")
  files = grep("preprocessed_BVCV.RData",files,value=T)
  communes = pbapply::pblapply(files,function(file){
    load(paste0("data/",file))
    communes_BVCV
  })
  communes = do.call("rbind",communes)
  dep_contours=aggregate(x = communes,
                         by = list("dep"=communes$dep),
                         FUN = function(x)x[1])
  dep_contours = dep_contours[,c("reg","dep","geometry")]
  save(dep_contours,file="data/contours_dep.RData")
  rdrop2::drop_upload(file="data/contours_dep.RData",dtoken=token,path="zonage/",autorename = F)
  
}
