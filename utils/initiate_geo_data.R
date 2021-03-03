source("global.R")
# my_reg = regions$reg[1]
drop_auth(rdstoken = "droptoken.rds")
params = fread("params.csv",sep=":")

dropbox_folder =  "zonage/" # "zonage_dev/"
TVS = get_TVS(dropbox_folder,params[file=="tvs"]$name)
dep = unique(TVS[,c("dep","reg","libdep")])
regions = get_regions_seuils(dropbox_folder,params[file=="seuils_arretes"]$name,TVS)
BVCV = get_BVCV(dropbox_folder,params[file=="bvcv"]$name)


my_reg = 28
my_reg = 84 # ARA
my_reg = 11 
my_reg = 4 #La Réunion
todo = c(11,84,93)#PLM
todo = setdiff(regions$reg,6)#no mayotte
todo = 11
drop_auth(rdstoken = "droptoken.rds")
params = fread("params.csv",sep=":")


  
for (my_reg in todo){
  print(my_reg)
  reg_name=regions[reg==my_reg]$libreg
  my_deps=dep[reg==my_reg]$dep
  # prep_geo_data_from_scratch(my_reg)
  prep_geo_data_from_scratch(my_reg = my_reg,
                             regions = regions,
                             dep = dep,
                             dropbox_folder = dropbox_folder,
                             TVS = TVS,
                             BVCV = BVCV,params = params)
}





rerun_dep_contours = T
rerun_dep_contours = F

path = "zonage/"
path = "zonage_dev/"
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
  rdrop2::drop_upload(file="data/contours_dep.RData",path=path,autorename = F)
  
}
