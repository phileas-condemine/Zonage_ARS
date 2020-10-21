library(data.table)
library(googledrive)
options(gargle_oauth_cache = ".secrets")

drive_auth(email = "drees-zonage-ars@sante.gouv.fr",
           token=".secrets/a924da283165ded31d5fb98542b0974f_drees-zonage-ars@sante.gouv.fr")

histo <- drive_find(type = "csv",q = "name contains 'mg_'")
histo <- drive_find(type = "csv",q = "name contains 'sf_'")
histo <- drive_find(type = "csv",q = "name contains 'inf_'")
histo <- drive_find(type = "csv",q = "name contains 'en_vigueur_'")
histo <- drive_find(type = "csv",q = "name contains 'qpv_'")

for(nm in histo$name){
  print(nm)
  drive_download(file = nm,path=paste0("data/",nm,".csv"),overwrite = T,type="csv")
}
