my_google_files <- drive_find(type = "csv",pattern = "en_vigueur")
my_google_files <- my_google_files[grepl(paste0("^",input$choix_ps),my_google_files$name),]
print("récupération des fichiers de zonage en vigueur")
if (length(my_google_files$name)>0){
  files = lapply(my_google_files$name,function(en_vigueur){
    print(en_vigueur)
    my_path = paste0("data/",en_vigueur,".csv")
    drive_download(file = en_vigueur,
                   path = my_path,
                   overwrite = T,type="csv")
    infos = gsub("_en_vigueur","",en_vigueur)
    ps = strsplit(infos,split = "_")[[1]][1]
    reg = strsplit(infos,split = "_")[[1]][2]
    cbind(fread(my_path,colClasses = c("agr"="character")),reg=reg)
  })
  zonages_en_vigueur = rbindlist(files)
  print("fichier de zonages en vigueur");print(head(zonages_en_vigueur))
  if (nrow(zonages_en_vigueur)>0){
    zonages_en_vigueur = zonages_en_vigueur[reg!=input$choix_reg]
    zonages_en_vigueur[,reg:=as.numeric(reg)]
    setnames(zonages_en_vigueur,"picked_zonage","en_vigueur_autre_reg")
  } else {
    zonages_en_vigueur =data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
  }
} else {
  zonages_en_vigueur =data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
  
}
rm(files)
