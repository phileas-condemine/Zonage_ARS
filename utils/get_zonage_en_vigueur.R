##### AGR ######
# input = list(choix_ps = "sf",choix_reg="24")



dl_zonage_en_vigueur_agr = function(ps,curr_reg){
  my_files = drop_dir(paste0("/zonage/",ps,"/"),dtoken = token)
  my_files = data.table(my_files)
  my_files = my_files[grepl(paste0("^en_vigueur_",ps),name)]
  print("récupération des fichiers de zonage en vigueur")
  if (length(my_files$name)>0){
    files = lapply(my_files$name,function(en_vigueur){
      print(en_vigueur)
      drop_path = paste0("zonage/",ps,"/",en_vigueur)
      local_path = paste0("data/",en_vigueur)
      drop_download(drop_path,local_path = "data/",overwrite = T,dtoken = token,verbose = T)
      infos = gsub("en_vigueur_","",en_vigueur)
      infos = gsub(".csv","",infos)

      ps = strsplit(infos,split = "_")[[1]][1]
      reg = strsplit(infos,split = "_")[[1]][2]
      cbind(fread(local_path,colClasses = c("agr"="character")),reg=reg)
    })
    zonages_en_vigueur = rbindlist(files)
    print("fichier de zonages en vigueur");print(head(zonages_en_vigueur))
    if (nrow(zonages_en_vigueur)>0){
      zonages_en_vigueur = zonages_en_vigueur[reg!=curr_reg]
      zonages_en_vigueur[,reg:=as.numeric(reg)]
      setnames(zonages_en_vigueur,"picked_zonage","en_vigueur_autre_reg")
    } else {
      zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
    }
  } else {
    zonages_en_vigueur = data.table(agr=character(), en_vigueur_autre_reg=character(), reg=numeric())
  }
  # rm(files)
  message("il faudrait vérifier la région majoritaire pour bien choisir celle \"en vigueur\" à sélectionner, pour l'instant on prend arbitrairement la 1ère du fichier.")
  zonages_en_vigueur = zonages_en_vigueur[,.SD[1],by="agr"]
  zonages_en_vigueur
}
