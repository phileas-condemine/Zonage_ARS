
##### QPV ######
dl_zonage_en_vigueur_qpv = function(ps,curr_reg){
  if (ps == "mg"){
    my_google_files <- drive_find(type = "csv",q = "name contains 'en_vigueur_qpv'")
    my_google_files <- my_google_files[grepl("^en_vigueur_qpv",my_google_files$name),]
    print("récupération des fichiers de zonage en vigueur")
    if (length(my_google_files$name)>0){
      files = lapply(my_google_files$name,function(en_vigueur){
        print(en_vigueur)
        my_path = paste0("data/",en_vigueur,".csv")
        drive_download(file = en_vigueur,
                       path = my_path,
                       overwrite = T,type="csv")
        # infos = gsub("_en_vigueur","",en_vigueur)
        infos = gsub("en_vigueur_","",en_vigueur)
        ps = strsplit(infos,split = "_")[[1]][1]
        reg = strsplit(infos,split = "_")[[1]][2]
        cbind(fread(my_path,colClasses = c("cod"="character")),reg=reg)
      })
      qpv_zonages_en_vigueur = rbindlist(files)
      print("fichier de zonages en vigueur");print(head(qpv_zonages_en_vigueur))
      if (nrow(qpv_zonages_en_vigueur)>0){
        qpv_zonages_en_vigueur = qpv_zonages_en_vigueur[reg!=curr_reg]
        qpv_zonages_en_vigueur[,reg:=as.numeric(reg)]
        setnames(qpv_zonages_en_vigueur,"picked_zonage","en_vigueur_autre_reg")
      } else {
        qpv_zonages_en_vigueur =data.table(cod=character(), en_vigueur_autre_reg=character(), reg=numeric())
      }
    } else {
      qpv_zonages_en_vigueur =data.table(cod=character(), en_vigueur_autre_reg=character(), reg=numeric())
      
    }
    # rm(files)
    qpv_zonages_en_vigueur
  }
  
}