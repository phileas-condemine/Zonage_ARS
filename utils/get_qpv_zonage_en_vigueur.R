
##### QPV ######
dl_zonage_en_vigueur_qpv = function(ps,curr_reg){
  if (ps == "mg"){
    
    my_files = drop_dir("zonage/mg/"
                        # ,dtoken = token
                        )
    my_files = data.table(my_files)
    my_files = my_files[grepl("^en_vigueur_qpv",name)]
    print("récupération des fichiers de zonage en vigueur")
    if (length(my_files$name)>0){
      files = lapply(my_files$name,function(en_vigueur){
        print(en_vigueur)
        drop_path = paste0("zonage/mg/",en_vigueur)
        local_path = paste0("data/",en_vigueur)
        drop_download(drop_path,local_path = "data/",overwrite = T
                      # ,dtoken = token,verbose = T
                      )
        infos = gsub("en_vigueur_","",en_vigueur)
        infos = gsub(".csv","",infos)
        infos = gsub("qpv_","",infos)
        ps = strsplit(infos,split = "_")[[1]][1]
        reg = strsplit(infos,split = "_")[[1]][2]
        cbind(fread(local_path,colClasses = c("cod"="character")),reg=reg)
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