
zonage_historique=readxl::read_xlsx("data/Zonage_medecin_20190703.xlsx",
                                    sheet="Zonage_communes")[,c(2,4,8,9)]
names(zonage_historique) <- c("reg","tvs","zonage_nat","zonage_ars")
zonage_historique$zonage_ars=factor(zonage_historique$zonage_ars)
levels(zonage_historique$zonage_ars) <- c("HV","ZAC","ZIP","ZV")
# zonage_historique$zonage_ars=relevel(zonage_historique$zonage_ars,ref = "ZIP")
zonage_historique=zonage_historique%>%
  mutate_if(is.factor,as.character)%>%
  select(reg,tvs,zonage_ars,zonage_nat)
zonage_historique=data.table(zonage_historique)
zonage_historique=unique(zonage_historique)


zonage_historique_reg=zonage_historique[reg==my_reg,c("tvs","zonage_ars","zonage_nat")]
setnames(zonage_historique_reg,"zonage_nat","CN")
zonage_historique_reg=unique(zonage_historique_reg)
zonage_historique_reg$tvs = stringi::stri_pad_left(zonage_historique_reg$tvs,5,"0")

# zonage_historique_reg$tvs = gsub(" ","", zonage_historique_reg$tvs)
# zonage_historique_reg$tvs = sprintf("%05s", zonage_historique_reg$tvs)
# zonage_historique_reg$tvs = gsub(" ","0", zonage_historique_reg$tvs)

CN = zonage_historique_reg[,c("tvs","CN")]%>%unique
VZN = zonage_historique_reg[,c("tvs","zonage_ars")]
VZN$check_historique=T


prep_zonage <- function(cadre_national=CN,vals_zonage_historique=VZN,my_google_files,choix_mil,env){
  tvs=data.table(communes_TVS)
  tvs[,"pop_tvs_per_reg":=.(sum(population)),by=c("agr","reg")]
  setorder(tvs,-pop_tvs_per_reg)
  
  tvs=tvs[,list(departements=paste(unique(dep),collapse=", "),
                regions=paste(unique(reg),collapse=", "),
                communes=paste(unique(libcom),collapse=", "),
                communes_codes=paste(unique(depcom), collapse=", "),
                # proportion_pop=round(100*sum(population[my_reg_TVS])/sum(population),1),
                # population=sum(population[my_reg_TVS]),
                proportion_pop=round(100*sum(population[reg==my_reg])/sum(population),1),
                population=sum(population[reg==my_reg]),
                nombre_regions=uniqueN(reg),
                reg_majoritaire=reg[1]),
          by=c("agr","libagr")]
  tvs[,is_majoritaire:=reg_majoritaire==my_reg]
  
  # tvs$agr = gsub(" ","", tvs$agr)
  # tvs$agr = sprintf("%05s", tvs$agr)#sous windows le padding de string se fait par des " " et non des "0"
  # tvs$agr = gsub(" ","0", tvs$agr)
  
  tvs=merge(tvs,cadre_national,
            by.x=c("agr"),by.y=c("tvs"),all.x=T)    
  
  radio_buttons=expand.grid(agr=tvs$agr,
                            statut=c("ZIP","ZAC","ZV","HV"),stringsAsFactors = F)%>%data.table
  radio_buttons=merge(radio_buttons,
                      tvs[,c("agr","is_majoritaire","CN")],
                      by="agr")
  
  radio_buttons=merge(radio_buttons,vals_zonage_historique,
                      by.x=c("agr","statut"),by.y=c("tvs","zonage_ars"),all.x=T)
  
  radio_buttons[(check_historique)]
  
  radio_buttons=data.table(radio_buttons)
  radio_buttons[is.na(check_historique),check_historique:=F]
  
  if(!choix_mil%in%my_google_files$name){
    print("from default values")
    radio_buttons[,html:=sprintf(
      "<input type='radio' name='%s' value='%s' %s class='zonage_radio_button%s'%s%s/>",
      agr,
      statut,
      ifelse(is_majoritaire,""," disabled='disabled'"),
      ifelse(check_historique," historical_choice' checked='checked",""),
      ifelse(CN=="01_Sélection nationale"&!check_historique,
             ifelse(statut=="ZIP",ifelse(my_reg%in%regions_derogatoires," checked='checked'",
                                         " checked='checked' disabled='disabled'"),
                    ifelse(my_reg%in%regions_derogatoires,""," disabled='disabled'")),""),
      ifelse(CN=="ZZ_Hors vivier"&statut=="HV"&!check_historique," checked='checked'","")
    )]
    vals=data.table(vals_zonage_historique[,c("tvs","zonage_ars")])
    setnames(vals,c("tvs","zonage_ars"),c("agr","picked_zonage"))
    
    setorder(vals,agr)
    if(choix_mil==""){
      sheet_name=paste0("mg_",input$choix_reg,"_cadre_national")
      gs_file_nm=paste0("data/mg_",input$choix_reg,"_cadre_national.csv")
    } else {
      sheet_name=choix_mil
      gs_file_nm=paste0("data/",choix_mil,".csv")
    }
    fwrite(vals,file=gs_file_nm)
    # gs_upload(gs_file_nm,sheet_title=sheet_name,overwrite = T)
    drive_upload(media = gs_file_nm,path = sheet_name,overwrite = T,  type = "csv")
    
    assign("vals",vals,env)

    
    
  } else {
    req(choix_mil%in%my_google_files$name)
    print("using historical data")
    print(choix_mil)
    zonage_saved <- NULL
    attempt <- 1
    while( is.null(zonage_saved) && attempt <= 5 ) {
      print(paste("try read sheet in gs:",attempt))
      attempt <- attempt + 1
      try(
        {
          drive_download(file = choix_mil,path=paste0("data/",choix_mil,".csv"),overwrite = T,type="csv")
          print(list.files("data/"))
          zonage_saved <- fread(paste0("data/",choix_mil,".csv"),colClasses = "character")%>%as_tibble()
          # zonage_saved <- gs_read(gs_title(choix_mil))
        }
      )
    } 
    print("zonage_saved") ; print(head(zonage_saved))
    zonage_saved = zonage_saved%>%
      mutate_all(as.character)%>%
      mutate(agr=stringi::stri_pad_left(agr,5,"0"))
    # zonage_saved$agr = gsub(" ","", zonage_saved$agr)
    # zonage_saved$agr = sprintf("%05s", zonage_saved$agr)
    # zonage_saved$agr = gsub(" ","0", zonage_saved$agr)
    assign("vals",zonage_saved,env)
    
    zonage_saved$value_set=T

    
    radio_buttons=merge(radio_buttons,zonage_saved,
                        by.x=c("agr","statut"),
                        by.y=c("agr","picked_zonage"),all.x=T)
    radio_buttons=data.table(radio_buttons)
    radio_buttons[is.na(value_set),value_set:=F]
    #Pour différencier le cas où la valeur a déjà été remplie (ancienne valeur) ou non.
    radio_buttons[,value_is_set:=sum(value_set)>0,by="agr"]
    radio_buttons[,html:=sprintf(
      "<input type='radio' name='%s' value='%s' %s class='zonage_radio_button%s%s'%s%s/>",
      agr,
      statut,
      ifelse(is_majoritaire,""," disabled='disabled'"),
      ifelse(check_historique,ifelse(value_is_set," historical_choice",
                                     " historical_choice' checked='checked"),""),
      ifelse(value_set,"saved_choice' checked='checked",""),
      ifelse(CN=="01_Sélection nationale"&!value_set&!check_historique,
             ifelse(statut=="ZIP",
                    ifelse(my_reg%in%regions_derogatoires," checked='checked'"," checked='checked' disabled='disabled'"),
                    ifelse(my_reg%in%regions_derogatoires,""," disabled='disabled'")),
             ""),
      ifelse(CN=="ZZ_Hors vivier"&statut=="HV"&!value_set&!check_historique," checked='checked'","")
    )]
  }
  
  # print("radio_buttons3") ; print(head(radio_buttons))
  radio_buttons=dcast(radio_buttons,agr~statut,value.var="html")
  tvs=merge(tvs,radio_buttons,by="agr")

  # tvs$agr=factor(tvs$agr)
  # tvs$libagr=factor(tvs$libagr)
  
  setorder(tvs,-proportion_pop)
  print("names tvs")
  print(class(tvs))
  assign("tvs",tvs,env)
  assign("radio_buttons",radio_buttons,env)
  
}
