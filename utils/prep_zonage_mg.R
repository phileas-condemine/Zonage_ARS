###### LOAD HIST TVS
# readxl::excel_sheets("data/Zonage_medecin_20191231.xlsx")


filename = params[file=="zonage_mg"]$name
if(!filename%in%list.files("data/")){
  drop_download(paste0(dropbox_folder(),filename),local_path = "data/",overwrite = T)
}

if(my_reg!="4"){
  zonage_historique=readxl::read_xlsx(paste0("data/",filename),
                                      sheet="Zonage_TVS")[,c(2,5,7,8)]
} else if (my_reg=="4"){
  zonage_historique=readxl::read_xlsx(paste0("data/",filename),
                                      sheet="Zonage_TVS")[,c(2,1,7,8)]
}
zonage_historique=data.table(zonage_historique)
names(zonage_historique) <- c("reg","tvs","zonage_nat","zonage_ars")
table(zonage_historique$zonage_ars)
zonage_historique[zonage_ars=="Zone de vigilance",zonage_ars:="ZV"]
zonage_historique[zonage_ars=="Hors vivier",zonage_ars:="HV"]
zonage_historique=zonage_historique%>%
  mutate_if(is.factor,as.character)%>%
  select(reg,tvs,zonage_ars,zonage_nat)%>% 
  data.table %>% 
  unique

if (my_reg=="4"){
  zonage_historique[,tvs:=substr(tvs,7,13)]
}

zonage_historique_reg=zonage_historique[reg==my_reg,c("tvs","zonage_ars","zonage_nat")]
setnames(zonage_historique_reg,"zonage_nat","CN")
zonage_historique_reg=unique(zonage_historique_reg)
# zonage_historique_reg$tvs = stringi::stri_pad_left(zonage_historique_reg$tvs,5,"0")
if (my_reg!="4"){
  zonage_historique_reg$tvs = stringi::stri_pad_right(zonage_historique_reg$tvs,5,"_")
}


CN = zonage_historique_reg[,c("tvs","CN")]%>%unique
VZN = zonage_historique_reg[,c("tvs","zonage_ars")]
VZN$check_historique=T
VZN <<- VZN




prep_zonage <- function(cadre_national=CN,vals_zonage_historique=VZN,vals_qpv_zonage_historique = qpv_VZN,my_dropbox_files,choix_mil,env){
  tvs=data.table(communes_TVS)
  tvs[,"pop_tvs_per_reg":=.(sum(population)),by=c("agr","reg")]
  tvs[,reg:=gsub("^0","",reg)]
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
  
  tvs=merge(tvs,cadre_national,
            by.x=c("agr"),by.y=c("tvs"),all.x=T)    
  
  radio_buttons=expand.grid(agr=tvs$agr,
                            statut=c("ZIP","ZAC","ZV","HV"),stringsAsFactors = F)%>%data.table
  radio_buttons=merge(radio_buttons,
                      tvs[,c("agr","is_majoritaire","CN")],
                      by="agr")
  radio_buttons=merge(radio_buttons,vals_zonage_historique,
                      by.x=c("agr","statut"),by.y=c("tvs","zonage_ars"),all.x=T)
  radio_buttons=data.table(radio_buttons)
  radio_buttons[is.na(check_historique),check_historique:=F]
  
  if(!choix_mil%in%my_dropbox_files$name){
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
      choix_mil = paste0("mg_",input$choix_reg,"_cadre_national.csv")
    }
    drop_name=paste0(dropbox_ps_folder(),choix_mil)
    local_name=paste0("data/",choix_mil)
    fwrite(unique(vals),file=local_name)
    drop_clean_upload(filename = choix_mil,drop_path = dropbox_ps_folder())
    
    assign("vals",vals,env)
  } else {
    req(choix_mil%in%my_dropbox_files$name)
    print("using historical data")
    print(choix_mil)
    zonage_saved <- NULL
    attempt <- 1
    while( is.null(zonage_saved) && attempt <= 5 ) {
      print(paste("try read sheet in dropbox:",attempt))
      attempt <- attempt + 1
      try(
        {
          drop_download(paste0(dropbox_ps_folder(),choix_mil),local_path = "data/",overwrite = T,verbose = T)
          print(list.files("data/"))
          zonage_saved <- fread(paste0("data/",choix_mil),colClasses = "character")%>%as.data.frame()
        }
      )
    } 
    print("zonage_saved") ; print(head(zonage_saved))
    zonage_saved = zonage_saved%>%
      mutate_all(as.character)%>%
      # mutate(agr=stringi::stri_pad_left(agr,5,"0"))
      mutate(agr=stringi::stri_pad_right(agr,5,"_"))

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
      ifelse(value_set," saved_choice' checked='checked",""),
      ifelse(CN=="01_Sélection nationale"&!value_set&!check_historique,
             ifelse(statut=="ZIP",
                    ifelse(my_reg%in%regions_derogatoires," checked='checked'"," checked='checked' disabled='disabled'"),
                    ifelse(my_reg%in%regions_derogatoires,""," disabled='disabled'")),
             ""),
      ifelse(CN=="ZZ_Hors vivier"&statut=="HV"&!value_set&!check_historique," checked='checked'","")
    )]
  }
  
  none_check = radio_buttons[,.(checked=grepl("checked='checked",html)),by="agr"][checked==0]
  if(nrow(none_check)>0){
    showNotification(sprintf("Aucune case n'est cochée pour les TVS suivants : %s. Merci de veiller à renseigner le zonage pour ces zones.",paste(none_check$agr,collapse=", ")),duration = NULL,type = "error",closeButton = T)
  }
  
  radio_buttons=dcast(radio_buttons,agr~statut,value.var="html")
  tvs=merge(tvs,radio_buttons,by="agr")

  setorder(tvs,-proportion_pop)
  print("names tvs")
  print(class(tvs))
  assign("tvs",tvs,env)
  assign("radio_buttons",radio_buttons,env)
}





