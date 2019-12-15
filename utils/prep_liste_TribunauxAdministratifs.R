get_TA <- function(){
  download.file("https://gitlab.com/DREES_code/formulaire_zonage_ars/raw/master/data/list_tribunaux_administratifs.txt?inline=false","data/list_tribunaux_administratifs.txt")
  TA=readLines("data/list_tribunaux_administratifs.txt",encoding = "UTF-8")
  TA=TA[TA!=""]
  TA=gsub("Tribunal administratif de ","",TA)
  TA=gsub("\\*","",TA)
  TA=strsplit(TA,split=" : ")
  TA=lapply(TA,function(x){data.table(TA=x[1],departements=x[2])})
  TA=rbindlist(TA)
  TA=tidyr::separate_rows(TA,departements,sep=", ")
  fichier_dep=fread("data/departement2019.csv",encoding = "UTF-8")
  TA=merge(TA,fichier_dep,by.x="departements",by.y="libelle")
  TA=TA[,c("TA","dep","reg")]
  save(TA,file="data/liste_tribunaux_administratifs.RData")
}