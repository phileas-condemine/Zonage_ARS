library(data.table)
library(dplyr)
library(readxl)
library(plotly)
zonage_historique=readxl::read_xlsx("data/Zonage_medecin_20190703.xlsx",
                                    sheet="Zonage_communes")[,c(2,4,8,9,10)]
names(zonage_historique) <- c("reg","tvs","zonage_nat","zonage_ars","population")
zonage_historique$zonage_ars=factor(zonage_historique$zonage_ars)
levels(zonage_historique$zonage_ars) <- c("HV","ZAC","ZIP","ZV")
# zonage_historique$zonage_ars=relevel(zonage_historique$zonage_ars,ref = "ZIP")
zonage_historique=zonage_historique%>%
  mutate_if(is.factor,as.character)%>%
  select(reg,tvs,zonage_ars,zonage_nat,population)%>%
  # mutate(tvs=as.character(tvs),tvs=stringi::stri_pad_left(tvs,5,"0"))
  mutate(tvs=as.character(tvs),tvs=stringi::stri_pad_right(tvs,5,"_"))
zonage_historique=data.table(zonage_historique)
zonage_historique=unique(zonage_historique)

seuils_reg_mg = read_xlsx("data/Seuils_arretes.xlsx",sheet="mg")
seuils_reg_mg = data.table(seuils_reg_mg)
seuils_reg_mg = melt(seuils_reg_mg,id.vars=c("reg","libreg"))
seuils_reg_mg = seuils_reg_mg[variable%in%c("maxZIP","maxZAC")]
seuils_reg_mg[variable == "maxZIP"]$variable = "ZIP"
seuils_reg_mg[variable == "maxZAC"]$variable = "ZAC"
setnames(seuils_reg_mg,c("variable","value"),c("zonage","seuil"))
seuils_reg_mg[,seuil:=as.numeric(seuil)]
# files = list.files("data")
# files = files[grepl("preprocessed_TVS",files)]
# file=sample(files,1)
# for (file in files){
#   my_reg = stringr::str_extract(file,"[0-9]+")
#   print(my_reg)
#   load(paste0("data/",file))
#   pop = communes_TVS%>%mutate(agr=as.character(agr),agr=stringi::stri_pad_left(agr,5,"0"))%>%
#     data.table%>%.[,c("reg","agr","population")]
#   pop = pop[reg==my_reg,.(population=sum(population,na.rm=T)),by="agr"]
#   setnames(pop,'agr','tvs')
#   zonage_historique[pop,on='tvs',population:=i.population]
# }

zonage_historique[,sum(is.na(population)),by="reg"]

zonage_reg = zonage_historique[,.(population=sum(population,na.rm=T)),by=c("reg","zonage_ars")]
zonage_reg[,pop_tot:=sum(population,na.rm=T),by="reg"]
zonage_reg[,pop_prop:=population/pop_tot]

comparaison = merge(zonage_reg[,c("reg","zonage_ars","pop_prop")],seuils_reg_mg,
                    by.x=c("reg","zonage_ars"),by.y=c("reg","zonage"))
g <- ggplot(data=comparaison,aes(x=pop_prop,y=seuil,color=factor(libreg),shape=zonage_ars))+geom_point()+
  geom_abline(slope=100,intercept=0)
ggplotly(g)

# 
# 
#   add_bars(data=comparaison,x=~libreg,y=~pop_prop,color=~zonage_ars)%>%
#   add_bars(data=comparaison,x=~libreg,y=~value,color=~zonage_ars)%>%
#   layout(barmode = 'stack')
