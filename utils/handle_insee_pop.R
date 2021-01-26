library(data.table)
# https://www.insee.fr/fr/statistiques/4171341?sommaire=4171351
pop = fread("data/BTT_TD_POP1B_2016.txt",fill=T)

# pop_femmes = pop[AGED100>=15 & AGED100 <= 49 & SEXE==2,.(population=round(sum(NB))),by="CODGEO"]
pop_femmes = pop[SEXE==2,.(population=round(sum(NB))),by="CODGEO"]
save(pop_femmes,file = "data/pop_femme2016.RData")
rdrop2::drop_upload(file="data/pop_femme2016.RData",path="zonage/",autorename = F)
rdrop2::drop_upload(file="data/pop_femme2016.RData",path="zonage_dev/",autorename = F)
# table(pop$NIVGEO)
# pop[,sum(NB),by=SEXE]
# pop[,sum(NB),by=AGED100]
# 
# min_age = 12
# max_age = 46
# stat_age = pop[,.(
#   NB1=sum(NB[AGED100>=12 & AGED100 <= 46 & SEXE==2]),
#   NB2=sum(NB[AGED100>=15 & AGED100 <= 49 & SEXE==2])
# ),by=.(CODGEO,NIVGEO,LIBGEO)]
# 
# 
# stat_age[,diff:=abs(NB1-NB2)*2/(NB1+NB2)]
# stat_age = stat_age[NB1>0 | NB2>0]
# setorder(stat_age,-diff)
# head(stat_age[NB1 + NB2 > 1000],10)


pop_plm = pop[substr(CODGEO,1,3)%in%c("751","132","693"),.(population=round(sum(NB))),by="CODGEO"]
setnames(pop_plm,"CODGEO","depcom")
pop_plm$tvs=pop_plm$depcom
save(pop_plm,file = paste0("data/pop_plm",annee,".RData"))
rdrop2::drop_upload(file=paste0("data/pop_plm",annee,".RData"),path="zonage/",autorename = F)
rdrop2::drop_upload(file=paste0("data/pop_plm",annee,".RData"),path="zonage_dev/",autorename = F)
