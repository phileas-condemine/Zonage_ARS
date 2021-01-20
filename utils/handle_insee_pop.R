library(data.table)

# 2016 https://www.insee.fr/fr/statistiques/4171341?sommaire=4171351
# 2017 https://www.insee.fr/fr/statistiques/4515539?sommaire=4516122 

annee = "2017"

pop = fread(paste0("data/BTT_TD_POP1B_",annee,".txt"),fill=T)

# pop_femmes = pop[AGED100>=15 & AGED100 <= 49 & SEXE==2,.(population=round(sum(NB))),by="CODGEO"]
pop_femmes = pop[SEXE==2,.(population=round(sum(NB))),by="CODGEO"]
save(pop_femmes,file = paste0("data/pop_femme",annee,".RData"))
rdrop2::drop_upload(file=paste0("data/pop_femme",annee,".RData"),path="zonage/",autorename = F)
rdrop2::drop_upload(file=paste0("data/pop_femme",annee,".RData"),path="zonage_dev/",autorename = F)
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
