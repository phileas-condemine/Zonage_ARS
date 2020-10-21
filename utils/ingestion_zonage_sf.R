library(data.table)
#prep fichier histo
histo = haven::read_sas("data/sf/cadre_nat_sf.sas7bdat")[,c(1,2,4)]
names(histo) <- c("reg","agr","zonage_nat")
histo=data.table(histo)
histo[,agr:=stringr::str_pad(agr,5,"right","_")]
histo[,zonage_nat:=substr(zonage_nat,1,1)]


# PACA
my_reg = "93"
paca = readxl::read_excel("data/zonages_pris_hors_app/sf/zonage_sf_PACA.xlsx",sheet = 1)
names(paca) <- c("agr","libagr","depcom","libcom","zonage_nat","picked_zonage")
paca= data.table(paca);
paca[,agr:=stringr::str_pad(agr,5,"right","_")]
#### coherence zonage par bvcv (decla à la commune)
if(nrow(unique(paca[,.(agr,zonage_nat)])[,.N,by="agr"][N>1])>0){stop("plusieurs valeurs de zonage national au sein d'au moins 1 BVCV")}
if(nrow(unique(paca[,.(agr,picked_zonage)])[,.N,by="agr"][N>1])>0){stop("plusieurs valeurs de zonage ARS au sein d'au moins 1 BVCV")}
#### cohérence des libellés de zonage
table(paca$zonage_nat)
table(paca$picked_zonage)

# reduce to 1 row per bvcv
paca=unique(paca[,.(agr,picked_zonage)])
paca[,picked_zonage:=substr(picked_zonage,1,1)]
paca[,picked_zonage:=as.numeric(picked_zonage)]
paca = paca[picked_zonage%in%1:5]


paca = merge(histo,paca,by="agr",all=T)
paca = paca[reg==my_reg | !is.na(picked_zonage)]#ma région ou bien zonage pris
paca = paca[zonage_nat!=picked_zonage]# modification par rapport au national
table(paca$reg)
paca

# ARA
my_reg = "84"
ara = readxl::read_excel("data/zonages_pris_hors_app/sf/zonage_sf_ARA.xlsx",sheet = 2)[,c(4,6)]
names(ara) <- c("agr","picked_zonage")
ara = data.table(ara)
table(ara$picked_zonage)
ara[,picked_zonage:=substr(picked_zonage,1,1)]
ara[,agr:=stringr::str_pad(agr,5,"right","_")]
ara = merge(histo,ara,by="agr",all=T)
ara = ara[reg==my_reg | !is.na(picked_zonage)]#ma région ou bien zonage pris
ara = ara[zonage_nat!=picked_zonage]# modification par rapport au national
table(ara$reg)
ara


# HDF
my_reg = "32"
hdf = readxl::read_excel("data/zonages_pris_hors_app/sf/zonage_sf_HDF.xlsx",sheet = 3)[,c(5,16)]
names(hdf) <- c("agr","picked_zonage")
hdf = data.table(hdf)
table(hdf$picked_zonage)
hdf[,picked_zonage:=substr(picked_zonage,1,1)]
hdf[,agr:=stringr::str_pad(agr,5,"right","_")]
hdf = merge(histo,hdf,by="agr",all=T)
hdf = hdf[reg==my_reg | !is.na(picked_zonage)]#ma région ou bien zonage pris
hdf = hdf[zonage_nat!=picked_zonage]# modification par rapport au national
table(hdf$reg)
hdf


# NAQ
my_reg = "75"
naq = readxl::read_excel("data/zonages_pris_hors_app/sf/zonage_sf_naq.xlsx",sheet = 3)[,c(5,16)]
names(naq) <- c("agr","picked_zonage")
naq = data.table(naq)
table(naq$picked_zonage)
naq[,picked_zonage:=substr(picked_zonage,1,1)]
naq[,agr:=stringr::str_pad(agr,5,"right","_")]
naq = merge(histo,naq,by="agr",all=T)
naq = naq[reg==my_reg | !is.na(picked_zonage)]#ma région ou bien zonage pris
naq = naq[zonage_nat!=picked_zonage]# modification par rapport au national
table(naq$reg)
naq

openxlsx::write.xlsx(list(paca = paca,hdf=hdf,ara=ara,naq=naq),"data/zonages_pris_hors_app/sf/diff_zonage_nat_vs_picked_zonage.xlsx")
