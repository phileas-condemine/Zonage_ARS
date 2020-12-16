library(data.table)
#prep fichier histo
histo = haven::read_sas("data/cadre_nat_inf.sas7bdat")[,c(1,2,4)]
names(histo) <- c("reg","agr","zonage_nat")
histo=data.table(histo)
histo[,agr:=stringr::str_pad(agr,5,"right","_")]
histo[,zonage_nat:=substr(zonage_nat,1,1)]


# HDF
my_reg = "32"
hdf = readxl::read_excel("data/zonages_pris_hors_app/inf/zonage_inf_HDF.xlsx",sheet = 1,range=cellranger::cell_rows(c(2,NA)))[,c(4,6)]
names(hdf) <- c("agr","picked_zonage")
hdf = unique(data.table(hdf))
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
naq = readxl::read_excel("data/zonages_pris_hors_app/inf/zonage_inf_NAQ.xlsx",sheet = 3)[,c(5,17)]
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



# GE
my_reg = "44"
ge = readxl::read_excel("data/zonages_pris_hors_app/inf/zonage_inf_GE.xlsx",sheet = 3)[,c(5,17)]
names(ge) <- c("agr","picked_zonage")
ge = data.table(ge)
table(ge$picked_zonage)
ge[,picked_zonage:=substr(picked_zonage,1,1)]
ge[,agr:=stringr::str_pad(agr,5,"right","_")]
ge = merge(histo,ge,by="agr",all=T)
ge = ge[reg==my_reg | !is.na(picked_zonage)]#ma région ou bien zonage pris
ge = ge[zonage_nat!=picked_zonage]# modification par rapport au national
table(ge$reg)
ge


openxlsx::write.xlsx(list(hdf=hdf,naq=naq,ge=ge),
                     "data/zonages_pris_hors_app/inf/diff_zonage_nat_vs_picked_zonage.xlsx")


