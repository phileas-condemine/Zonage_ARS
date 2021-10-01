library(readxl)
library(data.table)
readxl::excel_sheets("data/Zonage_medecin_20191231.xlsx")
zonage = setDT(read_excel("data/Zonage_medecin_20191231.xlsx",sheet = "Zonage_TVS"))
tvs_com = fread("data/tvs2017_com2017.csv")

code_tvs_zonage = zonage[,.(`Code du territoire de vie-santé`)]
names(code_tvs_zonage) <- "tvs"


tvs_com[!tv%in%code_tvs_zonage$tvs]

