library(data.table)
library(dplyr)
library(openxlsx)
load("data/27_preprocessed_BVCV.RData")
communes_BVCV = data.table(agr = unique(communes_BVCV$agr))
zonage_historique=sas7bdat::read.sas7bdat(paste0("data/cadre_nat_sf.sas7bdat"))
zonage_historique = zonage_historique%>%select(bvcv,zonage_nat)%>%mutate_if(is.factor,as.character) %>% data.table
my_table = merge(zonage_historique,communes_BVCV,by.x="bvcv",by.y="agr")
setnames(my_table,c("bvcv","zonage_nat"),c("bvcv","zonage"))

fwrite(my_table,file = "data/import/example_data_melt.csv")
openxlsx::write.xlsx(my_table,file="data/import/example_data_melt.xlsx")

my_table$val=1
my_table = dcast(my_table,bvcv~zonage,value.var="val",fill = 0)

fwrite(my_table,file = "data/import/example_data_cast.csv")

openxlsx::write.xlsx(my_table,file="data/import/example_data_cast.xlsx")
