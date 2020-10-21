TVS = haven::read_sas("data/tvs2019.sas7bdat")
# regions = unique(TVS$code_reg)
regions = unique(TVS[,c("code_reg","libreg"),with=F])

for (i in 1:nrow(regions)){
  nb = sample(c(LETTERS,0:9,letters),replace = T,10)
  nb = paste(nb,collapse="")
  code = regions$code_reg[i]
  # email = "phileas.condemine@sante.gouv.fr"
  name = regions$libreg[i]
  # key = openssl::sha256(email,nb)
  write(file = "data/auth.txt",append = T,x = paste0(code,",",name,",",nb))
}
# fread("data/auth.txt",encoding = "UTF-8")
#### ATTENTION NOMS REGIONS PAS UTF8, FIX MANUEL ?

library(rdrop2)
token <- readRDS("droptoken.rds")
rdrop2::drop_upload(file = paste0("data/auth.txt"),mode = "overwrite"
                    # ,dtoken = token
                    ,path = "zonage/")

