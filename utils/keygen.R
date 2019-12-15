TVS = haven::read_sas("data/tvs2019.sas7bdat")
regions = unique(TVS$code_reg)
nb = sample(LETTERS,replace = T,20)
nb = paste(nb,collapse="")

for (i in 1:length(regions)){
  reg = regions[i]
  email = "phileas.condemine@sante.gouv.fr"
  key = openssl::sha256(email,nb)
  write(file = "data/auth.txt",append = T,x = paste0(regions,",",email,",",key))
}
library(rdrop2)
token <- readRDS("droptoken.rds")
rdrop2::drop_upload(file = paste0("data/auth.txt"),mode = "overwrite",
                    dtoken = token,path = "zonage/")
