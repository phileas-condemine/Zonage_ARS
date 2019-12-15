TVS=readxl::read_xls("data/TVS.xls")
names(TVS) <- c("code_reg","reg","code_dep","code_com","com","code_tvs","tvs")
TVS=data.table(TVS)

div_TVS=TVS[,list(div_reg=uniqueN(reg),
          div_dep=uniqueN(code_dep),
          div_com=uniqueN(com),
          nb_row=.N),
    by="tvs"]
View(div_TVS)