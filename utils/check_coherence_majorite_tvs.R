pop_tvs_reg=data.table(communes)[,.(pop=sum(population)),by=.(code_reg,code_tvs)]
pop_tvs_reg[,transregional:=.N,by=code_tvs]
pop_tvs_reg[,"pop_tvs":=list(sum(pop)),by="code_tvs"]
pop_tvs_reg[,ratio_pop:=pop/pop_tvs]

setorder(pop_tvs_reg,-ratio_pop)
pop_tvs_reg[,"rank":=1:.N,by=code_tvs]
pop_tvs_reg[,dep_tvs_maj:=substr(code_tvs,1,2)]
dep

pop_tvs_reg=merge(pop_tvs_reg,dep[,c("dep","reg")],by.x=c("dep_tvs_maj"),by.y=c("dep"))
pop_tvs_reg
setnames(pop_tvs_reg,"reg","code_reg_majoritaire")

pop_tvs_reg[rank==1&code_reg!=code_reg_majoritaire]



population = readxl::read_excel("data/Données_géo_pop/pop2016_geo2018.xls",sheet = "Communes",skip = 7)
population = data.table(population)
population[,code_com := paste0(`Code département`,`Code commune`)]
population = merge(population,TVS,by="code_com",all.x=T)
setnames(population,c("Population municipale","Code région"),c("population","code_reg"))

pop_tvs_reg=data.table(population)[,.(pop=sum(population)),by=.(code_reg,code_tvs)]
pop_tvs_reg[,transregional:=.N,by=code_tvs]
pop_tvs_reg[,"pop_tvs":=list(sum(pop)),by="code_tvs"]
pop_tvs_reg[,ratio_pop:=pop/pop_tvs]

setorder(pop_tvs_reg,-ratio_pop)
pop_tvs_reg[,"rank":=1:.N,by=code_tvs]
pop_tvs_reg[,dep_tvs_maj:=substr(code_tvs,1,2)]
dep

pop_tvs_reg=merge(pop_tvs_reg,dep[,c("dep","reg")],by.x=c("dep_tvs_maj"),by.y=c("dep"))
pop_tvs_reg
setnames(pop_tvs_reg,"reg","code_reg_majoritaire")

pop_tvs_reg[rank==1&code_reg!=code_reg_majoritaire]
