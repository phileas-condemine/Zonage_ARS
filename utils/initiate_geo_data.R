source("global.R")
# my_reg = regions$reg[1]
my_reg = 6
for (my_reg in setdiff(regions$reg,6)){#Mayotte est exclue !
  print(my_reg)
  reg_name=regions[reg==my_reg]$libreg
  my_deps=dep[reg==my_reg]$dep
  source("utils/handle_geo_data.R")
  prep_geo_data_from_scratch(my_reg)
  
}
