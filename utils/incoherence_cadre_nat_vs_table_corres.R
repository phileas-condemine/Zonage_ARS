library(data.table)
ide = haven::read_sas("data/cadre_nat_inf.sas7bdat")
ide = data.table(ide)
corres = fread("data/bvcv2020.csv")
corres = corres[,.SD[1],by="bvcv"]

corres[,bvcv:=stringr::str_pad(bvcv,5,"right","_")]
ide[,bvcv:=stringr::str_pad(bvcv,5,"right","_")]
dplyr::anti_join(corres,ide,by="bvcv")
# bvcv reg dep depcom                libcom              libbvcv
# 1: 0708_  84  07  07069    Colombier-le-Vieux        Haut-Vivarais
# 2: 0714_  84  07  07063              Cheminas    Tournon-sur-Rhône
# 3: 18279  24  18  18036                Brinay              Vierzon
# 4: 3902_  27  39  39008                Amange              Authume
# 5: 3905_  27  39  39101             Champvans               Dole-1
# 6: 3906_  27  39  39150               Choisey               Dole-2
# 7: 3910_  27  39  39026              Augerans    Mont-sous-Vaudrey
# 8: 3998_  27  39  39198                  Dole                 Dole
# 9: 51230  44  51  51117 Champigneul-Champagne              Épernay
# 10: 7489_  84  74  74212  Glières-Val-de-Borne Glières-Val-de-Borne
# 11: 2785_  28  27  27089           Thénouville          Thénouville

dplyr::anti_join(ide,corres,by="bvcv")
# reg  bvcv apl_2017_bvcv        zonage_nat ZE_UD ZE_OD
# 1:  24 1899_      93.51401 3-Zone intermédia     0     0
# 2:  27 39198     122.37230 4-Zone très dotée     0     0
# 3:  44 5107_     112.00507 3-Zone intermédia     0     0
# 4:  44 5108_      84.30254 3-Zone intermédia     0     0
# 5:  44 5198_     121.45002 4-Zone très dotée     0     0
# 6:  84 07324     137.05254 4-Zone très dotée     0     0
# 7:  24 1819_      55.19662  1-Zone très sous     1     0
# 8:  24 3613_      41.69437  1-Zone très sous     1     0
# 9:  24 4112_      57.17997  1-Zone très sous     1     0
# 10:  44 5122_      66.31563 2-Zone sous dotée     1     0

