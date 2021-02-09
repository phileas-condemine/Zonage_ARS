# shinytest::testApp(testnames = "mytest")
testthat::test_file("tests/shinytest/manual_test_dl.R")
testthat::test_file("tests/shinytest/interaction_w_tableau_zonage.R")#remember the ps/reg/agr are picked randomly thus it could be useful to run this several times !
testthat::test_file("tests/shinytest/download_map_zonage_arrete.R")
# add test for import_file from custom zonage already prepared in csv/xlsx

test QPV

rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)

# add selenium basic test to see if the app is running in prod. There could be issue with the manifest.txt for example !
testthat::test_file("tests/shinytest/run_in_dev.R")

test prep_geo_data_from_scratch for each specific case : 4, 11, and one standard one


# rsconnect::showLogs(streaming=T,appName = "Zonage_ARS_dev",account = "drees")
rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)

testthat::test_file("tests/shinytest/run_in_prod.R")

# # fix packages dependencies
# 
# w = warnings()
# w = names(w)
# w = grep("Failed to infer source for package",w,value=T)
# w = gsub("Failed to infer source for package '","",w)
# w = gsub("'; using latest available version on CRAN instead","",w)
# install.packages(w)
