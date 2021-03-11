

# shinytest::testApp(testnames = "mytest")
testthat::test_file('tests/shinytest/manual_test_dl.R')
testthat::test_file('tests/shinytest/interaction_w_tableau_zonage.R')#remember the ps/reg/agr are picked randomly thus it could be useful to run this several times !
testthat::test_file('tests/shinytest/download_map_zonage_arrete.R')
# TODO add test for import_file from custom zonage already prepared in csv/xlsx

# TODO test QPV
# TODO test prep_geo_data_from_scratch for each specific case : 4, 11, and one standard one
# TODO add selenium basic test to see if the app is running in prod. There could be issue with the manifest.txt for example !

rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)

# TODO testthat::test_file("tests/shinytest/run_in_dev.R") # with basic rvest to see if it runs, then maybe RSelenium ?



# rsconnect::showLogs(streaming=T,appName = "Zonage_ARS_dev",account = "drees")
rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)

# TODO testthat::test_file("tests/shinytest/run_in_prod.R") # with basic rvest

# # fix packages dependencies
# 
# w = warnings()
# w = names(w)
# w = grep("Failed to infer source for package",w,value=T)
# w = gsub("Failed to infer source for package '","",w)
# w = gsub("'; using latest available version on CRAN instead","",w)
# install.packages(w)
