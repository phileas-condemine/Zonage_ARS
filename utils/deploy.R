
# to run tests, open the script run_tests.R.

# TODO add test for import_file from custom zonage already prepared in csv/xlsx

# TODO test QPV
# TODO test prep_geo_data_from_scratch for each specific case : 4, 11, and one standard one
# TODO add selenium basic test to see if the app is running in prod. There could be issue with the manifest.txt for example !

#check manifest consistency
manifest = readLines("manifest.txt")
files = list.files(".",full.names = T,recursive = T)
files = gsub("^./","",files)
manifest = gsub("( )*$","",manifest)
manifest[!manifest %in% files]


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
