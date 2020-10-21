rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)
# rsconnect::showLogs(streaming=T,appName = "Zonage_ARS_dev",account = "drees")
# rsconnect::deployApp(appFileManifest = "manifest.txt",
#                      appName = "Zonage_ARS",
#                      launch.browser = T,
#                      account = "drees",forceUpdate = T)


# # fix packages dependencies
# 
# w = warnings()
# w = names(w)
# w = grep("Failed to infer source for package",w,value=T)
# w = gsub("Failed to infer source for package '","",w)
# w = gsub("'; using latest available version on CRAN instead","",w)
# install.packages(w)
