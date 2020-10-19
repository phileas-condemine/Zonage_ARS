rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)
# rsconnect::showLogs(streaming=T,appName = "Zonage_ARS_dev",account = "drees")

rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Zonage_ARS",
                     launch.browser = T,
                     account = "drees",forceUpdate = T)
