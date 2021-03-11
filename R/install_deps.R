source('https://docs.rstudio.com/rspm/admin/check-user-agent.R')
options(HTTPUserAgent = sprintf('R/%s R (%s)', getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
remotes::install_deps(dependencies = TRUE,repos='https://packagemanager.rstudio.com/all/latest')
