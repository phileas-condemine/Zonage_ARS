source('https://docs.rstudio.com/rspm/admin/check-user-agent.R')
options(HTTPUserAgent = sprintf('R/%s R (%s)', getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
# https://packagemanager.rstudio.com/client/#/repos/1/overview
  options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))
# remotes::install_deps(dependencies = TRUE)
remotes::install_deps(dependencies = TRUE,repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')
