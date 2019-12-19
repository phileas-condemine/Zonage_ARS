devtools::install_github('oswaldosantos/ggsn')
library(reactlog)
library(shiny)
# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)
runApp(".")
reactlog::reactlog_show()
