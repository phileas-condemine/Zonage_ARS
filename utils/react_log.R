
# library(reactlog)
library(shiny)
# tell shiny to log all reactivity
# options(shiny.reactlog = TRUE)
# runApp(".")
# reactlog::reactlog_show()
options(shiny.error = browser)
shiny::runApp(display.mode="showcase")
