library(shiny)

ui <- fluidPage(
  tags$style(
    "

          @keyframes pulse {
  0% {
    transform: scale(0);
    opacity: 0;
  }
  33% {
    transform: scale(1);
    opacity: 1;
  }
  100% {
    transform: scale(3);
    opacity: 0;
  }
}
.pulse {
  position: relative;
}
.pulse:before, .pulse:after {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(100, 100, 255, 0.4);
  border-radius: 50%;
  width: 80px;
  height: 20px;
  opacity: 0;
  margin: auto;
}
.pulse:before {
  animation: pulse 1.5s infinite linear;
}
.pulse:after {
  animation: pulse 2s .4s infinite linear;
}
.pulse:hover:before, .pulse:hover:after {
  display: none;
}
"
  ),
  
    actionButton("go1",class="pulse", "Simulate long process 1")

  
)

server <- function(input, output, session) {
}

shinyApp(ui, server)