library(shiny)
devtools::install_github("byandell/foundr")
library(foundr)

################################################################

ui <- foundr::foundrUI("Founder Study")

server <- function(input, output, session) {
    
  foundr::foundrServer(input, output, session)
  
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}


shiny::shinyApp(ui = ui, server = server)
