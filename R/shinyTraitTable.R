#' Shiny Module UI for Trait Table
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' 
#' @rdname shinyTraitTable
#' @importFrom shiny NS uiOutput
#' @export
#'
shinyTraitTableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::radioButtons(ns("butresp"), "Response",
                      c("value", "cellmean", "signal"),
                      "value", inline = TRUE)
}

#' Shiny Module UI for Trait Object
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' 
#' @rdname shinyTraitTable
#' @importFrom shiny NS 
#' @importFrom DT dataTableOutput
#' @export
#'
shinyTraitTableOutput <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(ns("shiny_traitObject"))
}

#' Shiny Module Server for Trait Object
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param main_par reactive arguments
#' @param trait_names reactive with trait names
#' @param traitData,traitSignal reactive objects from `foundrServer`
#'
#' @return reactive object for `shinyTrait` routines
#' 
#' @importFrom shiny moduleServer radioButtons reactive reactiveVal renderUI req
#' @importFrom DT renderDataTable
#' @export
#'

shinyTraitTable <- function(id, main_par, trait_names, traitData, traitSignal,
                            customSettings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$strains
    # traitObject inputs: (see traitObjectUI)
    #   input$butresp
    
    # RETURNS
    # traitSolosObject()
    
    # Wrap input$butresp
    resp_selection <- shiny::reactiveVal(NULL, label = "resp_selection")
    shiny::observeEvent(input$butresp,
                        resp_selection(input$butresp))

    # traitSolosObject Data Frame
    traitSolosObject <- shiny::reactive({
      traitSolos(shiny::req(traitData()),
                 shiny::req(traitSignal()),
                 shiny::req(trait_names()),
                 shiny::req(resp_selection()),
                 shiny::req(main_par$strains))
    })
    
    # Data Table
    datameans <- shiny::reactive({
      shiny::req(trait_names(), main_par$strains, input$butresp)
      
      summary(traitSolosObject())
    })
    
    output$shiny_traitObject <- DT::renderDataTable(
      {
        shiny::req(trait_names(), datameans())
        datameans()
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    #############################################################
    
    # traitSolosObject Returned
    traitSolosObject
  })
}
