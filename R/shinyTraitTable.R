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
#' @param trait_par,main_par reactive arguments
#' @param keyTrait,relTraits reactives with trait names
#' @param traitData,traitSignal static objects 
#'
#' @return reactive object for `shinyTrait` routines
#' 
#' @importFrom shiny moduleServer radioButtons reactive reactiveVal renderUI req
#' @importFrom DT renderDataTable
#' @export
#'

shinyTraitTable <- function(id, trait_par, main_par, keyTrait, relTraits,
                            traitData, traitSignal,
                            customSettings = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$strains
    #   trait_par$reldataset
    # traitObject inputs: (see traitObjectUI)
    #   input$butresp
    
    # RETURNS
    # traitSolosObject()
    
    # Wrap input$butresp
    resp_selection <- shiny::reactiveVal(NULL, label = "resp_selection")
    shiny::observeEvent(input$butresp,
                        resp_selection(input$butresp))
    
    # Filter static traitData based on selected trait_names.
    keyData <- shiny::reactive({
      shiny::req(keyTrait())
      
      subset_trait_names(traitData, keyTrait())
    })
    relData <- shiny::reactive({
      shiny::req(trait_par$reldataset)
      
      subset_trait_names(traitData, relTraits())
    })
    traitDataInput <- shiny::reactive({
      out <- shiny::req(keyData())
      
      if(shiny::isTruthy(trait_par$reldataset)) {
        out <- dplyr::bind_rows(out, relData())
      }
      out
    })
    
    # traitSolosObject Data Frame
    traitSolosObject <- shiny::reactive({
      traitSolos(shiny::req(traitDataInput()),
                 traitSignal,
                 shiny::req(trait_names()),
                 shiny::req(resp_selection()),
                 shiny::req(main_par$strains))
    })
    
    trait_names <- shiny::reactive({
      c(shiny::req(keyTrait()), relTraits())
    })

    # Data Table
    datameans <- shiny::reactive({
      shiny::req(traitSolosObject())
      
      summary(traitSolosObject())
    })
    
    output$shiny_traitObject <- DT::renderDataTable(
      shiny::req(datameans()),
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    #############################################################
    
    # traitSolosObject Returned
    traitSolosObject
  })
}
