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
                      c("value", "cellmean"),
                      "value", inline = TRUE)
}

#' Shiny Module UI for Trait Object
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' 
#' @rdname shinyTraitTable
#' @importFrom shiny h3 NS tagList 
#' @importFrom DT dataTableOutput
#' @export
#'
shinyTraitTableOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Cell Means"),
    DT::dataTableOutput(ns("shiny_traitObject")))
}

#' Shiny Module Server for Trait Object
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments
#' @param keyTrait,relTraits reactives with trait names
#' @param traitData,traitSignal static objects 
#'
#' @return reactive object for `shinyTrait` routines
#' 
#' @importFrom shiny moduleServer radioButtons reactive reactiveVal renderUI req
#' @importFrom DT renderDataTable
#' @export
#'

shinyTraitTable <- function(id, panel_par, main_par, keyTrait, relTraits,
                            traitData, traitSignal,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$strains
    #   panel_par$reldataset
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
      shiny::req(panel_par$reldataset)
      
      subset_trait_names(traitData, relTraits())
    })
    traitDataInput <- shiny::reactive({
      out <- shiny::req(keyData())
      
      if(shiny::isTruthy(panel_par$reldataset)) {
        out <- dplyr::bind_rows(out, relData())
      }
      out
    }, label = "traitDataInput")
    
    # traitSolosObject Data Frame
    traitSolosObject <- shiny::reactive({
      traitSolos(shiny::req(traitDataInput()),
                 traitSignal,
                 shiny::req(trait_names()),
                 shiny::req(resp_selection()),
                 shiny::req(main_par$strains))
    }, label = "traitSolosObject")
    
    trait_names <- shiny::reactive({
      c(shiny::req(keyTrait()), relTraits())
    }, label = "trait_names")

    # Data Table
    datameans <- shiny::reactive({
      shiny::req(traitSolosObject())
      
      summary(traitSolosObject(), customSettings)
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
