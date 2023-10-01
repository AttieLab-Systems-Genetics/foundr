#' Shiny Module Input for Time Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimePlot
#' @export
#' @importFrom shiny NS
#'
shinyTimePlotInput <- function(id) {
  ns <- shiny::NS(id)
  
  # Trait Table Response.
  shinyTraitTableUI(ns("shinyTable"))
}

#' Shiny Module Output for Time Plots
#'
#' @param id identifier for shiny reactive
#'
#' @return nothing returned
#' @rdname shinyTimePlot
#' @export
#' @importFrom shiny NS uiOutput
#'
shinyTimePlotOutput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("shinyOutput"))
}

#' Shiny Module Server for Time Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitSignal static object
#' @param traitTimesData reactive object
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 moduleServer observeEvent plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom stringr str_remove str_replace_all
#' @export
#'
shinyTimePlot <- function(id, main_par,
                          traitSignal, traitTimesData) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # passed inputs:
    #   main_par$height
    #   main_par$facet
    #   main_par$strains
    
    # OUTPUTS

    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)

    keyTrait <- shiny::reactive({
      shiny::req(traitTimesData())
      
      timetraits_filter(traitSignal, traitTimesData())
    })
    relTraits <- shiny::reactiveVal(NULL)
    
    # Outputs
    output$shinyOutput <- shiny::renderUI({
      shiny::req(main_par$height, statstable(), timeplots(), timestats())
      
      shiny::tagList(
        # Traits or Stats and Downloads.
        shiny::fluidRow(
          shiny::column(4, shiny::radioButtons(ns("buttime"), "",
            c("Plots","Tables"), inline = TRUE)),
          shiny::column(8, shiny::uiOutput(ns("downloads")))),
        
        # Plots and Tables.
        shiny::uiOutput(ns("plotstables")))
    })
    
    output$plotstables <- shiny::renderUI({
      switch(shiny::req(input$buttime),
             Plots  = shiny::uiOutput(ns("plots")),
             Tables = shiny::uiOutput(ns("tables")))
    })

    # Tables.
    statstable <- shiny::reactive({
      shiny::req(traitTimesData())
      
      stats_time_table(traitTimesData()$stats)
    }, label = "statstable")
    traitstable <- shiny::reactive({
      shiny::req(traitTimesData())
      
      # Summary is a bit klugey for now.
      summary_traitTime(traitTimesData())
    }, label = "statstable")
    output$tables <- shiny::renderUI({
      shiny::req(statstable())
      
      shiny::tagList(
        shiny::radioButtons(ns("buttable"), "Download:",
          c("Cell Means","Stats"), "Cell Means", inline = TRUE),
        
        shiny::h3("Cell Means"),
        DT::renderDataTable(traitstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)),
      
        shiny::h3("Stats: p.value"),
        DT::renderDataTable(statstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)))
    })
  
    output$plots <- shiny::renderUI({
      shiny::req(timeplots(), timestats())
      
      shiny::tagList(
        shiny::h3("Plot over Time"),
        shiny::plotOutput(ns("timeplots"),
                          height = paste0(main_par$height, "in")),
        shiny::h3("Plot of Time Summaries"),
        shiny::plotOutput(ns("timestats"),
                          height = paste0(main_par$height, "in")))
    })
    output$timeplots <- shiny::renderPlot(print(timeplots()))
    output$timestats <- shiny::renderPlot(print(timestats()))

    timeplots <- shiny::reactive({
      shiny::req(traitTimesData(), main_par$strains)
      
      ggplot_traitTimes(traitTimesData()$traits, facet_strain = main_par$facet)
    }, label = "timeplots")
    timestats <- shiny::reactive({
      shiny::req(traitTimesData())
      
      ggplot_traitTimes(traitTimesData()$stats)
    }, label = "timestats")

    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(input$buttime)
      shiny::fluidRow(
        shiny::column(3, shiny::downloadButton(
          ns(paste0("download", input$buttime)), input$buttime)),
        shiny::column(9, shiny::uiOutput(ns("filename"))))
    })
    # Download File Prefix
    output$filename <- renderUI({
      shiny::req(traitTimesData())
      
      filename <- "Time_"
      if(shiny::req(input$buttime) == "Tables")
        filename <- paste0(filename, 
                           stringr::str_remove(input$buttable, " "), "_")
      filename <- paste0(filename,
                         paste(names(traitTimesData()$traits), collapse = ","))
      filename <- stringr::str_replace_all(filename, ": ", "_")
      
      shiny::textAreaInput(ns("filename"), "File Prefix", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        shiny::req(timeplots(), timestats(), main_par$height)
        grDevices::pdf(file, width = 9, height = main_par$height)
        print(timeplots())
        print(timestats())
        invisible()
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        shiny::req(traitTimesData())
        utils::write.csv(
          switch(shiny::req(input$buttable),
                 "Cell Means" = traitstable(),
                 Stats        = statstable()),
          file, row.names = FALSE)
      })
  })
}