#' User Interface for Founder Shiny App
#'
#' See foundrServer for needed user-supplied functions.
#' 
#' @param title title of shiny app
#'
#' @return A UI definition that can be passed to the `shinyUI` function.
#' 
#' @export
#' @importFrom shiny fluidPage mainPanel sidebarLayout sidebarPanel sliderInput titlePanel uiOutput
#'
#' @examples
foundrUI <- function(title) {
  shiny::fluidPage(
    
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("intro"),
        shiny::uiOutput("upload"),
        shiny::uiOutput("settings"),
        shiny::uiOutput("strains"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::uiOutput("downloads"),
        shiny::uiOutput("trait")),
      
      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::uiOutput("outs")
      )
    )
  )
}

#' Server for Founder Shiny App
#'
#' User needs to supply the following routines:
#'   foundrIntro() # introductory remarks about study
#'   foundrScatplot() # wrapper for foundr::scatplot
#'   foundrData() # routine to subset data
#'   foundrMean() # routine to subset mean summaries
#'   
#' @param input,output,session shiny parameters
#' @param traitdata data frame with trait data
#' @param traitpvalue data frame with summary data
#' @param condition column(s) identifying condition for plotting
#'
#' @return A Server definition that can be passed to the `shinyServer` function.
#' @export
#' @importFrom shiny checkboxGroupInput checkboxInput column conditionalPanel
#'             downloadButton downloadHandler fluidRow isTruthy observeEvent
#'             plotOutput radioButtons reactive renderPlot renderUI req
#'             selectInput selectizeInput tagList textInput textAreaInput uiOutput updateSelectizeInput
#'             setProgress withProgress
#' @importFrom dplyr across arrange everything filter mutate
#' @importFrom tidyselect where
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom ggplot2 ggplot
#' @importFrom grDevices pdf dev.off
#' @importFrom utils combn write.csv
#' @importFrom stringr str_detect
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#'
#' @examples
foundrServer <- function(input, output, session,
                         traitdata = NULL,
                         traitpvalue = NULL,
                         condition = "sex_condition") {

  
  traitData <- shiny::reactive({
    if(shiny::isTruthy(input$upload)) {
      file <- input$upload
      datapath <- file$datapath
      traitdata <- switch(
        tools::file_ext(datapath),
        csv = read.csv(datapath),
        xls, xlsx = readxl::read_excel(datapath),
        rds = readRDS(datapath))
    }
    if(!is.null(traitdata)) {
      if(!"datatype" %in% names(traitdata))
        traitdata$datatype <- "uploaded"
      
      if(shiny::isTruthy(input$upload)) {
        # Normal scores with jitter for new data
        traitdata <-
          dplyr::ungroup(
            dplyr::mutate(
              dplyr::group_by(traitdata, datatype, trait),
              value = nqrank(value, jitter = TRUE)))
      }
    }
    traitdata
  })
  datatypes <- shiny::reactive({
    unique(shiny::req(traitData())$datatype)
  })
  
  traitPvalue <- shiny::reactive({
    shiny::req(traitData(), datatypes())
    if(shiny::isTruthy(input$upload) | is.null(traitpvalue)) {
      if(!is.null(traitData()))
        shiny::withProgress(
          message = 'P-value calculation in progress',
          detail = 'This may take a while...',
          value = 0.5,
          { 
            traitpvalue <- broomit(traitData())
            shiny::setProgress(
              message = "Done",
              value = 1)
          })
      else
        traitpvalue <- NULL
    }
    if(!is.null(traitpvalue)) {
      if(!"datatype" %in% names(traitpvalue))
        traitpvalue$datatype <- unique(traitData()$datatype)[1]
    }
    traitpvalue
  })
  cond <- shiny::reactive({condition})
  
  output$intro <- foundrIntro()
  output$upload <- shiny::renderUI({
    shiny::fileInput("upload", "Upload CSV, XLS, XLSX or RDS:", accept = c(".csv",".xls",".xlsx",".rds"),
                     width = "100%")
  })
  output$settings <- shiny::renderUI({
    shiny::req(traitPvalue(), datatypes())
    p_types <- names(traitPvalue())
    p_types <- p_types[stringr::str_detect(p_types, "^p_")]
    
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          "datatype", "Measurement set",
          datatypes(), datatypes()[1],
          multiple = TRUE)),
      shiny::column(
        4,
        shiny::selectInput(
          "order", "Order traits by",
          c(p_types, "variability", "alphabetical", "original"),
          p_types[1])),
      shiny::column(
        4,
        shiny::checkboxInput(
          "facet", "Facet by strain?", FALSE)))
  })
  
  output$strains <- shiny::renderUI({
    choices <- names(CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                       choices = choices, selected = choices, inline = TRUE)
  })
  
  # Trait summaries (for ordering traits, and summary table)
  dataset <- shiny::reactive({
    shiny::req(input$datatype)
    dplyr::filter(traitData(), datatype %in% input$datatype)
  })
  traitarrange <- shiny::reactive({
    shiny::req(input$order, input$datatype, traitPvalue())
    if(is.null(traitPvalue()))
      return(NULL)
    
    out <- dplyr::filter(traitPvalue(), datatype %in% input$datatype)
    if(!nrow(out)) # Happens if traitPvalue changes
      return(NULL)

    if(input$order == "variability") {
      out <- dplyr::arrange(out, dplyr::desc(rawSD))
    } else if(input$order == "alphabetical") {
      out <- dplyr::arrange(out, trait)
    } else {
      if(input$order != "original") {
        out <- dplyr::arrange(out, .data[[input$order]])
      }
    }
    out
    
  })
  traitorder <- shiny::reactive({
    traitarrange()$trait
  })
  
  # Select traits
  output$trait <- shiny::renderUI({
    shiny::req(traitorder(), input$order, dataset())
    shiny::selectizeInput("trait", "Traits:", choices = NULL, multiple = TRUE)
  })
  shiny::observeEvent({
    shiny::req(dataset(), traitData(), traitorder())
  },
  {
    shiny::updateSelectizeInput(session, "trait", choices = traitorder(),
                         server = TRUE)
  })
  
  # Data for selected traits
  datatraitslong <- shiny::reactive({
    shiny::req(dataset(), input$trait, input$strains)
    dplyr::filter(
      dataset(),
      trait %in% input$trait,
      strain %in% input$strains)
  })
  datatraits <- shiny::reactive({
    shiny::req(datatraitslong(), input$trait)
    foundrData(datatraitslong(), input$trait)
  })
  
  # Output: Plots or Data
  output$outs <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("button", "", c("Plots", "Pair Plots", "Data Means", "Data Summary"), "Plots", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.button == 'Plots'",
        shiny::uiOutput("plots")),
      shiny::conditionalPanel(
        condition = "input.button == 'Pair Plots'",
        shiny::uiOutput("scatPlot")),
      shiny::conditionalPanel(
        condition = "input.button == 'Data Means'",
        DT::dataTableOutput("datatable")),
      shiny::conditionalPanel(
        condition = "input.button == 'Data Summary'",
        DT::dataTableOutput("tablesum")))
  })
  
  # Plots
  distplot <- shiny::reactive({
    if(!shiny::isTruthy(dataset()) | !shiny::isTruthy(input$trait)) {
      return(ggplot2::ggplot())
    }
    if(!all(input$trait %in% dataset()$trait)) {
      return(ggplot2::ggplot())
    }
    
    foundr::strainplot(
      datatraits(),
      facet_strain = input$facet,
      condition = cond(),
      boxplot = TRUE)
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  output$plots <- shiny::renderUI({
    shiny::req(input$height)
    shiny::plotOutput("distPlot", height = paste0(input$height, "in"))
  })
  output$downloads <- foundrDownloads(shiny::req(input$trait), shiny::req(input$datatype))
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$plotname), ".pdf") },
    content = function(file) {
      shiny::req(input$height)
      grDevices::pdf(file, width = 9, height = input$height)
      print(distplot())
      grDevices::dev.off()
    })
  
  # Data Table
  datameans <- shiny::reactive({
    foundrMean(datatraits())
  })
  output$datatable <- DT::renderDataTable(
    datameans(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablesum <- DT::renderDataTable(
    dplyr::mutate(
      traitarrange(),
      dplyr::across(
        tidyselect::where(is.numeric),
        function(x) signif(x, 4))),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablename <- shiny::renderUI({
    filename <- shiny::req(input$datatype)
    shiny::textInput("tablename", "Summary File Prefix", filename)
  })
  output$downloadMean <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$plotname), ".csv") },
    content = function(file) {
      utils::write.csv(datameans(), file, row.names = FALSE)
    }
  )
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      shiny::req(input$datatype)
      paste0(shiny::req(input$tablename), ".csv") },
    content = function(file) {
      utils::write.csv(traitarrange(), file, row.names = FALSE)
    }
  )
  
  output$pair <- shiny::renderUI({
    # Somehow when input$height is changed this is reset.
    shiny::req(input$trait)
    if(length(input$trait) < 2)
      return(NULL)
    choices <- traitpairs(input$trait)
    
    shiny::selectInput(
      "pair", "Select pairs for scatterplots",
      choices = choices, selected = choices[1],
      multiple = TRUE, width = '100%')
  })
  output$scatPlot <- shiny::renderUI({
    shiny::req(input$trait, input$datatype, input$order)
    shiny::tagList(
      shiny::uiOutput("pair"),
      shiny::plotOutput("scatplot", height = paste0(input$height, "in"))
    )
  })
  output$scatplot <- shiny::renderPlot({
    if(!shiny::isTruthy(input$pair)) {
      return(ggplot2::ggplot())
    }
    
    foundrScatplot(req(input$trait), datatraitslong(), req(input$pair))
  })
}

#' Downloads for Founder App
#'
#' See `foundrServer` for details of download outputs.
#' 
#' @param trait trait name(s)
#' @param datatype type of data
#'
#' @return uses shiny downloadHandler
#' 
#' @export
#'
#' @examples
foundrDownloads <- function(trait, datatype) {
  shiny::renderUI({
    ltrait <- length(trait)
    filename <- paste0(paste(datatype, collapse = "."),
                       "_",
                       paste(abbreviate(trait, ceiling(60 / ltrait)),
                             collapse = "."))
    
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::textAreaInput("plotname", "File Prefix", filename)),
        shiny::column(
          3,
          shiny::downloadButton("downloadPlot", "Plots")),
        shiny::column(
          3,
          shiny::downloadButton("downloadMean", "Means"))),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::uiOutput("tablename")),
        shiny::column(
          3,
          shiny::downloadButton("downloadTable", "Summary"))))
  })
}
