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
#'             tabsetPanel tabPanel
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
        shiny::tabsetPanel(
          type = "tabs", header = "Plots and Tables",
          shiny::tabPanel("Singles & Pairs", shiny::uiOutput("tab_trait")),
          shiny::tabPanel("Correlation", shiny::uiOutput("tab_cor")),
          shiny::tabPanel("Volcano & Effects", shiny::uiOutput("tab_volcano"))
        )
      )
    )
  )
}

#' Server for Founder Shiny App
#'
#' User needs to supply the following routines:
#'   userDatasets() # function returning list of text for intro
#'   
#' @param input,output,session shiny parameters
#' @param traitdata data frame with trait data
#' @param traitstats data frame with summary data
#'
#' @return A Server definition that can be passed to the `shinyServer` function.
#' @export
#' @importFrom shiny checkboxGroupInput checkboxInput column conditionalPanel
#'             downloadButton downloadHandler fluidRow isTruthy observeEvent
#'             plotOutput radioButtons reactive reactiveVal renderPlot renderUI req
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
                         traitstats = NULL,
                         traitsignal = NULL) {

  # INPUT DATA (changes at server call or upload)
  # Trait Data: <datatype>, trait, strain, sex, <condition>, value
  traitDataInput <- shiny::reactive({
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
  # Trait Stats: <datatype>, trait, term, SD, p.value
  traitStatsInput <- shiny::reactive({
    shiny::req(traitDataInput(), datatypes())
    if(shiny::isTruthy(input$upload) | is.null(traitstats)) {
      if(!is.null(traitDataInput()))
        shiny::withProgress(
          message = 'Stats calculations in progress',
          detail = 'This may take a while...',
          value = 0.5,
          { 
            traitstats <- strainstats(traitDataInput())
            shiny::setProgress(
              message = "Done",
              value = 1)
          })
      else
        traitstats <- NULL
    }
    if(!is.null(traitstats)) {
      if(!"datatype" %in% names(traitstats))
        traitstats$datatype <- unique(traitDataInput()$datatype)[1]
    }
    traitstats
  })
  # Trait Signal: <datatype>, strain, sex, <condition>, trait, signal, mean
  traitSignalInput <- shiny::reactive({
    shiny::req(traitDataInput(), datatypes())
    if(shiny::isTruthy(input$upload) | is.null(traitsignal)) {
      if(!is.null(traitDataInput()))
        shiny::withProgress(
          message = 'Signal calculations in progress',
          detail = 'This may take a while...',
          value = 0.5,
          { 
            traitsignal <- partition(traitDataInput())
            shiny::setProgress(
              message = "Done",
              value = 1)
          })
      else
        traitsignal <- NULL
    }
    if(!is.null(traitsignal)) {
      if(!"datatype" %in% names(traitsignal))
        traitsignal$datatype <- unique(traitDataInput()$datatype)[1]
    }
    traitsignal
  })

  # SELECTING SUBSETS OF INPUT DATA by datatype
  # Select Data Types
  datatypes <- shiny::reactive({
    unique(shiny::req(traitDataInput())$datatype)
  })
  # Trait Data from selected datatypes
  traitDataSelectType <- shiny::reactive({
    shiny::req(input$datatype)
    out <- dplyr::filter(
      traitDataInput(),
      datatype %in% input$datatype)
    if("condition" %in% names(out)) {
      if(all(is.na(out$condition)))
        out$condition <- NULL
    }
    out
  })
  # Trait Stats from selected datatypes
  traitStatsSelectType <- shiny::reactive({
    shiny::req(input$datatype)
    dplyr::filter(
      traitStatsInput(),
      datatype %in% input$datatype)
  })
  # Trait Signal from selected datatypes
  traitSignalSelectType <- shiny::reactive({
    shiny::req(input$datatype)
    out <- dplyr::filter(
      traitSignalInput(),
      datatype %in% input$datatype)
    
    if(!nrow(out)) # Happens if traitStatsSelectType changes
      return(NULL)
    
    if("condition" %in% names(out)) {
      if(all(is.na(out$condition)))
        out$condition <- NULL
    }
    out
  })
  
  # Arrange Trait Stats (order traits for menu and summary table)
  traitStatsArranged <- shiny::reactive({
    shiny::req(input$order,
               input$datatype,
               traitStatsSelectType(),
               traitSignalSelectType())
    
    out <- traitStatsSelectType()
    if(is.null(out))
      return(NULL)
    
    if(input$order == "alphabetical") {
      out <- dplyr::arrange(out, trait)
    } else {
      if(input$order == "correlation") {
        out <- traitSignalBestCor()
      } else {
        if(input$order != "original") {
          # Order by p.value for termname
          termname <- stringr::str_remove(input$order, "p_")
          out <- traitOrderStats(traitStatsSelectType(), termname)
        }
      }
    }
    out
  })
  corobject <- reactive({
    term <- shiny::req(input$corterm)
    bestcor(traitSignalSelectType(),
            trait_selection(),
            term)
  })
  traitSignalBestCor <- shiny::reactive({
    out <- traitStatsSelectType()
    if(!shiny::isTruthy(trait_selection()))
      return(out)
    
    o <- c(trait_selection(),
           corobject()$trait)
    dplyr::mutate(
      dplyr::arrange(
        dplyr::mutate(
          out,
          trait = factor(trait, o)),
        trait),
      trait = as.character(trait))
  })
  traitNamesArranged <- shiny::reactive({
    unique(shiny::req(traitStatsArranged())$trait)
  })
  
  # Trait Data for Selected Traits
  traitDataSelectTrait <- shiny::reactive({
    shiny::req(traitDataSelectType(), input$trait, input$strains)
    
    foundrData(
      dplyr::filter(
        traitDataSelectType(),
        trait %in% input$trait,
        strain %in% input$strains),
      input$trait)
  })
  
  # Render UIs for shiny UI
  output$intro <- foundrIntro()
  output$upload <- shiny::renderUI({
    shiny::fileInput("upload", "Upload CSV, XLS, XLSX or RDS:", accept = c(".csv",".xls",".xlsx",".rds"),
                     width = "100%")
  })
  output$datatype <- shiny::renderUI({
    shiny::req(datatypes())
    shiny::selectInput(
      "datatype", "Measurement set",
      datatypes(), datatypes()[1],
      multiple = TRUE)
  })
  output$order <- shiny::renderUI({
    p_types <- paste0("p_", unique(traitStatsSelectType()$term))
    choices <- c(p_types, "alphabetical", "original")
#    if(shiny::isTruthy(input$trait)) # This causes reset.
    choices <- c("correlation", choices)
    shiny::selectInput("order", "Order traits by", choices, p_types[1])
  })

  output$settings <- shiny::renderUI({
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput("datatype")),
      shiny::column(
        4,
        shiny::uiOutput("order")),
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
  
  # Select traits
  output$trait <- shiny::renderUI({
    shiny::req(traitNamesArranged(), input$order, traitDataSelectType())
    shiny::selectizeInput("trait", "Traits:",
                          choices = NULL, multiple = TRUE)
  })
  shiny::observeEvent(
    shiny::req(traitDataSelectType(), traitDataInput(), traitNamesArranged()),
    {
      # Use current selection of input$trait.
      # But make sure they are still in the traitNamesArranged().
      selected <- trait_selection()
      choices <- traitNamesArranged()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- NULL
      shiny::updateSelectizeInput(session, "trait", choices = choices,
                                  server = TRUE, selected = selected)
    })
  # https://stackoverflow.com/questions/28379937/change-selectize-choices-but-retain-previously-selected-values
  # Create reactiveVal and observe changes to input$trait.
  trait_selection <- shiny::reactiveVal(NULL)
  # now store your current selection in the reactive value
  shiny::observeEvent(input$trait, {
    trait_selection(input$trait)
  })
  
  # Output: Plots or Data
  output$tab_trait <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("buttrait", "Singles & Pairs Plots",
                          c("Trait Plots", "Pair Plots"),
                          "Trait Plots", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.buttrait == 'Trait Plots'",
        shiny::uiOutput("plots")),
      shiny::conditionalPanel(
        condition = "input.buttrait == 'Pair Plots'",
        shiny::uiOutput("scatPlot")),
      DT::dataTableOutput("datatable")
    )
  })
  output$tab_cor <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput("corterm", "Correlation Type",
                             c("signal","mean"), "signal")),
        shiny::column(
          6,
          shiny::checkboxInput("abscor", "Absolute Correlation?", TRUE)
        )),
      shiny::sliderInput("mincor", "Minimum", 0.5, 1, 0.7),
      shiny::plotOutput("corplot", height = paste0(input$height, "in")),
      DT::dataTableOutput("cortable"))

  })
  output$corplot <- shiny::renderPlot({
    shiny::req(input$mincor, corobject())
    if(is.null(corobject()) || !nrow(corobject()))
      return(print(ggplot2::ggplot()))
    
    print(ggplot2::autoplot(corobject(), input$mincor, input$abscor))
  })
  output$cortable <- DT::renderDataTable(
    corobject(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  
  output$tab_volcano <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("butvol", "Volcano & Effects Plots",
                          c("Volcano", "Effects"),
                          "Volcano", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.butvol == 'Volcano'",
        shiny::uiOutput("volcano")),
      shiny::conditionalPanel(
        condition = "input.butvol == 'Effects'",
        shiny::plotOutput("effectplot")),
      DT::dataTableOutput("tablesum"))
  })
output$effectplot <- shiny::renderPlot({
  print(effectplot(traitStatsSelectType(), trait_selection()))
})

  # Plots
  distplot <- shiny::reactive({
    if(!shiny::isTruthy(traitDataSelectType()) | !shiny::isTruthy(input$trait)) {
      return(print(ggplot2::ggplot()))
    }
    if(!all(input$trait %in% traitDataSelectType()$trait)) {
      return(print(ggplot2::ggplot()))
    }
    
    print(strainplot(
      traitDataSelectTrait(),
      facet_strain = input$facet,
      boxplot = TRUE))
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  output$plots <- shiny::renderUI({
    shiny::req(input$height)
    shiny::plotOutput("distPlot", height = paste0(input$height, "in"))
  })
  
  termstats <- reactive({
    shiny::req(traitStatsSelectType())
    termStats(traitStatsSelectType())
  }) 
  volcanoplot <- reactive({
    shiny::req(traitStatsSelectType(), input$interact, input$term)
    volcano(traitStatsSelectType(), input$term,
            interact = (input$interact == "yes"))
  })
  output$volcanoly <- plotly::renderPlotly(
    plotly::ggplotly(volcanoplot())
  )
  output$volcanopr <- shiny::renderPlot(
    print(volcanoplot())
  )
  output$volcano <- shiny::renderUI({
    shiny::req(traitDataSelectType())
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput("term", "Volcano term:", termstats(), termstats()[1])),
        shiny::column(
          6,
          shiny::selectInput("interact", "Interactive?", c("no","yes"), "no"))),
      shiny::conditionalPanel(
        condition = "input.interact == 'yes'",
        plotly::plotlyOutput("volcanoly")),
      shiny::conditionalPanel(
        condition = "input.interact == 'no'",
        shiny::plotOutput("volcanopr")))
  })
  
  output$downloads <- foundrDownloads(shiny::req(input$trait), shiny::req(input$datatype))
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      filestub <- shiny::req(input$plotname)
      if(req(input$button) == "Volcano")
        filestub = paste0("Volcano_",
                         paste(req(input$datatype), collapse = ","))
      paste0(filestub, ".pdf") },
    content = function(file) {
      shiny::req(input$height)
      grDevices::pdf(file, width = 9, height = input$height)
      switch(req(input$button),
             Plots = distplot(),
             "Pair Plots" = scatplot(),
             Volcano = print(volcanoplot()),
             ggplot2::ggplot())
      grDevices::dev.off()
    })
  
  # Data Table
  datameans <- shiny::reactive({
    foundrMean(traitDataSelectTrait())
  })
  output$datatable <- DT::renderDataTable(
    datameans(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablesum <- DT::renderDataTable(
    dplyr::mutate(
      traitStatsArranged(),
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
      utils::write.csv(traitStatsArranged(), file, row.names = FALSE)
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
      return(print(ggplot2::ggplot()))
    }
    
    print(foundrScatplot(req(input$trait),
                         traitDataSelectTrait(),
                         req(input$pair)))
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
