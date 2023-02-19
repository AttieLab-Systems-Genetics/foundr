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
#'             tabsetPanel tabPanel includeMarkdown tags includeMarkdown
#'
#' @examples
foundrUI <- function(title) {
  shiny::fluidPage(
    
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("upload"),
        shiny::uiOutput("settings"),
        shiny::uiOutput("strains"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::uiOutput("downloads"),
        shiny::uiOutput("trait")),
      
      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs", header = "", id = "tabpanel",
          shiny::tabPanel("Traits", shiny::uiOutput("tab_trait")),
          shiny::tabPanel("Correlation", shiny::uiOutput("tab_cor")),
          shiny::tabPanel("Volcano", shiny::uiOutput("tab_volcano")),
          shiny::tabPanel("About", shiny::uiOutput("intro"))
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
#' @param customSettings list of custom settings
#' @param condition_name name of condition
#' 
#' @description 
#' The `customSettings` is a list that may include
#'   `help` = name of help markdown file (e.g. `"help.md"`)
#'   `condition` = names of condition (e.g. `"diet"`) 
#'   `datatype` = names of datatypes (e.g. `uploaded = "MyUpload"`)
#'
#' @return A Server definition that can be passed to the `shinyServer` function.
#' @export
#' @importFrom shiny a br checkboxGroupInput checkboxInput column conditionalPanel
#'             downloadButton downloadHandler fluidRow isTruthy observeEvent
#'             plotOutput radioButtons reactive reactiveVal renderPlot renderUI req
#'             selectInput selectizeInput tagList textInput textAreaInput uiOutput updateSelectizeInput
#'             setProgress withProgress
#' @importFrom dplyr across arrange everything filter mutate
#' @importFrom tidyselect where
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom ggplot2 ggplot
#' @importFrom grDevices pdf dev.off
#' @importFrom utils combn packageVersion write.csv
#' @importFrom stringr str_detect
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#'
#' @examples
foundrServer <- function(input, output, session,
                         traitdata = NULL,
                         traitstats = NULL,
                         traitsignal = NULL,
                         customSettings = NULL,
                         condition_name = "condition") {
  
  # Temporary kludge
  if(!is.list(customSettings)) {
    customSettings <- list(
      help = customSettings,
      condition = condition_name)
  }
  if(is.null(customSettings$condition))
    customSettings$condition <- "condition"
  if(is.null(customSettings$datatype$uploaded)) {
    if(is.null(customSettings$datatype))
      customSettings$datatype <- list()
    customSettings$datatype$uploaded <- "Uploaded"
  }
  
  # INPUT DATA (changes at server call or upload)
  # Trait Data: <datatype>, trait, strain, sex, <condition>, value
  newtraitdata <- reactive({
    if(shiny::isTruthy(input$upload)) {
      newTraitData(input$upload$datapath,
                   customSettings$condition,
                   customSettings$datatype$uploaded)
    } else {
      NULL
    }
  })
  
  traitDataInput <- shiny::reactive({
    newdata <- newtraitdata()
    if(!is.null(newdata)) {
      if(is.null(traitdata)) {
        traitdata <- newdata
      } else {
        # Append new data
        trnames <- names(traitdata)
        newtrnames <- names(newtraitdata())
        keepcol <- match(newtrnames, trnames, nomatch = 0)
        if(any(keepcol == 0)) {
          # Drop unused columns (see verifyColumns for column handling)
          newtrnames[newtrnames[keepcol == 0]] <- NULL
        }
        traitdata <- dplyr::bind_rows(
          traitdata,
          newtraitdata()[trnames[keepcol]])
      }
    }
    rename_datatypes(traitdata, customSettings$datatype)
  })
  # Trait Stats: <datatype>, trait, term, SD, p.value
  traitStatsInput <- shiny::reactive({
    shiny::req(traitDataInput(), datatypes())
    
    # Create stats for new data
    if(!is.null(newtraitdata())) {
      newtraitstats <- progress(newtraitdata(), strainstats, "Stats")
      if(!is.null(newtraitstats)) {
        traitstats <- dplyr::bind_rows(
          traitstats,
          newtraitstats)
      }
    }
    if(!is.null(traitstats)) {
      if(!"datatype" %in% names(traitstats))
        traitstats$datatype <- unique(traitDataInput()$datatype)[1]
    }
    rename_datatypes(traitstats, customSettings$datatype)
  })
  # Trait Signal: <datatype>, strain, sex, <condition>, trait, signal, mean
  traitSignalInput <- shiny::reactive({
    shiny::req(traitDataInput(), datatypes())
    
    # Create signal for new data
    if(!is.null(newtraitdata())) {
      newtraitsignal <- progress(newtraitdata(), partition, "Signal")
      if(!is.null(newtraitsignal)) {
        traitsignal <- dplyr::bind_rows(
          traitsignal,
          newtraitsignal)
      }
    }
    rename_datatypes(traitsignal, customSettings$datatype)
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
    bestcor(traitSignalSelectType(),
            trait_selection(),
            shiny::req(input$corterm))
  })
  traitSignalBestCor <- shiny::reactive({
    bestcorStats(traitStatsSelectType(),
                 c(trait_selection(), corobject()$trait))
  })
  traitNamesArranged <- shiny::reactive({
    unique(shiny::req(traitStatsArranged())$trait)
  })
  
  # Trait Data for Selected Traits
  traitDataSelectTrait <- shiny::reactive({
    selectTrait(shiny::req(traitDataSelectType()),
                shiny::req(traitSignalSelectType()),
                shiny::req(input$trait),
                shiny::req(input$strains),
                shiny::req(input$butresp))
  })
  
  # Render UIs for shiny UI
  output$intro <- foundrIntro(customSettings$help)
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
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::radioButtons("buttrait", "Single & Pair Plots",
                              c("Trait Plots", "Pair Plots"),
                              "Trait Plots", inline = TRUE)),
        shiny::column(
          6,
          shiny::radioButtons("butresp", "Response",
                              c("individual", "mean", "signal", "ind_signal"),
                              "individual", inline = TRUE))),
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
  corplot <- shiny::reactive({
    ggplot2::autoplot(corobject(), input$mincor, input$abscor)
  })
  output$corplot <- shiny::renderPlot({
    shiny::req(input$mincor, corobject())
    if(is.null(corobject()) || !nrow(corobject()))
      return(print(ggplot2::ggplot()))
    
    print(corplot())
  })
  output$cortable <- DT::renderDataTable(
    dplyr::mutate(
      corobject(),
      dplyr::across(
        tidyselect::where(is.numeric),
        function(x) signif(x, 4))),
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
        shiny::plotOutput("effects")),
      DT::dataTableOutput("tablesum"))
  })
  effectsplot <- shiny::reactive({
    print(effectplot(traitStatsSelectType(), trait_selection()))
  })
  output$effects <- shiny::renderPlot({
    effectsplot()
  })

  # Plots
  distplot <- shiny::reactive({
    if(!shiny::isTruthy(traitDataSelectType()) | !shiny::isTruthy(input$trait)) {
      return(ggplot2::ggplot())
    }
    if(!all(input$trait %in% traitDataSelectType()$trait)) {
      return(ggplot2::ggplot())
    }
    
    strainplot(
      traitDataSelectTrait(),
      facet_strain = input$facet,
      boxplot = TRUE)
  })
  output$distPlot <- shiny::renderPlot({
    print(distplot())
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
  
  output$filename <- renderUI({
    filename <- paste(shiny::req(input$datatype), collapse = ".")
    if(shiny::isTruthy(input$trait)) {
      ltrait <- length(input$trait)
      if(shiny::req(input$tabpanel) != "Volcano") {
        filename <- paste0(filename,
                           "_",
                           paste(abbreviate(input$trait, ceiling(60 / ltrait)),
                                 collapse = "."))
      }
    }
    shiny::textAreaInput("filename", "File Prefix", filename)
  })
  output$downloads <- renderUI({
    shiny::req(input$trait)
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::uiOutput("filename")),
        shiny::column(
          3,
          shiny::downloadButton("downloadPlot", "Plots")),
        shiny::column(
          3,
          shiny::downloadButton("downloadTable", "Data"))))
  })
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      fname <- paste0(shiny::req(input$filename), ".pdf")
      switch(
        shiny::req(input$tabpanel),
        Traits = {
          switch(input$buttrait,
                 "Trait Plots" = {
                   fname <- paste0("trait_", fname)
                 },
                 "Pair Plots" = {
                   fname <- paste0("pair_", fname)
                 })
        },
        Correlation = {
          fname <- paste0("cor_", fname)
        },
        Volcano = {
          switch(input$butvol,
                 Volcano = {
                   fname <- paste0("volcano_", fname)
                 },
                 Effects = {
                   fname <- paste0("effect_", fname)
                 })
        })
      fname
    },
    content = function(file) {
      shiny::req(input$height)
      grDevices::pdf(file, width = 9, height = input$height)
      switch(
        shiny::req(input$tabpanel),
        Traits = {
          switch(input$buttrait,
                 "Trait Plots" = print(distplot()),
                 "Pair Plots" = print(scatsplot()))
        },
        Correlation = {
          print(corplot())
        },
        Volcano = {
          switch(input$butvol,
                 Volcano = print(volcanoplot()),
                 Effects = print(effectsplot()))
        },
        ggplot2::ggplot())
      grDevices::dev.off()
    })
  
  # Data Table
  datameans <- shiny::reactive({
    response <- shiny::req(input$butresp)
    if(response == "individual")
      response <- "mean"
    if(response == "ind_signal")
      response <- "signal"
    selectSignalWide(traitSignalSelectType(),
                 shiny::req(input$trait),
                 shiny::req(input$strains),
                 response, customSettings$condition)
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
  
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      fname <- paste0(shiny::req(input$filename), ".csv")
      switch(
        shiny::req(input$tabpanel),
        Traits = {
          fname <- paste0("trait_", fname)
        },
        Correlation = {
          fname <- paste0("cor_", fname)
        },
        Volcano = {
          fname <- paste0("stats_", fname)
        })
      fname
    },
    content = function(file) {
      utils::write.csv(
        switch(
          shiny::req(input$tabpanel),
          Traits = datameans(),
          Correlation = corobject(),
          Volcano = traitStatsArranged()),
        file, row.names = FALSE)
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
  scatsplot <- reactive({
    foundrScatplot(req(input$trait),
                   traitDataSelectTrait(),
                   req(input$pair))
  })
  output$scatplot <- shiny::renderPlot({
    if(!shiny::isTruthy(input$pair)) {
      return(print(ggplot2::ggplot()))
    }
    
    print(scatsplot())
  })
}
