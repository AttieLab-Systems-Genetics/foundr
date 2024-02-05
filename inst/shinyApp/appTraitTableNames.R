dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")
traitData <- readRDS(file.path(dirpath, "traitData.rds"))
traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

################################################################

title <- "Test Shiny Trait Table"

ui <- function() {
  # INPUTS
  #   input$facet: Facet by strain?
  #   input$strains: Strains to select
  #   input$height: Plot Height
  #   input$
  #
  # OUTPUTS (see shinyTraitTable)
  #   traitSolosObject()

  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(3, shiny::uiOutput("dataset")),
          shiny::column(3, foundr::shinyTraitOrderInput("shinyOrder")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyKeyTrait"))),
        
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, foundr::shinyTraitNamesUI("shinyRelTraits"))),
        
        # Trait Table Response.
        foundr::shinyTraitTableUI("shinyTable"),
        
        shiny::uiOutput("strains"),
      ),

      shiny::mainPanel(
        shiny::tagList(
          shiny::textOutput("orderTable"),
          shiny::textOutput("keyTrait"),
          shiny::textOutput("corTable"),
          shiny::textOutput("relTraits"),
          foundr::shinyTraitTableOutput("shinyTable"),
          foundr::shinyCorTableOutput("shinyCorTable"))
      )))
}

server <- function(input, output, session) {
  
#  shiny::onStop(function() {RSQLite::dbDisconnect(db)})

  # MODULES
  # Order Traits by Stats.
  orderOutput <- foundr::shinyTraitOrder("shinyOrder", input, input,
                                         traitStats, traitSignal)
  
  # Key Trait.
  keyTraitOutput <- foundr::shinyTraitNames("shinyKeyTrait", input, orderOutput)
  
  # Key Trait and Correlation Table.
  corTableOutput <- foundr::shinyCorTable("shinyCorTable", input, input,
                                  keyTraitOutput, traitSignal)
  # Related Traits.
  relTraitsOutput <- foundr::shinyTraitNames("shinyRelTraits", input,
                                     corTableOutput, TRUE)

  # Trait Table.
  tableOutput <- foundr::shinyTraitTable("shinyTable", input, input,
                                         keyTraitOutput, relTraitsOutput,
                                         traitData, traitSignal)
  
  relTraits <- shiny::reactiveVal()
  shiny::observeEvent(
    keyTraitOutput(),
    relTraits(c(keyTraitOutput(), relTraitsOutput())),
    ignoreNULL = FALSE)
  shiny::observeEvent(
    relTraitsOutput(),
    relTraits(c(keyTraitOutput(), relTraitsOutput())),
    ignoreNULL = FALSE)
  # I/O FROM MODULE
  output$keyTrait <- renderText({
    paste("keyTrait", shiny::req(keyTraitOutput()))
  })
  output$orderTable <- renderText({
    shiny::req(orderOutput())
    
    paste("orderOutput", foundr::unite_datatraits(orderOutput())[1])
  })
  output$corTable <- renderText({
    shiny::req(corTableOutput())
    
    paste("corTableOutput", foundr::unite_datatraits(corTableOutput(), key = TRUE)[1])
  })
  output$relTraits <- renderText({
    shiny::isTruthy(relTraits())
    paste("relTraits", relTraits())
  })
  
  # SERVER-SIDE INPUTS
  output$dataset <- shiny::renderUI({
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    
    # Get datasets.
    shiny::selectInput("dataset", "Datasets:",
                       datasets, datasets[1], multiple = TRUE)
  })
  output$strains <- shiny::renderUI({
    choices <- names(foundr::CCcolors)
    shiny::checkboxGroupInput("strains", "Strains",
                              choices = choices, selected = choices,
                              inline = TRUE)
  })
  output$traits <- shiny::renderUI({
    traits <- unique(foundr::unite_datatraits(traitSignal))[1:5]

    shiny::selectInput("trait","Traits:", traits)
  })
  
  # Related Datasets.
  datasets <- shiny::reactive({
    unique(traitStats$dataset)
  })
  output$reldataset <- renderUI({
    shiny::selectInput("reldataset", "Related Datasets:",
                       datasets(), datasets()[1], multiple = TRUE)
  })
  shiny::observeEvent(
    datasets(),
    {
      selected <- datasets()[1]
      choices <- datasets()
      selected <- selected[selected %in% choices]
      if(!length(selected))
        selected <- choices[1]
      shiny::updateSelectInput(session, "reldataset", choices = choices,
                               selected = selected)
    })
}

shiny::shinyApp(ui = ui, server = server)
