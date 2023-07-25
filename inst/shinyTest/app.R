devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
#devtools::install_github("byandell/foundr")

dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData", "Normalized")

#traitData <- readRDS(file.path(dirpath, "traitData.rds"))
db <- RSQLite::dbConnect(RSQLite::SQLite(),
                         file.path(dirpath, "traitData.sqlite"))
traitData <- dplyr::tbl(db, "traitData")

traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))

customSettings <- list(
  help = "~/FounderDietStudy/help.md",
  condition = "diet",
  dataset = c(
    PlaMet0 = "Plasma metabolites 0min",
    PlaMet120 = "Plasma metabolites 120min",
    LivMet = "Liver metabolites",
    LivRna = "Liver gene expression",
    Physio = "Physiological traits",
    Enrich = "Plasma enrichment",
    Module = "WGCNA Module eigentraits"))


################################################################

title <- "Test Shiny Tab"

ui <- foundr::ui(title)

server <- function(input, output, session) {
  
  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  foundr::server(input, output, session,
                 traitData, traitSignal, traitStats,
                 customSettings)
  
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}

shiny::shinyApp(ui = ui, server = server)
