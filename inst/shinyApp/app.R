devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
options(shiny.sanitize.errors = FALSE)

dirpath <- file.path("~", "founder_diet_study")
dirpath <- file.path(dirpath, "HarmonizedData")

traitData <- readRDS(file.path(dirpath, "traitData.rds"))

#devtools::install_github("byandell/foundr")
#db <- RSQLite::dbConnect(RSQLite::SQLite(),
#                         file.path(dirpath, "traitData.sqlite"))
#traitData <- dplyr::tbl(db, "traitData")

traitSignal <- readRDS(file.path(dirpath, "traitSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "traitStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

#dirpath <- file.path("~/Documents/Research/attie_alan/FounderDietStudy/deployLiver")
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds"))
traitStats <- readRDS(file.path(dirpath, "liverStats.rds"))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

# Set up help.md using datasets in `traitSignal`
foundr::link_datasets(
  traitSignal,
  file.path(dirpath, "../RawData/source.csv"),
  file.path(dirpath, "deploy"))
datasets <- readRDS(file.path(dirpath, "deploy", "datasets.rds"))

customSettings <- list(
  help = file.path(dirpath, "deploy", "help.md"),
  condition = "diet",
  entrykey = "Founder",
  dataset = datasets)

################################################################

title <- "Test Shiny Tab"

ui <- foundr::ui(title)

server <- function(input, output, session) {
  
#  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  foundr::server(input, output, session,
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}

shiny::shinyApp(ui = ui, server = server)
