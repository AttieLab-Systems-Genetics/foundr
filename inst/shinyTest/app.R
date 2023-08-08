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

# Pull dataset names from `source.csv`
dataset <- read.csv(file.path(dirpath, "../../RawData", "source.csv"))
dataset <- dataset[dataset$longname != "", c(1,3)]
rownames(dataset) <- NULL
datasets <- dataset$longname
names(datasets) <- dataset$shortname

rmarkdown::render("~/FounderDietStudy/help.Rmd",
                  rmarkdown::md_document(),
                  output_file = "~/FounderDietStudy/help.md")

customSettings <- list(
  help = "~/FounderDietStudy/help.md",
  condition = "diet",
  entrykey = "Founder",
  dataset = datasets)


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
