#' Create a function to query Trait Data
#'
#' Create a function that will connect to a SQLite database of founder
#' variant information and return a data frame with variants for a
#' selected region.
#'
#' @param dbfile Name of database file
#' @param db Optional database connection (provide one of `dbfile` and `db`).
#' @param table_name Name of table in the database
#'
#' @return Function with two arguments, `dataset` and `trait`,
#'     which returns a data frame with the traits.
#'
#' This requires an existing SQLite database of traits with names at least
#'     `traitData`, `traitStats`, `traitSignal`.
#'
#' @export
#' @importFrom RSQLite dbConnect dbDisconnect
#' @importFrom dplyr collect tbl
#'

create_trait_query_func <- function(dbfile=NULL, db=NULL,
                                    table_name = "traitData") {
  if(!is.null(db)) {
    if(!is.null(dbfile))
      warning("Provide just one of dbfile or db; using db")
    
    query_func <- function(traitnames) {
      subset_trait_names.sql(db, table_name, traitnames)
    }
  }
  else {
    if(is.null(dbfile) || dbfile=="")
      stop("Provide either dbfile or db")
    
    query_func <- function(traitnames) {
      if(!file.exists(dbfile))
        stop("File ", dbfile, " doesn't exist")
      
      db <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile)
      on.exit(RSQLite::dbDisconnect(db)) # disconnect on exit
      
      subset_trait_names.sql(db, table_name, traitnames)
    }
  }
  
  query_func
}

#' Create Trait SQLite File
#'
#' @param traitData data frame
#' @param filename name of SQLite file to create
#'
#' @return list of tables (invisbly)
#' @export
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbWriteTable SQLite
#'
create_trait_sqlite <- function(traitData, filename = "traitData.sqlite") {
  db <- RSQLite::dbConnect(RSQLite::SQLite(), filename)
  on.exit(RSQLite::dbDisconnect(db))
  
  DBI::dbWriteTable(db, value = traitData, name = "traitData",
                    indexes = list(c("dataset", "trait")))
  invisible(dbListTables(db))
}