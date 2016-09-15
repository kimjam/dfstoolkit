#' @title nfl_query
#' @description
#' \code{nfl_query} a utility function to get data from nfl schema
#'
#' @param db_path Path to sqlite database. Defaults to SQLITE_DB variable in .Renviron.
#' @param query query to send to database
#' @return results of query in a dataframe
nfl_query <- function(
    db_path = Sys.getenv('SQLITE_DB'),
    query
) {
    con <- DBI::dbConnect(drv = RSQLite::SQLite(),
                          dbname = db_name)

    results <- DBI::dbGetQuery(con, query)

    return(results)
}


#' @title nfl_insert
#' @description \code{nfl_insert} a utility function to insert data into nfl schema
#'
#' @param db_path Path to sqlite database. Defaults to SQLITE_DB variable in .Renviron.
#' @param table Name of table to write data to.
#' @param df The dataframe to write.
nfl_insert <- function(
    db_name = Sys.getenv('SQLITE_DB'),
    table,
    df
) {
    con <- DBI::dbConnect(drv = RSQLite::SQLite(),
                          dbname = db_name)

    DBI::dbWriteTable(conn = con,
                      name = table,
                      value = df,
                      append = TRUE)
}
