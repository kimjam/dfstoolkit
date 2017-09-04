#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @title dfs_query
#' @description Function to get data from dfs db.
#'
#' @param db_path Path to sqlite database. Defaults to DFS_DB variable in .Renviron.
#' @param query query to send to database
#'
#' @return results of query in a dataframe
#' @export
dfs_query <- function(
    db_path = Sys.getenv('DFS_DB'),
    query
) {
    con <- DBI::dbConnect(drv = RSQLite::SQLite(),
                          dbname = db_path)

    results <- DBI::dbGetQuery(con, query)

    return(results)
}


#' @title dfs_insert
#' @description Function to insert data into dfs db.
#'
#' @param db_path Path to sqlite database. Defaults to DFS_DB variable in .Renviron.
#' @param table Name of table to write data to.
#' @param df The dataframe to write.
#' @param ... additional parameters for DBI::dbWriteTAble
#'
#' @export
dfs_insert <- function(
    db_path = Sys.getenv('DFS_DB'),
    table,
    df,
    ...
) {
    con <- DBI::dbConnect(drv = RSQLite::SQLite(),
                          dbname = db_path)

    DBI::dbWriteTable(conn = con,
                      name = table,
                      value = df,
                      ...)
}

#' @title sqlite2posix
#' @description Utility function to change integer to date.
#'
#' @param x numeric value to change to POSIXct
#'
#' @return Returns dataframe with date column fixed.
#' @export
sqlite2posix <- function(x) {
    as.POSIXct(x, origin = '1970-01-01')
}
