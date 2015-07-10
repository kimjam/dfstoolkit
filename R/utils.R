#' @title nfl_query
#' @description
#' \code{nfl_query} a utility function to get data from nfl schema
#'
#' @param query query to send to database
#' @param target_date query will only get data at or after this date
#' @return results of query in a dataframe

nfl_query <- function(query, target_date) {
    
    db <- RMySQL::dbConnect(MySQL(), 
                            user='root',
                            password='Datmysqljawn97%!',
                            dbname='nfl',
                            host='localhost'
    )
    
    rs <- RMySQL::dbSendQuery(conn = db,
                              statement = query
    )
    
    data <- RMySQL::fetch(rs, n = -1)
    data$date <- as.Date(data$date)
    
    data <- data %>%
        dplyr::filter(date >= as.Date(target_date))
    
    RMySQL::dbClearResult(rs)
    
    return(data)
}

#' @title nfl_insert
#' @description \code{nfl_insert} a utility function to insert data into nfl schema
#' 
#' @param dataframe a dataframe that contains the data to be inserted into table
#' @param table the name of the table to insert the dataframe into

nfl_insert <- function(dataframe, table) {
    db <- RMySQL::dbConnect(MySQL(), 
                            user='root',
                            password='Datmysqljawn97%!',
                            dbname='nfl',
                            host='localhost'
    )
    
    RMySQL::dbWriteTable(conn = db, 
                         name = table, 
                         value = dataframe, 
                         append = TRUE, 
                         row.names=FALSE
    )
} 
