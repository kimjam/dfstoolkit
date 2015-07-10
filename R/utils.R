#' @title nfl_query
#' @description
#' \code{nfl_query} a utility function to get data from nfl schema
#'
#' @param query query to send to database
#' 
#' @return results of query in a dataframe

nfl_query <- function(query) {
    db <- RMySQL::dbConnect(MySQL(), 
                            user='root',
                            password='Datmysqljawn97%!',
                            dbname='nfl',
                            host='localhost'
    )
    
    rs <- RMySQL::dbSendQuery(db,
                              query
    )
    
    data <- RMySQL::fetch(rs, n = -1)
    
    RMySQL::dbClearResult(rs)
    
    return(data)
}
