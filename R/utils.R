#' @title nfl_query
#' @description
#' \code{nfl_query} a utility function to get data from nfl schema
#'
#' @param query query to send to database
#' @param target_date query will only get data at or after this date. Defaults
#' to start of 2002 NFL season.
#' @return results of query in a dataframe

nfl_query <- function(query, target_date = '2002-09-05') {

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

    query_split = unlist(strsplit(query, ' '))
    pos_tables = c('qb', 'rb', 'wr', 'te')
    if (query_split[match('from', query_split) + 1] %in% pos_tables) {
        data <- data %>%
            dplyr::filter(date >= target_date)
    }

    RMySQL::dbClearResult(rs)

    DBI::dbDisconnect(db)

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

    DBI::dbDisconnect(db)
}

#' @title add_join_helpers
#' @description function to add season and week columns to assist in creating
#' average tables
#'
#' @param df a dataframe to add the columns to
#' @param sched a dataframe of nfl schedule data

add_join_helpers <- function(df, sched = nfl_query('select * from nflschedule')) {
    season <- sched %>%
        dplyr::filter(
            week == 'season'
        )

    year <- vector(length = nrow(df))
    for (i in 1:nrow(df)) {
        for(n in 1:nrow(season)) {
            if (df$date[i] >= season$start[n] & df$date[i] <= season$end[n]) {
                year[i] <- season$year[n]
            }
        }
    }

    df$year <- year
    weeks <- sched %>%
        dplyr::filter(
            week != 'season'
        )

    week <- vector(length = nrow(df))
    for (i in 1:nrow(df)) {
        s_weeks <- weeks %>%
            dplyr::filter(
                year == df$year[i]
            )
        for (n in 1:nrow(s_weeks)) {
            if (df$date[i] >= s_weeks$start[n] & df$date[i] <= s_weeks$end[n]) {
                week[i] <- s_weeks$week[n]
            }
        }
    }

    df$week <- as.numeric(week)

    return(df)
}
