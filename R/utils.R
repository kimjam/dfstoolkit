#' @title clean_conns
#' @description function to manage db connections to avoid too many connections
#' error

clean_conns <- function() {
    all_cons <- DBI::dbListConnections(MySQL())
    for (con in all_cons) {
        DBI::dbDisconnect(con)
    }
}

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

    clean_conns()

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

    DBI::dbWriteTable(conn = db,
                      name = table,
                      value = dataframe,
                      append = TRUE,
                      row.names=FALSE
    )

    clean_conns()
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

    df[,'year'] <- 0
    for (i in 1:nrow(season)) {
        df$year[df$date >= season$start[i] & df$date <= season$end[i]] <- season$year[i]
    }

    weeks <- sched %>%
        dplyr::filter(
            week != 'season'
        )

    df[, 'week'] <- 0
    for (i in 1:nrow(weeks)) {
        df$week[df$date >= weeks$start[i] & df$date <= weeks$end[i]] <- weeks$week[i]
    }

    return(df)
}

#' @title shift_one
#' @description offsets data by one week, adds actual stat columns
#'
#' @param df dataframe to offset
#' @param pts_name name of column that contains fantasy points
#' @param num_start name of column that starts stats
#' @param def boolean that indicates if df is a defensive stats dataframe,
#' default is FALSE

shift_one <- function(df, pts_name, num_start, def = FALSE) {
    pts_ind <- match(pts_name, names(df))
    start <- match(num_start, names(df))
    actual <- df
    names(actual) <- paste0('act_', names(df))

    df[2:nrow(df), start:pts_ind] <- df[1:(nrow(df) - 1), start:pts_ind]

    if (!def) {
        df <- cbind(df, actual[start:pts_ind])
    }
    df <- tail(df, n = nrow(df) - 1)

    return(df)
}

#' @title roll_n
#' @description finds rolling average for previous n weeks, adds actual stat columns
#'
#' @param df dataframe to find rolling average
#' @param n number of weeks to look base prediction on
#' @param pts_name name of column that contains fantasy points
#' @param num_start name of column that starts stats
#' @param def boolean that indicates if df is a defensive stats dataframe,
#' default is FALSE

roll_n <- function(df, n, pts_name, num_start, def = FALSE) {
    pts_ind <- match(pts_name, names(df))
    start <- match(num_start, names(df))
    actual <- df
    names(actual) <- paste0('act_', names(df))

    fill <- apply(
        df[1:(nrow(df) - 1), start:pts_ind],
        2,
        zoo::rollmean,
        k = n
    ) %>%
        as.data.frame()
    fill <- apply(fill, 2, round, digits = 1)
    df[(n + 1):nrow(df), start:pts_ind] <- fill

    if (!def) {
        df <- cbind(df, actual[start:pts_ind])
    }
    df <- tail(df, n = nrow(df) - n)

    return(df)
}
