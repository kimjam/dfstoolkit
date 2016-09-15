#' @title get_week_dates
#' @description Function to get the dates for each week.
#'
#' @param year Season to get data for.
#' @param insert Should the function insert data into nfl_week_date table?
#' Defaults to FALSE.
#'
#' @return Returns dataframe of dates for each week.
#' @export
get_week_dates <- function(year, insert = FALSE) {
    sched_url <- sprintf(
        'http://www.pro-football-reference.com/years/%s/games.htm',
        as.character(year)
    )

    sched <- xml2::read_html(
        sched_url
    ) %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table() %>% .[1:3]

    names(sched)[1:3] <- c('week', 'day', 'date')

    sched %<>%
        dplyr::filter(
            !grepl('Pre*|Week| |WildCard|Division|ConfChamp|SuperBowl', week)
        ) %>%
        dplyr::mutate(
            week = as.numeric(week),
            date = ifelse(
                grepl('January*', date),
                strftime(as.POSIXct(paste0(date, ', ', year + 1),
                                    format = '%B %d, %Y')),
                strftime(as.POSIXct(paste0(date, ', ', year),
                                    format = '%B %d, %Y'))
            )
        )

    week_date <- data.frame(week = 1:17) %>%
        dplyr::left_join(y = sched %>%
                             dplyr::select(week, date),
                         by = 'week') %>%
        unique() %>%
        dplyr::mutate(
            season = year
        )

    if (insert == TRUE) {
        db_insert(table = 'nfl_week_date', df = week_date)
        invisible(week_date)
    } else {
        return(week_date)
    }
}
