#' @title predict_nfl
#' @description make predictions for given players
#'
#' @param df dataframe containing names / teams of players to make
#' predictions for
#' @param sched dataframe containing nfl schedule data
#' @param season current season
#' @param position position to make predictions for
#' @param def_start column containing first stat column
#'
#' @return returns a dataframe containing predictions to be used in an
#' excel solver

project_nfl <- function(
    df,
    sched = nfl_query('select * from nflschedule'),
    season = 2015,
    position,
    def_start
) {
    names(df) <- tolower(names(df))
    names(df)[1] <- 'name'
    df$year <- season
    names <- df$name

    # get starting date for current week
    date <- sched %>%
        dplyr::filter(
            year == season,
            week == df$week[1]
        ) %>%
        dplyr::select(
            start
        )

    df$date <- date$start

    # get date for 4 weeks ago
    start <- as.Date(df$date[1]) - lubridate::dweeks(4)

    # get player data
    data <- data.frame()
    for (name in names) {
        query <- paste0('select * from ', position, ' where name = \'', name, '\'')
        data <- rbind(data,
                      nfl_query(
                          query,
                          start
                      )
        )
    }

    # remove excess rows, split into groups
    data <- trim_df(data)
    data <- add_join_helpers(data)

    oneweek <- data %>%
        dplyr::filter(
            group == 1
        ) %>%
        dplyr::select(
            -group
        ) %>%
        dplyr::group_by(
            name,
            team
        )

    twoweek <- data %>%
        dplyr::filter(
            group == 2
        ) %>%
        dplyr::select(
            -group
        ) %>%
        dplyr::group_by(
            name,
            team
        )

    threeweek <- data %>%
        dplyr::filter(
            group == 3
        ) %>%
        dplyr::select(
            -group
        ) %>%
        dplyr::group_by(
            name,
            team
        )

    if (position == 'qb') {
        def <- nfl_query(
            paste0('select * from qbdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from qbdefavg where year = ', season)
        )
    } else if (position == 'rb') {
        def <- nfl_query(
            paste0('select * from rbdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from rbdefavg where year = ', season)
        )
    } else if (position == 'wr') {
        def <- nfl_query(
            paste0('select * from wrdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from wrdefavg where year = ', season)
        )
    } else if (position == 'te') {
        def <- nfl_query(
            paste0('select * from tedef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from tedefavg where year = ', season)
        )
    }

    names(defavg)[(match(def_start, names(defavg))):ncol(defavg)] <- paste0(
        'avg_', names(defavg)[(match(def_start, names(defavg))):ncol(defavg)]
    )

    cols <- names(def)[match(def_start, names(def)):ncol(def)]
    def <- add_join_helpers(def)

    remove_ind <- ncol(def) + 1

    vegas <- nfl_query(
        paste0('select * from vegas where date >= ', '\'', start, '\'')
    )
    names(vegas) <- tolower(names(vegas))
    vegas <- add_join_helpers(vegas)

    def <- def %>%
        dplyr::left_join(
            y = defavg %>% dplyr::select(-year),
            by = 'week'
        )

    for (col in cols) {
        def[, col] <- round(
            def[, col] / def[, paste0('avg_', col)],
            digits = 2
        )
    }

    def[is.na(def)] <- 0

    def <- def[-c(remove_ind:ncol(def))]



}
