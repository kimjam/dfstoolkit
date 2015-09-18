#' @title generate_features
#' @description creates features needed to make predictions
#'
#' @param df dataframe containing names / teams of players to make
#' predictions for
#' @param sched dataframe containing nfl schedule data
#' @param season current season
#' @param pos position to make predictions for
#' @param def_start column containing first stat column for defense
#' @param num_start column containing first stat for player
#' @param pts_name column name of pts
#'
#' @return returns list of dataframes containing one, two, three week based
#' features

generate_features <- function(
    df,
    sched = nfl_query('select * from nflschedule'),
    season = 2015,
    pos,
    def_start,
    num_start
) {

    names <- df %>%
        dplyr::filter(
            position == pos
        ) %>%
        dplyr::select(
            name
        ) %>%
        as.matrix() %>%
        as.vector()

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
    t_date <- as.character(as.POSIXct(df$date[1]) - lubridate::ddays(7))
    start <- as.character(as.POSIXct(df$date[1]) - lubridate::dweeks(4))

    # get player data
    data <- data.frame()
    for (name in names) {
        query <- paste0(
            'SELECT * FROM ', pos, ' WHERE name = \'', name, '\''
        )
        data <- rbind(data,
                      nfl_query(
                          query,
                          start
                      )
        )
    }

    vegas <- nfl_query(
        paste0('select * from sportsbook where date >= ', '\'', df$date[1], '\'')
    )
    names(vegas) <- tolower(names(vegas))
    # names(vegas)[match('over/under', names(vegas))] <- 'over.under'
    vegas <- add_join_helpers(vegas)
    vegas$over.under <- as.numeric(vegas$over.under)
    vegas$spread <- as.numeric(vegas$spread)

    data <- data %>%
        dplyr::left_join(
            y = vegas %>%
                dplyr::select(
                    team,
                    date
                ),
            by = c('team')
        ) %>%
        dplyr::rename(
            date = date.x
        )

    data$date <- data$date.y
    data <- data %>%
        dplyr::select(
            -date.y
        )

    for (i in 1:nrow(data)) {
        x <- match(data$team[i], vegas$team)
        if ((x %% 2) == 1) {
            data$opp[i] <- vegas$team[x+1]
        } else if ((x %% 2) == 0) {
            data$opp[i] <- vegas$team[x-1]
        }
    }
    # remove excess rows, split into groups
    data <- trim_df(data)
    data[match(num_start, names(data)):ncol(data)] <- sapply(
        data[match(num_start, names(data)):ncol(data)],
        as.numeric
    )
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

    if (pos == 'QB') {
        def <- nfl_query(
            paste0('select * from qbdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from qbdefavg where year = ', season)
        )
    } else if (pos == 'RB') {
        def <- nfl_query(
            paste0('select * from rbdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from rbdefavg where year = ', season)
        )
    } else if (pos == 'WR') {
        def <- nfl_query(
            paste0('select * from wrdef where date >= ', '\'', start, '\'')
        )

        defavg <- nfl_query(
            paste0('select * from wrdefavg where year = ', season)
        )
    } else if (pos == 'TE') {
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

    remove_ind <- match('year', names(def))

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

    def <- def %>%
        dplyr::left_join(
            y = vegas %>%
                dplyr::select(
                    team,
                    date
                ),
            by = c('defense' = 'team')
        ) %>%
        dplyr::rename(
            date = date.x
        )

    def$date <- def$date.y
    def <- def %>%
        dplyr::select(
            -date.y
        )

    for (i in 1:nrow(def)) {
        x <- match(def$defense[i], vegas$team)
        if ((x %% 2) == 1) {
            def$offense[i] <- vegas$team[x+1]
        } else if ((x %% 2) == 0) {
            def$offense[i] <- vegas$team[x-1]
        }
    }

    oneweek <- oneweek %>%
        dplyr::left_join(
            y = vegas %>%
                dplyr::select(
                    date,
                    team,
                    spread,
                    over.under
                ),
            by = c('date', 'team')
        ) %>%
        dplyr::left_join(
            y = def %>%
                dplyr::select(
                    -home,
                    -offense
                ),
            by = c('opp' = 'defense', 'date')
        ) %>%
        dplyr::select(
            -year,
            -week
        )

    twoweek <- twoweek %>%
        dplyr::left_join(
            y = vegas %>%
                dplyr::select(
                    date,
                    team,
                    spread,
                    over.under
                ),
            by = c('date', 'team')
        ) %>%
        dplyr::left_join(
            y = def %>%
                dplyr::select(
                    -home,
                    -offense
                ),
            by = c('opp' = 'defense', 'date')
        ) %>%
        dplyr::select(
            -year,
            -week,
            -g,
            -date,
            -opp
        ) %>%
        dplyr::summarise_each(
            funs(mean(., na.rm = TRUE))
        )

    threeweek <- threeweek %>%
        dplyr::left_join(
            y = vegas %>%
                dplyr::select(
                    date,
                    team,
                    spread,
                    over.under
                ),
            by = c('date', 'team')
        ) %>%
        dplyr::left_join(
            y = def %>%
                dplyr::select(
                    -home,
                    -offense
                ),
            by = c('opp' = 'defense', 'date')
        ) %>%
        dplyr::select(
            -year,
            -week,
            -g,
            -date,
            -opp
        ) %>%
        dplyr::summarise_each(
            funs(mean(., na.rm = TRUE))
        )

    oneweek[is.na(oneweek)] <- 0
    twoweek[is.na(twoweek)] <- 0
    threeweek[is.na(threeweek)] <- 0

    return(
        list(
            oneweek = oneweek,
            twoweek = twoweek,
            threeweek = threeweek
        )
    )
}
