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
#' @param bye_teams character vector of teams on bye
#'
#' @return returns list of dataframes containing one, two, three week based
#' features

generate_features <- function(
    df,
    sched = nfl_query('select * from nflschedule'),
    season = 2015,
    pos,
    def_start,
    num_start,
    bye_teams
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

    # filter out players on bye
    data <- data %>%
        dplyr::filter(
            !(team %in% bye_teams)
        )

    data <- add_join_helpers(data)

    # remove excess rows, split into groups
    data <- trim_df(data)
    data[match(num_start, names(data)):ncol(data)] <- sapply(
        data[match(num_start, names(data)):ncol(data)],
        as.numeric
    )

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

    def <- def[-c((remove_ind+2):ncol(def))]

    if (nrow(oneweek) > 0) {
        oneweek_def <- def %>%
            dplyr::filter(
                week == max(week)
            )

        oneweek_def <- oppdate_fixer(
            df = oneweek_def,
            v = vegas
        )
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
                y = oneweek_def %>%
                    dplyr::select(
                        -home,
                        -offense,
                        -year,
                        -week
                    ),
                by = c('opp' = 'defense', 'date')
            ) %>%
            dplyr::select(
                -year,
                -week
            )

        oneweek[is.na(oneweek)] <- 0
    } else {
        oneweek <- data.frame()
    }

    if (nrow(twoweek) > 0) {
        twoweek_def <- def %>%
            dplyr::group_by(
                defense
            )

        twoweek_def <- dplyr::do(
            twoweek_def,
            roll_n(
                df = .,
                n = 2,
                pts_name = 'opppts',
                num_name = def_start
            )
        )

        twoweek_def <- oppdate_fixer(
            df = twoweek_def,
            v = vegas
        )

        twoweek <- twoweek %>%
            dplyr::group_by(
                name
            )

        twoweek <- dplyr::do(
            twoweek,
            roll_n(
                df = .,
                n = 2,
                pts_name = 'pts',
                num_name = num_start
            )
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
                y = twoweek_def %>%
                    dplyr::select(
                        -home,
                        -offense,
                        -year,
                        -week
                    ),
                by = c('opp' = 'defense', 'date')
            ) %>%
            dplyr::select(
                -year,
                -week,
                -g,
                -date,
                -opp
            )

        twoweek[is.na(twoweek)] <- 0
    } else {
        twoweek <- data.frame()
    }

    if (nrow(threeweek) > 0) {
        threeweek_def <- def %>%
            dplyr::group_by(
                defense
            )

        threeweek_def <- dplyr::do(
            threeweekdef,
            roll_n(
                df = .,
                n = 3,
                pts_name = 'opppts',
                num_name = def_start
            )
        )

        threeweek_def <- oppdate_fixer(
            df = threeweek_def,
            v = vegas
        )

        threeweek <- threeweek %>%
            dplyr::group_by(
                name
            )

        threeweek <- dplyr::do(
            threeweek,
            roll_n(
                df = .,
                n = 3,
                pts_name = 'pts',
                num_name = num_start
            )
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
                y = threeweek_def %>%
                    dplyr::select(
                        -home,
                        -offense,
                        -year,
                        -week
                    ),
                by = c('opp' = 'defense', 'date')
            ) %>%
            dplyr::select(
                -year,
                -week,
                -g,
                -date,
                -opp
            )#%>%
        #         dplyr::summarise_each(
        #             funs(mean(., na.rm = TRUE))
        #         )

        threeweek[is.na(threeweek)] <- 0
    } else {
        threeweek <- data.frame()
    }

    return(
        list(
            oneweek = oneweek,
            twoweek = twoweek,
            threeweek = threeweek
        )
    )
}
