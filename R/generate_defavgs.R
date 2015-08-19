#' @title generate_defavgs
#' @description
#' \code{generate_defstats} Generates league average defensive statistics for
#' each position
#'
#' @param qbdef dataframe of quarterback defense
#' @param rbdef dataframe of runningback defense
#' @param wrdef dataframe of wide receiver defence
#' @param tedef dataframe of te_def
#' @param target_date date to determine what data to generate defensive statistics
#' from

generate_defavgs <- function(
    qbdef = nfl_query('select * from qbdef'),
    rbdef = nfl_query('select * from rbdef'),
    wrdef = nfl_query('select * from wrdef'),
    tedef = nfl_query('select * from tedef'),
    target_date = '2002-08-31'
) {

    print('Adding join helpers to qbdef...')
    qbdef <- qbdef %>%
        dplyr::filter(
            date >= target_date
        ) %>%
        add_join_helpers()
    print('Done.')

    print('Adding join helpers to rbdef...')
    rbdef <- rbdef %>%
        dplyr::filter(
            date >= target_date
        ) %>%
        add_join_helpers()
    print('Done.')

    print('Adding join helpers to wrdef...')
    wrdef <- wrdef %>%
        dplyr::filter(
            date >= target_date
        ) %>%
        add_join_helpers()
    print('Done.')

    print('Adding join helpers to tedef...')
    tedef <- tedef %>%
        dplyr::filter(
            date >= target_date
        ) %>%
        add_join_helpers()
    print('Done.')

    qbdefavg <- qbdef %>%
        dplyr::group_by(
            year,
            week
        ) %>%
        dplyr::summarise(
            oppcmp = mean(oppcmp, na.rm = TRUE),
            oppatt = mean(oppatt, na.rm = TRUE),
            oppyds = mean(oppyds, na.rm = TRUE),
            opptd = mean(opptd, na.rm = TRUE),
            oppints = mean(oppints, na.rm = TRUE),
            oppru_att = mean(oppru_att, na.rm = TRUE),
            oppru_yds = mean(oppru_yds, na.rm = TRUE),
            oppru_td = mean(oppru_td, na.rm = TRUE),
            opppts = mean(opppts, na.rm = TRUE)
        ) %>%
        dplyr::arrange(
            year,
            as.numeric(week)
        ) %>%
        as.data.frame()

    qbdefavg[3:ncol(qbdefavg)] <- apply(
        qbdefavg[3:ncol(qbdefavg)],
        2,
        function(x) round(x, digits = 1)
    )
    rbdefavg <- rbdef %>%
        dplyr::group_by(
            year,
            week
        ) %>%
        dplyr::summarise(
            oppatt = mean(oppatt, na.rm = TRUE),
            oppru_yds = mean(oppru_yds, na.rm = TRUE),
            oppru_td = mean(oppru_td, na.rm = TRUE),
            opptgt = mean(opptgt, na.rm = TRUE),
            opprec = mean(opprec, na.rm = TRUE),
            opprec_yds = mean(opprec_yds, na.rm = TRUE),
            opprec_td = mean(opprec_td, na.rm = TRUE),
            opppts = mean(opppts, na.rm = TRUE)
        ) %>%
        dplyr::arrange(
            year,
            as.numeric(week)
        ) %>%
        as.data.frame()

    rbdefavg[3:ncol(rbdefavg)] <- apply(
        rbdefavg[3:ncol(rbdefavg)],
        2,
        function(x) round(x, digits =1)
    )

    wrdefavg <- wrdef %>%
        dplyr::group_by(
            year,
            week
        ) %>%
        dplyr::summarise(
            opptgt = mean(opptgt, na.rm = TRUE),
            opprec = mean(opprec, na.rm = TRUE),
            opprec_yds = mean(opprec_yds, na.rm = TRUE),
            opprec_td = mean(opprec_td, na.rm = TRUE),
            opppts = mean(opppts, na.rm = TRUE)
        ) %>%
        dplyr::arrange(
            year,
            as.numeric(week)
        ) %>%
        as.data.frame()

    wrdefavg[3:ncol(wrdefavg)] <- apply(
        wrdefavg[3:ncol(wrdefavg)],
        2,
        function(x) round(x, digits = 1)
    )

    tedefavg <- tedef %>%
        dplyr::group_by(
            year,
            week
        ) %>%
        dplyr::summarise(
            opptgt = mean(opptgt, na.rm = TRUE),
            opprec = mean(opprec, na.rm = TRUE),
            opprec_yds = mean(opprec_yds, na.rm = TRUE),
            opprec_td = mean(opprec_td, na.rm = TRUE),
            opppts = mean(opppts, na.rm = TRUE)
        ) %>%
        dplyr::arrange(
            year,
            as.numeric(week)
        ) %>%
        as.data.frame()

    tedefavg[3:ncol(tedefavg)] <- apply(
        tedefavg[3:ncol(tedefavg)],
        2,
        function(x) round(x, digits = 1)
    )

    nfl_insert(dataframe = qbdefavg,
               table = 'qbdefavg'
    )

    nfl_insert(dataframe = rbdefavg,
               table = 'rbdefavg'
    )

    nfl_insert(dataframe = wrdefavg,
               table = 'wrdefavg'
    )

    nfl_insert(dataframe = tedefavg,
               table = 'tedefavg'
    )

}
