#' @title generate_defstats
#' @description
#' \code{generate_defstats} Generates defensive statistics from box score data and
#' inserts into appropriate database table. Will be utilized after completion of
#' week to update defense tables.
#'
#' @param target_date date to determine what data to generate defensive statistics
#' from

generate_defstats <- function(target_date) {

    qbdata <- nfl_query(query = 'SELECT * FROM QB',
                        target_date = target_date
    )

    rbdata <- nfl_query(query = 'SELECT * FROM RB',
                        target_date = target_date
    )

    wrdata <- nfl_query(query = 'SELECT * FROM WR',
                        target_date = target_date
    )

    tedata <- nfl_query(query = 'SELECT * FROM TE',
                        target_date = target_date
    )

    rbdata[9:ncol(rbdata)] <- apply(rbdata[9:ncol(rbdata)], 2, as.numeric)

    qbdef <- qbdata %>%
        dplyr::group_by(
            opp,
            date
        ) %>%
        dplyr::summarise(
            offense = team[1],
            home = ifelse(home[1] == 1, 0, 1),
            oppcmp = sum(as.numeric(cmp)),
            oppatt = sum(as.numeric(att)),
            oppyds = sum(as.numeric(yds)),
            opptd = sum(as.numeric(td)),
            oppints = sum(as.numeric(ints)),
            oppru_att = sum(as.numeric(ru_att)),
            oppru_yds = sum(as.numeric(ru_yds)),
            oppru_td = sum(as.numeric(ru_td)),
            opppts = sum(as.numeric(pts))
        ) %>%
        dplyr::rename(
            defense = opp
        ) %>%
        as.data.frame()

    rbdef <- rbdata %>%
        dplyr::group_by(
            opp,
            date
        ) %>%
        dplyr::summarise(
            offense = team[1],
            home = ifelse(home[1] == 1, 0, 1),
            oppatt = sum(as.numeric(att)),
            oppru_yds = sum(as.numeric(ru_yds)),
            oppru_td = sum(as.numeric(ru_td)),
            opptgt = sum(as.numeric(tgt)),
            opprec = sum(as.numeric(rec)),
            opprec_yds = sum(as.numeric(rec_yds)),
            opprec_td = sum(as.numeric(rec_td)),
            opppts = sum(as.numeric(pts))
        ) %>%
        dplyr::rename(
            defense = opp
        ) %>%
        as.data.frame()

    wrdef <- wrdata %>%
        dplyr::group_by(
            opp,
            date
        ) %>%
        dplyr::summarise(
            offense = team[1],
            home = ifelse(home[1] == 1, 0, 1),
            opptgt = sum(as.numeric(tgt)),
            opprec = sum(as.numeric(rec)),
            opprec_yds = sum(as.numeric(rec_yds)),
            opprec_td = sum(as.numeric(rec_td)),
            opppts = sum(as.numeric(pts))
        ) %>%
        dplyr::rename(
            defense = opp
        ) %>%
        as.data.frame()

    tedef <- tedata %>%
        dplyr::group_by(
            opp,
            date
        ) %>%
        dplyr::summarise(
            offense = team[1],
            home = ifelse(home[1] == 1, 0, 1),
            opptgt = sum(as.numeric(tgt)),
            opprec = sum(as.numeric(rec)),
            opprec_yds = sum(as.numeric(rec_yds)),
            opprec_td = sum(as.numeric(rec_td)),
            opppts = sum(as.numeric(pts))
        ) %>%
        dplyr::rename(
            defense = opp
        ) %>%
        as.data.frame()

    nfl_insert(dataframe = qbdef,
               table = 'qbdef'
    )

    nfl_insert(dataframe = rbdef,
               table = 'rbdef'
    )

    nfl_insert(dataframe = wrdef,
               table = 'wrdef'
    )

    nfl_insert(dataframe = tedef,
               table = 'tedef'
    )

}
