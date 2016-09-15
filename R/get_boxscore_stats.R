#' @title get_boxscore_stats
#' @description Function to scrape offense stats and snap counts tables.
#' Generates defensive tables from offense stats.
#'
#' @param url Url of boxscore.
#' @param insert Should the function insert the data into the nfl_offense and
#' nfl_defense tables? Defaults to FALSE.
#'
#' @return Returns list containing offensive stats (off_stats) and defensive
#' stats (def_stats)
#' @export
get_boxscore_stats <- function(url, insert = FALSE) {
    # http://stackoverflow.com/questions/39232596/how-to-get-table-using-rvest
    html <- xml2::read_html(url) %>%
        rvest::html_node('body')

    # Only save and work with the body
    xml2::write_xml(html, "nfl.xml")

    # Find and remove comments
    html <- readLines("nfl.xml")
    html <- html[-grep("<!--", html)]
    html <- html[-grep("-->", html)]
    writeLines(html, "nfl.xml")

    # Read the file back in and process normally
    html <-xml2::read_html("nfl.xml")

    # table 4 - stats, table 10 and 11 snap counts
    tables <- html %>%
        rvest::html_nodes('table.sortable.stats_table') %>%
        rvest::html_table()

    names(tables) <- html %>%
        rvest::html_nodes('table.sortable.stats_table') %>%
        rvest::html_attr('id')

    file.remove('nfl.xml')

    # offensive stats, put names in sysdata
    off_stats <- tables$player_offense
    names(off_stats) <- c(
        'player', 'tm', 'cmp', 'pa_att', 'pa_yds', 'pa_td', 'int', 'sk', 'sk_yds',
        'pa_lng', 'rate', 'ru_att', 'ru_yds', 'ru_td', 'ru_lng', 'tgt', 'rec',
        'rec_yds', 'rec_td', 'rec_lng', 'fmb', 'fmbl'
    )

    off_stats %<>%
        dplyr::filter(!player %in% c('Player' ,'')) %>%
        dplyr::group_by(player,tm) %>%
        dplyr::mutate_each(dplyr::funs(as.numeric)) %>%
        dplyr::ungroup()

    # table to join to get opponent
    opp <- data.frame(tm = unique(off_stats$tm)) %>%
        dplyr::mutate(opp = rev(tm)) %>%
        dplyr::mutate_each(dplyr::funs(as.character))

    off_stats %<>%
        dplyr::left_join(y = opp, by = 'tm') %>%
        dplyr::select(-sk, -sk_yds, -pa_lng, -rate, -ru_lng, -rec_lng)

    # get snap counts for offensive players
    if (any(grepl('snap_counts', names(tables)))) {
        inds <- (1:length(tables))[grepl('snap_counts', names(tables))]
        snap_counts <- do.call(rbind, tables[inds])
        names(snap_counts) <- c('player', 'pos', 'snaps', 'snap_pct',
                                'def_num', 'def_pct', 'st_num', 'st_pct')

        snap_counts %<>%
            dplyr::filter(snaps != '0', player != 'Player') %>%
            dplyr::select(player, pos, snaps, snap_pct) %>%
            dplyr::mutate(snaps = as.numeric(snaps),
                          snap_pct = readr::parse_number(snap_pct),
                          pos = replace(pos, pos == 'FB', 'RB'))
    } else {
        stop
    }
    # create table home_date to join with who was home / away based on header
    # and date of game
    nfl_teams <- db_query(query = 'select * from nfl_teams')

    matchup_date <- xml2::read_html(url) %>%
        rvest::html_node('h1') %>%
        rvest::html_text() %>%
        strsplit(., ' - ') %>% unlist()

    date <- as.POSIXct(gsub('th|rd|st|nd', '', matchup_date[2]),
                       format = '%B %d, %Y')

    home_date <- trimws(unlist(strsplit(matchup_date[1], ' at '))) %>%
        data.frame() %>%
        dplyr::rename_('team_long' = '.') %>%
        dplyr::mutate(home = c(0, 1),
                      team_long = as.character(team_long),
                      date = date)

    home_date %<>% dplyr::left_join(y = nfl_teams, by = 'team_long')

    # join snap data, home, date, to offensive stats
    off_stats %<>%
        dplyr::left_join(
            y = snap_counts %>%
                dplyr::select(
                    player,
                    pos,
                    snaps,
                    snap_pct
                ),
            by = 'player'
        ) %>%
        dplyr::left_join(
            y = home_date %>% dplyr::select(-team_long),
            by = c('tm' = 'team_abbr')
        )

    # defensive stats
    def_stats <- off_stats %>%
        dplyr::select(-player, -tm, -snap_pct, -home, -date) %>%
        dplyr::rename(tm = opp) %>%
        dplyr::group_by(tm, pos) %>%
        dplyr::summarise_each(dplyr::funs(sum)) %>%
        dplyr::rename(def_num = snaps) %>%
        dplyr::left_join(
            y = home_date %>% dplyr::select(-team_long),
            by = c('tm' = 'team_abbr')
        )

    reps <- def_stats %>%
        dplyr::group_by(tm) %>%
        dplyr::summarise(count = n()) %>%
        .$count

    snaps <- rep(def_stats %>%
                     dplyr::filter(pos == 'QB') %>%
                     .$def_num,
                 times = reps)

    def_stats$def_num <- snaps

    # if one position did not get stats, fill with 0
    fill_def_stats <- data.frame(
        tm = rep(home_date$team_abbr, each = 4),
        pos = rep(c('QB', 'RB', 'WR', 'TE'), times = 2)
    ) %>%
        dplyr::mutate_each(dplyr::funs(as.character)) %>%
        dplyr::left_join(
            y = def_stats,
            by = c('tm', 'pos')
        )

    blanks <- is.na(fill_def_stats)

    fill_def_stats[, 3:16][blanks[, 3:16]] <- 0
    fill_def_stats[17:19] %<>% lapply(., zoo::na.locf)

    week_date <- db_query(query = 'select * from nfl_week_date') %>%
        dplyr::mutate(date = as.POSIXct(date, format = '%Y-%m-%d'))

    off_stats %<>%
        dplyr::left_join(
            y = week_date,
            by = 'date'
        ) %>%
        dplyr::mutate(
            dk_pts = pa_yds / 25 + pa_td * 4 + ifelse(pa_yds >= 300, 3, 0) -
                int + ru_yds / 10 + ru_td * 6 + ifelse(ru_yds >= 100, 3, 0) +
                rec_yds / 10 + rec_td * 6 + rec + ifelse(rec_yds >= 100, 3, 0) -
                fmbl
        )
    fill_def_stats %<>%
        dplyr::left_join(
            y = week_date,
            by = 'date'
        )

    if (insert) {
        db_insert(table = 'nfl_offense', df = off_stats)
        db_insert(table = 'nfl_defense', df = def_stats)
        invisible(list(off_stats = off_stats, def_stats = fill_def_stats))
    } else {
        return(list(off_stats = off_stats, def_stats = fill_def_stats))
    }
}
