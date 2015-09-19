#' @title salary_parser
#' @description parses salary data from DraftKings or FanDuel
#'
#' @param platform string indicating 'DK' or 'FD'
#' @param week the week you are making predictions for
#' @return dataframe of parsed df to be used by other dfstoolkit functions

salary_parser <- function(platform, week) {
    if (platform == 'DK') {

        query <- paste0(
            'select * from draftkings where week = ',
            week
        )
        salary = nfl_query(query)
        salary <- salary %>%
            dplyr::filter(
                position != 'K'
            )
        
        salary$name[salary$position == 'DST'] <- sapply(
            salary$name[salary$position == 'DST'],
            function(x) gsub(' ', '', x)
        )

    } else if (platform == 'FD') {
        query <- paste0(
            'select * from fanduel where week = ',
            week
        )
        salary = nfl_query(query)
        salary$name <- ifelse(
            salary$position != 'D',
            paste(salary$firstname, salary$lastname),
            salary$lastname
        )
        salary$position[salary$position == 'D'] <- 'DST'
    }
    salary <- salary[c('name', 'position', 'week', 'salary')]
    salary$name <- tolower(gsub("'", "", salary$name))


    return(salary)
}
