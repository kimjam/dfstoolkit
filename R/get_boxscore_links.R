#' @title get_boxscore_links
#' @description Function to get links for boxscores from the schedule page.
#'
#' @param year Season to get links from.
#' @param week Week interested in. Defaults to all weeks(1 - 17).
#'
#' @return Returns list of links.
#' @export
get_boxscore_links <- function(year, week = 1:17) {
    url <- sprintf(
        'http://www.pro-football-reference.com/years/%s/games.htm',
        as.character(year)
    )

    table <- xml2::read_html(url) %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table() %>%
        janitor::clean_names() %>%
        dplyr::filter(!week %in% c('Week', ''),
                      !is.na(as.numeric(week)))

    mask <- table$week %in% week

    boxscore_links <- xml2::read_html(url) %>%
        rvest::html_nodes('.center a') %>%
        rvest::html_attr('href') %>%
        lapply(
            .,
            function(x)
                paste0('http://www.pro-football-reference.com', x)
        ) %>%
        .[mask]

    return(boxscore_links)
}
