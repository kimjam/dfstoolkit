#' @title create_profiles
#' @description Function to create profiles for players.
#'
#' @param df Dataframe to create profiles from.
#' @param window_size Size of rolling window. Defaults to 3.
#'
#' @return Returns nested list of lists containing profile and projection.
#' @export
create_profiles <- function(df, window_size = 3) {

    df %<>%
        dplyr::arrange(date) %>%
        dplyr::mutate(row_index = 1:nrow(.))

    df_long <- df %>%
        dplyr::select(-date, -week, -dk_pts) %>%
        tidyr::gather(stat, value, c(cmp:fmbl, snaps, snap_pct))

    if (max(df$row_index) <= window_size) {
        return(NULL)
    } else {
        starts <- 1:(max(df$row_index) - window_size)
        ends <- starts + window_size - 1
        proj_inds <- ends + 1

        profs <- purrr::map2(
            .x = starts,
            .y = ends,
            .f = ~ df_long %>%
                dplyr::filter(row_index %in% .x:.y) %>%
                dplyr::arrange(stat, row_index)
        )

        projections <- df$dk_pts[proj_inds]
        opp <- df$opp[proj_inds]

        profiles <- purrr::pmap(
            list(profs = profs,
                 projection = projections,
                 opp = opp),
            .f = list
        ) %>%
          lapply(., function(x) x %>% setNames(c('profile', 'projection', 'opp')))
        return(profiles)
    }
}
