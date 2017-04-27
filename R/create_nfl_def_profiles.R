#' @title create_nfl_def_profiles
#' @description Function to profiles for nfl defenses.
#'
#' @param df Dataframe to create profile from
#' @param window_size Size of rolling window. Defaults to 3.
#'
#' @return Returns list of profiles.
#' @export
create_nfl_def_profiles <- function(df, window_size = 3) {
    df %<>%
        dplyr::arrange(date)

    home <- ifelse(df$home == 0, 1, 0)

    df_long <- df %>%
        dplyr::select(-home, -date, -week) %>%
        setNames(paste0('def_', names(.))) %>%
        dplyr::rename(def_num = def_def_num) %>%
        dplyr::mutate(row_index = 1:nrow(.)) %>%
        tidyr::gather(stat, value, c(def_cmp:def_num))

    if (max(df_long$row_index) <= window_size) {
        return(NULL)
    } else {
        starts <- 1:(max(df_long$row_index) - window_size)
        ends <- starts + window_size - 1
        proj_inds <- ends + 1

        profs <- purrr::map2(
            .x = starts,
            .y = ends,
            .f = ~ df_long %>%
                dplyr::filter(row_index %in% .x:.y) %>%
                dplyr::arrange(stat, row_index)
        )

        off_home <- home[proj_inds]

        profiles <- purrr::map2(
            .x = profs,
            .y = off_home,
            .f = ~list(profile = .x, home = .y)
        )

        return(profiles)
    }
}
