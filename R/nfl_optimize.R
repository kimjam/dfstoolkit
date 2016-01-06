#' @title nfl_optimize
#' @description build optimzed line up using simplex algorithm
#'
#' @param df dataframe containing player projections, salaries, positions
#' @param platform 'dk' or 'fd' depending on platform
#' @return dataframe of chosen lineup

nfl_optimize <- function(df, platform) {
    obj <- df$svm

    if (platform == 'dk') {
        # qb, rb, rb, wr, wr, te, te, dst
        con <- rbind(
            as.numeric(df$position == 'QB'),
            as.numeric(df$position == 'RB'),
            as.numeric(df$position == 'RB'),
            as.numeric(df$position == 'WR'),
            as.numeric(df$position == 'WR'),
            as.numeric(df$position == 'TE'),
            as.numeric(df$position == 'TE'),
            as.numeric(df$position == 'DST'),
            rep(1, nrow(df)),
            df$salary
        )
        dir <- c('==', '>=', '<=', '>=', '<=', '>=', '<=', '==', '==', '<=')
        rhs <- c(1, 2, 3, 3, 4, 1, 2, 1, 9, 50000)
        result <- lpSolve::lp(
            'max',
            obj,
            con,
            dir,
            rhs,
            all.bin = TRUE
        )

        lineup <- df[result$solution == 1, ]
        return(lineup)

    } else if (platform == 'fd') {
        con <- rbind(
            as.numeric(df$position == 'QB'),
            as.numeric(df$position == 'RB'),
            as.numeric(df$position == 'WR'),
            as.numeric(df$position == 'TE'),
            as.numeric(df$position == 'K'),
            as.numeric(df$position == 'DST'),
            rep(1, nrow(df)),
            df$salary
        )
        dir <- c(rep('==', 7), '<=')
        rhs <- c(1, 2, 3, 1, 1, 1, 9, 60000)

        result <- lpSolve::lp(
            'max',
            obj,
            con,
            dir,
            rhs,
            all.bin = TRUE
        )

        lineup <- df[result$solution == 1, ]
        return(lineup)
    }
}
