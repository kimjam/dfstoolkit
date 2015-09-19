#' @title predict_wr
#' @description make predictions for wr
#'
#' @param wr_dflist list of wr dataframes (one ,two, three week based)
#' @param price df of player prices
#'
#' @return returns dataframe

predict_wr <- function(wr_dflist, price) {

    price$name <- tolower(gsub("'", "", price$name))
    price <- price %>%
        dplyr::filter(
            position == 'WR'
        )

    oneweek <- wr_dflist[['oneweek']] %>% as.data.frame()
    twoweek <- wr_dflist[['twoweek']] %>% as.data.frame()
    threeweek <- wr_dflist[['threeweek']] %>% as.data.frame()

    oneweek_svm <- predict(wr_svm1, newdata = oneweek)
    twoweek_svm <- predict(wr_svm2, newdata = twoweek)
    threeweek_svm <- predict(wr_svm3, newdata = threeweek)

    if (nrow(oneweek) > 0) {

        td <- predict(wr_td1, newdata = oneweek, 'probs')
        oneweek_ind <- (
            predict(wr_rec1, newdata = oneweek) / 2 +
                predict(wr_yds1, newdata = oneweek) / 10 +
                (td %*% matrix(
                    ifelse(
                        is.null(ncol(td)),
                        0:(length(td) - 1),
                        0:(ncol(td) - 1)
                    ),
                    nrow = ifelse(
                        is.null(ncol(td)),
                        length(td),
                        ncol(td)
                    )
                )) * 6
        )
    } else {
        oneweek_ind <- data.frame()
    }

    if (nrow(twoweek) > 0) {

        td <- predict(wr_td2, newdata = twoweek, 'probs')
        twoweek_ind <- (
            predict(wr_rec2, newdata = twoweek) / 2 +
                predict(wr_yds2, newdata = twoweek) / 10 +
                (td %*% matrix(
                    ifelse(
                        is.null(ncol(td)),
                        0:(length(td) - 1),
                        0:(ncol(td) - 1)
                    ),
                    nrow = ifelse(
                        is.null(ncol(td)),
                        length(td),
                        ncol(td)
                    )
                )) * 6
        )
    } else {
        twoweek_ind <- data.frame()
    }

    if (nrow(threeweek) > 0) {

        td <- predict(wr_td3, newdata = threeweek, 'probs')
        threeweek_ind <- (
            predict(wr_rec3, newdata = threeweek) / 2 +
                predict(wr_yds3, newdata = threeweek) / 10 +
                (td %*% matrix(
                    ifelse(
                        is.null(ncol(td)),
                        0:(length(td) - 1),
                        0:(ncol(td) - 1)
                    ),
                    nrow = ifelse(
                        is.null(ncol(td)),
                        length(td),
                        ncol(td)
                    )
                )) * 6
        )
    } else {
        threeweek_ind <- data.frame()
    }

    oneweek_pred <- data.frame(
        name = oneweek$name,
        team = oneweek$team,
        svm.1 = oneweek_svm,
        ind.1 = oneweek_ind
    )

    twoweek_pred <- data.frame(
        name = twoweek$name,
        team = twoweek$team,
        svm.2 = twoweek_svm,
        ind.2 = twoweek_ind
    )

    threeweek_pred <- data.frame(
        name = threeweek$name,
        team = threeweek$team,
        svm.3 = threeweek_svm,
        ind.3 = threeweek_ind
    )

    wr_pred <- rbind(oneweek_pred, twoweek_pred, threeweek_pred)
    wr_pred$name <- as.character(wr_pred$name)
    wr_pred$team <- as.character(wr_pred$team)

    price$position <- 'WR'
    wr_pred <- price %>%
        dplyr::select(
            name,
            position,
            salary
        ) %>%
        dplyr::inner_join(
            y = wr_pred,
            by = c('name')
        )

    return(wr_pred)
}
