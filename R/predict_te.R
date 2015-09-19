#' @title predict_te
#' @description make predictions for te
#'
#' @param te_dflist list of te dataframes (one ,two, three week based)
#' @param price df of te prices
#'
#' @return returns dataframe of predictions

predict_te <- function(te_dflist, price) {

    price$name <- tolower(gsub("'", "", price$name))

    oneweek <- te_dflist[['oneweek']] %>% as.data.frame()
    twoweek <- te_dflist[['twoweek']] %>% as.data.frame()
    threeweek <- te_dflist[['threeweek']] %>% as.data.frame()

    oneweek_svm <- predict(te_svm1, newdata = oneweek)
    twoweek_svm <- predict(te_svm2, newdata = twoweek)
    threeweek_svm <- predict(te_svm3, newdata = threeweek)

    if (nrow(oneweek) > 0) {

        td <- predict(te_td1, newdata = oneweek, 'probs')
        oneweek_ind <- (
            predict(te_rec1, newdata = oneweek) / 2 +
                predict(te_yds1, newdata = oneweek) / 10 +
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

        td <- predict(te_td2, newdata = twoweek, 'probs')
        twoweek_ind <- (
            predict(te_rec2, newdata = twoweek) / 2 +
                predict(te_yds2, newdata = twoweek) / 10 +
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

        td <- predict(te_td3, newdata = threeweek, 'probs')
        threeweek_ind <- (
            predict(te_rec3, newdata = threeweek) / 2 +
                predict(te_yds3, newdata = threeweek) / 10 +
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

    te_pred <- rbind(oneweek_pred, twoweek_pred, threeweek_pred)
    te_pred$name <- as.character(te_pred$name)
    te_pred$team <- as.character(te_pred$team)

    price$position <- 'TE'
    te_pred <- price %>%
        dplyr::select(
            name,
            position,
            salary
        ) %>%
        dplyr::inner_join(
            y = te_pred,
            by = c('name')
        )

    return(te_pred)
}
