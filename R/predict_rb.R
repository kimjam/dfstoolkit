#' @title predict_rb
#' @description make predictions for rb
#'
#' @param rb_dflist list of rb dataframes (one ,two, three week based)
#' @param price df of player prices
#'
#' @return returns dataframe

predict_rb <- function(rb_dflist, price) {

    price$name <- tolower(gsub("'", "", price$name))
    price <- price %>%
        dplyr::filter(
            position == 'RB'
        )

    oneweek <- rb_dflist[['oneweek']] %>% as.data.frame()
    twoweek <- rb_dflist[['twoweek']] %>% as.data.frame()
    threeweek <- rb_dflist[['threeweek']] %>% as.data.frame()

    oneweek_svm <- predict(rb_svm1, newdata = oneweek)
    twoweek_svm <- predict(rb_svm2, newdata = twoweek)
    threeweek_svm <- predict(rb_svm3, newdata = threeweek)

    if (nrow(oneweek) > 0) {
        td <- predict(rb_td1, newdata = oneweek, 'probs')
        rectd <- predict(rb_rtd1, newdata = oneweek, 'probs')
        oneweek_ind <- (
            predict(rb_rec1, newdata = oneweek) / 2 +
            predict(rb_yds1, newdata = oneweek) / 10 +
            predict(rb_ryd1, newdata = oneweek) / 10 +
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
            )) * 6 +
            (rectd %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rectd)),
                    length(rectd),
                    ncol(rectd)
                )
            )) * 6
        )
    } else {
        oneweek_ink <- data.frame()
    }

    if (nrow(twoweek) > 0) {
        td <- predict(rb_td2, newdata = twoweek, 'probs')
        rectd <- predict(rb_rtd2, newdata = twoweek, 'probs')
        twoweek_ind <- (
            predict(rb_rec2, newdata = twoweek) / 2 +
            predict(rb_yds2, newdata = twoweek) / 10 +
            predict(rb_ryd2, newdata = twoweek) / 10 +
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
            )) * 6 +
            (rectd %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rectd)),
                    length(rectd),
                    ncol(rectd)
                )
            )) * 6
        )
    } else {
        twoweek_ind <- data.frame()
    }

    if (nrow(threeweek) > 0) {
        td <- predict(rb_td3, newdata = threeweek, 'probs')
        rectd <- predict(rb_rtd3, newdata = threeweek, 'probs')
        threeweek_ind <- (
            predict(rb_rec3, newdata = threeweek) / 2 +
            predict(rb_yds3, newdata = threeweek) / 10 +
            predict(rb_ryd3, newdata = threeweek) / 10 +
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
            )) * 6 +
            (rectd %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rectd)),
                    length(rectd),
                    ncol(rectd)
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

    rb_pred <- rbind(oneweek_pred, twoweek_pred, threeweek_pred)
    rb_pred$name <- as.character(rb_pred$name)
    rb_pred$team <- as.character(rb_pred$team)

    price$position <- 'RB'
    rb_pred <- price %>%
        dplyr::select(
            name,
            position,
            salary
        ) %>%
        dplyr::inner_join(
            y = rb_pred,
            by = c('name')
        )

    return(rb_pred)
}
