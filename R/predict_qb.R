#' @title predict_qb
#' @description make predictions for qb
#'
#' @param qb_dflist list of qb dataframes (one ,two, three week based)
#' @param price df of qb prices
#'
#' @return returns dataframe

predict_qb <- function(qb_dflist, price) {

    price$name <- tolower(gsub("'", "", price$name))

    oneweek <- qb_dflist[['oneweek']] %>% as.data.frame()
    twoweek <- qb_dflist[['twoweek']] %>% as.data.frame()
    threeweek <- qb_dflist[['threeweek']] %>% as.data.frame()

    oneweek_svm <- predict(qb_svm1, newdata = oneweek)
    twoweek_svm <- predict(qb_svm2, newdata = twoweek)
    threeweek_svm <- predict(qb_svm3, newdata = threeweek)

    td <- predict(qb_td1, newdata = oneweek, 'probs')
    int <- predict(qb_int1, newdata = oneweek, 'probs')
    rtd <- predict(qb_rutd1, newdata = oneweek, 'probs')
    oneweek_ind <- (
        predict(qb_yds1, newdata = oneweek) / 25 +
            predict(qb_ruyds1, newdata = oneweek) / 10 +
            (td %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(is.null(ncol(td)),
                              length(td),
                              ncol(td)
                )
            )) * 4 -
            (int %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(int) - 1),
                    0:(ncol(int) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(td)),
                    length(int),
                    ncol(int)
                )
            )) +
            (rtd %*% matrix(
                ifelse(
                    is.null(ncol(rtd)),
                    0:(length(rtd) - 1),
                    0:(ncol(rtd) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rtd)),
                    length(rtd),
                    ncol(rtd)
                )
            )) * 6
    )

    td <- predict(qb_td2, newdata = twoweek, 'probs')
    int <- predict(qb_int2, newdata = twoweek, 'probs')
    rtd <- predict(qb_rutd2, newdata = twoweek, 'probs')
    twoweek_ind <- (
        predict(qb_yds2, newdata = twoweek) / 25 +
            predict(qb_ruyds2, newdata = twoweek) / 10 +
            (td %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(is.null(ncol(td)),
                              length(td),
                              ncol(td)
                )
            )) * 4 -
            (int %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(int) - 1),
                    0:(ncol(int) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(td)),
                    length(int),
                    ncol(int)
                )
            )) +
            (rtd %*% matrix(
                ifelse(
                    is.null(ncol(rtd)),
                    0:(length(rtd) - 1),
                    0:(ncol(rtd) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rtd)),
                    length(rtd),
                    ncol(rtd)
                )
            )) * 6
    )

    td <- predict(qb_td3, newdata = threeweek, 'probs')
    int <- predict(qb_int3, newdata = threeweek, 'probs')
    rtd <- predict(qb_rutd3, newdata = threeweek, 'probs')
    threeweek_ind <- (
        predict(qb_yds3, newdata = threeweek) / 25 +
            predict(qb_ruyds3, newdata = threeweek) / 10 +
            (td %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(td) - 1),
                    0:(ncol(td) - 1)
                ),
                nrow = ifelse(is.null(ncol(td)),
                              length(td),
                              ncol(td)
                )
            )) * 4 -
            (int %*% matrix(
                ifelse(
                    is.null(ncol(td)),
                    0:(length(int) - 1),
                    0:(ncol(int) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(td)),
                    length(int),
                    ncol(int)
                )
            )) +
            (rtd %*% matrix(
                ifelse(
                    is.null(ncol(rtd)),
                    0:(length(rtd) - 1),
                    0:(ncol(rtd) - 1)
                ),
                nrow = ifelse(
                    is.null(ncol(rtd)),
                    length(rtd),
                    ncol(rtd)
                )
            )) * 6
    )

    oneweek_pred <- data.frame(
        name = oneweek$name,
        team = oneweek$team,
        svm = oneweek_svm,
        ind = oneweek_ind
    )

    twoweek_pred <- data.frame(
        name = twoweek$name,
        team = twoweek$team,
        svm = twoweek_svm,
        ind = twoweek_ind
    )

    threeweek_pred <- data.frame(
        name = threeweek$name,
        team = threeweek$team,
        svm = threeweek_svm,
        ind = threeweek_ind
    )

    qb_pred <- rbind(oneweek_pred, twoweek_pred, threeweek_pred)
    qb_pred$name <- as.character(qb_pred$name)
    qb_pred$team <- as.character(qb_pred$team)

    price$position <- 'QB'
    qb_pred <- price %>%
        dplyr::select(
            name,
            position,
            salary
        ) %>%
        dplyr::inner_join(
            y = qb_pred,
            by = c('name')
        )

    return(qb_pred)
}
