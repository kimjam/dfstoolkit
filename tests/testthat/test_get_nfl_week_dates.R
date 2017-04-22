context('Check that dates for NFL weeks is scraped properly')

test_that('Check that dates are scraped correctly', {
    dates <- get_nfl_week_dates(year = 2012)

    expect_equal(unique(dates$week), 1:17)
    expect_equal(unique(dates$season), 2012)
    expect_equal(
        vapply(dates, class, character(1), USE.NAMES = FALSE),
        c('numeric', 'character', 'numeric')
    )
})
