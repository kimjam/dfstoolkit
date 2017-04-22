context('Check that boxscore links are scraped properly')

test_that('Number of NFL links scraped is correct', {
    # whole season
    links <- get_nfl_boxscore_links(year = 2012)
    expect_equal(length(links), 256)

    # one week
    links <- get_nfl_boxscore_links(year = 2012, week = 1)
    expect_equal(length(links), 16)
})
