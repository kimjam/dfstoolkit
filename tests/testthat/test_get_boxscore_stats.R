context('Check that boxscores are scraped properly')

test_that('Check that NFL boxscores are scraped properly', {
    stats <- get_nfl_boxscore_stats(
        url = 'http://www.pro-football-reference.com/boxscores/201609080den.htm'
    )

    expect_equal(length(stats), 2)
    expect_named(stats, c('off_stats', 'def_stats'))
    expect_equal(ncol(stats$off_stats), 25)
    expect_equal(ncol(stats$def_stats), 21)
    expect_equal(nrow(stats$def_stats), 8)
    expect_equal(unique(stats$def_stats$pos), c('QB', 'RB', 'WR', 'TE'))
})

test_that('Check that positions are scraped properly', {
    positions <- get_nfl_positions(
        links = c('http://www.pro-football-reference.com/players/N/NewtCa00.htm',
                  'http://www.pro-football-reference.com/players/S/StewJo00.htm',
                  'http://www.pro-football-reference.com/players/B/BenjKe00.htm',
                  'http://www.pro-football-reference.com/players/O/OlseGr00.htm',
                  'http://www.pro-football-reference.com/players/J/JanoAn00.htm')
    )

    expect_equal(positions, c('QB', 'RB', 'WR', 'TE', 'RB'))
})
