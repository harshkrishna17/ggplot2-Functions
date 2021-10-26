############# TESTING PLOT_TRENDLINE ################
# Scraping data and selecting only 200 rows
laliga_2022 <- get_match_results(country = "ESP", gender = "M", season_end_year = c(2020, 2021, 2022), tier = "1st")
data <- laliga_2022
data1 <- data[c(1:200), ]

# Creating a dataset with a row of NA's (Beginning, middle and end of dataset)
dat1 <- laliga_2022[c(1:200), ]
dat2 <- laliga_2022[257, ]
dat2 <- NA
dat3 <- laliga_2022[c(400:600), ]

#Beginning
data2 <- rbind(dat1, dat2, dat3)
#Middle
data3 <- rbind(dat2, dat1, dat3)
#End
data4 <- rbind(dat1, dat3, dat2)

# Normal dataframe
data5 <- laliga_2022

testthat::test_that("Testing plotting trendlines: ", {
  p <- plot_trendline(data = laliga_2022, team = "Barcelona",
                      colour_xg = "blue", colour_xga = "red",
                      roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
  
  #testing for plotting on a dataframe with NA's 
  p <- plot_trendline(data = data3, team = "Barcelona",
                        colour_xg = "blue", colour_xga = "red",
                        roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
  
  #testing using a dataframe that has limited rows
  p <- plot_trendline(data = data1, team = "Barcelona",
                         colour_xg = "blue", colour_xga = "red",
                         roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
})
