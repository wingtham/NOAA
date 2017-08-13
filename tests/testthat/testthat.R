library(testthat)
library(NOAA)


df <- read_delim(file = file.path("..", "..", "ins", "extdata", "signif.txt"),
                 delim = "\t")

test_that("data clean", {
  expect_that(df, is_a("data.frame"))

  cleaned_df <- eq_clean_data(df)
  expect_that(cleaned_df, is_a("data.frame"))

  loc_cleaned_df <- eq_location_clean(df)
  expect_that(loc_cleaned_df, is_a("data.frame"))
})


test_that("timeline test", {
  plot1 <- ggplot(data=df) +
    aes(x=DATE) +
    geom_timeline()
  expect_that(plot1, is_a("ggplot"))

  plot2 <- ggplot(data=df) +
    aes(x=DATE) +
    stat_timeline()
  expect_that(plot2, is_a("ggplot"))

  expect_that(theme_timeline(), is_a("theme"))
})


test_that("label test", {
  label1 <- ggplot(data=df) +
    aes(x=DATE) +
    geom_timelabel()
  expect_that(label1, is_a("ggplot"))

  label2 <- ggplot(data=df) +
    aes(x=DATE) +
    stat_timeline()
  expect_that(label2, is_a("ggplot"))
})


df <- eq_clean_data(df)
test_that("interactive map test", {
  map1 <- df %>%
    dplyr::filter(COUNTRY == "MEXICO" & year(DATE) >=2000) %>%
    eq_map(annot_col = "DATE")
  expect_that(map1, is_a("leaflet"))

  pop_texts <- eq_create_label(df)
  expect_that(pop_texts, is_a("character"))

})
