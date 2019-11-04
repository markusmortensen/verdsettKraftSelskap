context("test-hent_data")

test_that("function returns one value", {
  df <- hent_produksjonsdata()
  expect_length(hent_produksjon_for_kraftverk(df, kraftverk = "Aall-Ulefoss"), 1)

  expect_error(hent_produksjon_for_kraftverk(df, "Bergen"))
})
