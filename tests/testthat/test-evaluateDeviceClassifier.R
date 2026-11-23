test_that("multiplication works", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  chat <- ellmer::chat_ollama(model = "MedGemma:4b")
  expect_no_error(
   res <- evaluateDeviceClassifier(chat,
                           data = device_test_set |>
                             head(5),
                           batchSize = 5)
  )

})
