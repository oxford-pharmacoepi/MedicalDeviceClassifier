test_that("example use", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  chat <- ellmer::chat_ollama(model = "MedGemma:4b")

  devices <- dplyr::tibble(
    device_id = 1:5,
    device_name = c(
      "Medtronic Micra AV",
      "Stryker Triathlon CR Femoral Component",
      "FreeStyle Optium blood glucose test strips",
      "BD Alaris GP Volumetric Pump",
      "Diprobase cream"
    )
  )

  expect_no_error(res <- classifyMedicalDevices(chat, devices))

})
