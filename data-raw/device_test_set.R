
device_test_set <- readr::read_csv(
  "data-raw/device_test_set.csv",
  trim_ws = FALSE,
  show_col_types = FALSE,
  col_types = readr::cols(
    device_id = readr::col_integer(),
    device_name = readr::col_character(),
    expected_is_device = readr::col_character(),
    expected_class = readr::col_character(),
    expected_speciality = readr::col_character(),
    expected_is_implantable = readr::col_character(),
    expected_is_active = readr::col_character()
  )
)

usethis::use_data(device_test_set, overwrite = TRUE)
