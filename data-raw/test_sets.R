
device_test_set <- readr::read_csv(
  "data-raw/device_test_set.csv",
  trim_ws = FALSE,
  show_col_types = FALSE
  )
mapping_test_set <- readr::read_csv(
  "data-raw/mapping_test_set.csv",
  trim_ws = FALSE,
  show_col_types = FALSE
)

usethis::use_data(device_test_set,
                  mapping_test_set,
                  overwrite = TRUE)
