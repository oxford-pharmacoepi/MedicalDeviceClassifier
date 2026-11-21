#' Example medical devices and non-device clinical products
#'
#' A curated benchmark dataset of 50 product names covering EU/UK MDR classes
#' (`Class I` to `Class III`), IVDR classes (`IVD (Class A)` to `IVD (Class D)`),
#' 18 clinical specialities, common NHS dm+d non-device items (such as emollient
#' creams, pharmaceuticals, and nutritional feeds), and a whitespace duplicate.
#'
#' @format A tibble with 50 rows and 7 columns:
#' \describe{
#'   \item{device_id}{Integer row identifier.}
#'   \item{device_name}{Character product name to classify.}
#'   \item{expected_is_device}{Character reference label (`"yes"` or `"no"`).}
#'   \item{expected_class}{Character reference EU/UK MDR or IVDR risk class, or
#'     `NA` for non-devices.}
#'   \item{expected_speciality}{Character reference clinical speciality, or `NA`
#'     for non-devices.}
#'   \item{expected_is_implantable}{Character reference implantable flag
#'     (`"yes"`, `"no"`, or `NA` for non-devices).}
#'   \item{expected_is_active}{Character reference active-device flag
#'     (`"yes"`, `"no"`, or `NA` for non-devices).}
#' }
#' @source Curated synthetic examples based on EU/UK MDR, IVDR, and NHS dm+d
#'   product naming conventions.
"device_test_set"
