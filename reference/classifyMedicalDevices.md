# Classify medical devices

Uses an ellmer `Chat` object (e.g.,
[`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html)
or
[`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html))
to classify device data on the following:

- **Is a device:** Flags whether each item is truly a medical device
  (`"yes"` or `"no"`), excluding non-device products such as creams,
  pharmaceuticals, integral drug-device medicines, and nutritional
  feeds.

- **Class:** Assigns medical devices to an EU/UK MDR risk class
  (`Class I`, `Class IIa`, `Class IIb`, `Class III`) or EU/UK IVDR risk
  class (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`,
  `IVD (Class D)`).

- **FDA medical speciality:** Assigns true medical devices to an FDA
  clinical speciality (18 clinical specialities plus `Other`).

- **MHRA medical speciality:** Assigns true medical devices to an MHRA
  speciality (10 clinical specialities plus `Other`).

- **EMA medical speciality:** Assigns true medical devices to an EMA
  Medical Device Expert Panel (12 thematic panels plus `Other`).

- **EMDN category:** Assigns true medical devices to a top-level
  European Medical Device Nomenclature (EMDN) category (23 lettered
  categories, A-Z including Annex XVI Category X, plus `Other`).

- **Implantable:** Flags whether medical devices are implantable
  (`"yes"` or `"no"`).

- **Active status:** Flags whether medical devices are active/powered
  (`"yes"` or `"no"`).

- **Software status:** Flags whether medical devices are standalone
  Software as a Medical Device / SaMD (`"yes"` or `"no"`).

## Usage

``` r
classifyMedicalDevices(chat, data, maxRetries = 3L, batchSize = 1L)
```

## Arguments

- chat:

  An ellmer `Chat` object (such as from
  `ellmer::chat_openai(model = "gpt-4o-mini")` or
  `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).

- data:

  A `data.frame` or `tibble` containing a `device_name` column which
  will be used to classify devices.

- maxRetries:

  Maximum number of attempts for any concept before returning
  `NA_character_`.

- batchSize:

  Number of unique device names to classify per LLM request. Keep this
  low (e.g., `1L`-`5L`) for smaller local models to maintain
  structured-output reliability, and higher (e.g., `20L`-`50L`) for
  larger frontier models to reduce request overhead and latency.

## Value

A `tibble` with the original rows and columns of `data`, plus:

- `is_medical_device`: Character (`"yes"`, `"no"`, or `NA_character_`).

- `device_class`: Character (`"Class I"`, `"Class IIa"`, `"Class IIb"`,
  `"Class III"`, `"IVD (Class A)"`, `"IVD (Class B)"`,
  `"IVD (Class C)"`, `"IVD (Class D)"`, or `NA_character_` if not a
  device or failed).

- `fda_medical_speciality`: Character (one of the 19 FDA medical
  specialities, or `NA_character_` if not a device or failed).

- `mhra_medical_speciality`: Character (one of the 11 MHRA medical
  specialities, or `NA_character_` if not a device or failed).

- `ema_medical_speciality`: Character (one of the 13 EMA medical
  specialities, or `NA_character_` if not a device or failed).

- `emdn_category`: Character (one of the 23 EMDN top-level categories or
  `"Other"`, or `NA_character_` if not a device or failed).

- `is_implantable`: Character (`"yes"`, `"no"`, or `NA_character_` if
  not a device or failed).

- `is_active`: Character (`"yes"`, `"no"`, or `NA_character_` if not a
  device or failed).

- `is_software`: Character (`"yes"`, `"no"`, or `NA_character_` if not a
  device or failed).

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ellmer)

devices <- tibble(
  device_id = 1:5,
  device_name = c(
    "Medtronic Micra AV",
    "Stryker Triathlon CR Femoral Component",
    "FreeStyle Optium blood glucose test strips",
    "BD Alaris GP Volumetric Pump",
    "Diprobase cream"
  )
)

# Using a cloud model (e.g., Google Gemini or OpenAI)
chat <- chat_google_gemini(model = "gemini-2.5-flash")
res <- classifyMedicalDevices(chat, devices)

# Or using a local model via Ollama
local_chat <- chat_ollama(
  model = "qwen2.5:7b",
  api_args = list(temperature = 0)
)
res_local <- classifyMedicalDevices(
  local_chat,
  devices,
  batchSize = 1L
)
} # }
```
