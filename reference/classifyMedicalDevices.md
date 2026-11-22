# Classify medical devices

Uses an ellmer `Chat` object (e.g.,
[`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html)
or
[`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html))
to classify device data on the following:

- **Is a device:** Flags whether each item is truly a medical device
  (`"yes"` or `"no"`), excluding non-device products such as creams,
  pharmaceuticals, and nutritional feeds.

- **Class:** Assigns medical devices to an EU/UK MDR risk class
  (`Class I`, `Class IIa`, `Class IIb`, `Class III`) or EU/UK IVDR risk
  class (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`,
  `IVD (Class D)`).

- **Medical speciality:** Assigns true medical devices to a clinical
  speciality (`Anesthesiology`, `Cardiovascular`, `Chemistry`, `Dental`,
  `Ear, Nose, and Throat (ENT)`, `Gastroenterology and Urology`,
  `General and Plastic Surgery`, `General Hospital`, `Hematology`,
  `Immunology`, `Neurology`, `Obstetrical and Gynecological`,
  `Ophthalmic`, `Orthopedic`, `Pathology`, `Physical Medicine`,
  `Radiology`, `Toxicology`, or `Other`).

- **Implantable:** Flags whether medical devices are implantable
  (`"yes"` or `"no"`).

- **Active status:** Flags whether medical devices are active/powered
  (`"yes"` or `"no"`).

- **Confidence:** Classification confidence (`"high"`, `"medium"`, or
  `"low"`) for each evaluated item.

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
  `NA_character_`

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

- `medical_speciality`: Character (one of the 19 medical specialities,
  or `NA_character_` if not a device or failed).

- `is_implantable`: Character (`"yes"`, `"no"`, or `NA_character_` if
  not a device or failed).

- `is_active`: Character (`"yes"`, `"no"`, or `NA_character_` if not a
  device or failed).

- `confidence`: Character (`"high"`, `"medium"`, `"low"`, or
  `NA_character_` if failed).

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
