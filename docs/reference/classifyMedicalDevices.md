# Classify medical devices

Uses an ellmer `Chat` object (e.g.,
[`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html)
or
[`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html))
to screen and classify unique `device_name` entries:

- **Is a device:** Flags whether each item is truly a medical device
  (`"yes"` or `"no"`), excluding non-device products such as creams,
  pharmaceuticals, and nutritional feeds.

- **Class:** Assigns true medical devices to an EU/UK MDR risk class
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

- **Implantable & active status:** Flags whether true medical devices
  are implantable (`"yes"` or `"no"`) and whether they are
  active/powered devices (`"yes"` or `"no"`).

- **Confidence:** Reports classification confidence (`"high"`,
  `"medium"`, or `"low"`) for every evaluated item to support targeted
  manual review.

## Usage

``` r
classifyMedicalDevices(
  chat,
  data,
  maxRetries = 3L,
  batchSize = 20L,
  showCost = TRUE
)
```

## Arguments

- chat:

  An ellmer `Chat` object (such as from
  `ellmer::chat_openai(model = "gpt-4o-mini")` or
  `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).

- data:

  A `data.frame` or `tibble` containing a `device_name` column.

- maxRetries:

  Integer scalar. Maximum number of attempts for any concept before
  returning `NA_character_`. Defaults to `3L`.

- batchSize:

  Integer scalar. Number of unique device names to classify per LLM
  request. Defaults to `20L`.

- showCost:

  Logical scalar. Whether to track and display cumulative API costs via
  `chat$get_cost()` during execution. Defaults to `TRUE`.

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
