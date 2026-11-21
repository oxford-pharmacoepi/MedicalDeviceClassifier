# Classify medical devices

Uses an ellmer `Chat` object (e.g.,
[`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html)
or
[`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html))
to screen and classify unique `device_name` entries:

- **Is a device:** Flags whether each item is a true medical device
  (`"yes"` or `"no"`), excluding non-device dm+d products such as
  creams, pharmaceuticals, and nutritional feeds.

- **Regulatory:** Assigns true medical devices to an EU/UK MDR risk
  class (`Class I`, `Class IIa`, `Class IIb`, or `Class III`).

- **Medical speciality:** Assigns true medical devices to an EU/UK MDR
  risk class (`Anesthesiology`, `Cardiovascular`, `Chemistry`, `Dental`,
  `Ear, Nose, and Throat (ENT)`, `Gastroenterology and Urology`,
  `General and Plastic Surgery`, `General Hospital`, `Hematology`,
  `Immunology`, `Neurology`, `Obstetrical and Gynecological`,
  `Ophthalmic`, `Orthopedic`, `Pathology`, `Physical Medicine`,
  `Radiology`, `Toxicology`).

## Usage

``` r
classify_medical_devices(
  chat,
  data,
  max_retries = 3L,
  batch_size = 20L,
  show_cost = TRUE
)
```

## Arguments

- chat:

  An ellmer `Chat` object (such as from
  `ellmer::chat_openai(model = "gpt-4o-mini")` or
  `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).

- data:

  A `data.frame` or `tibble` containing a `device_name` column.

- max_retries:

  Integer scalar. Maximum number of attempts for any concept before
  returning `NA_character_`. Defaults to `3L`.

- batch_size:

  Integer scalar. Number of unique device names to classify per LLM
  request. Defaults to `20L`.

- show_cost:

  Logical scalar. Whether to track and display cumulative API costs via
  `chat$get_cost()` during execution. Defaults to `TRUE`.

## Value

A `tibble` with the original rows and columns of `data`, plus:

- `is_medical_device`: Character (`"yes"`, `"no"`, or `NA_character_`).

- `device_class`: Character (`"Class I"`, `"Class IIa"`, `"Class IIb"`,
  `"Class III"`, or `NA_character_` if not a device or failed).

- `medical_specialty`: Character (one of the 17 standard medical
  specialties, or `NA_character_` if not a device or failed).
