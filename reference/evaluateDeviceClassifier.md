# Evaluate an LLM on the medical device classification benchmark

Uses the vitals package to evaluate an ellmer `Chat` model's accuracy
with
[`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md)
against a labelled benchmark dataset.

Runs
[`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md)
once and produces a separate vitals `Task` and accuracy score (`"C"` =
Correct, `"I"` = Incorrect) for each of the nine classification columns:

- `is_medical_device`: Scored across all rows in `data`.

- `device_class`, `fda_medical_speciality`, `mhra_medical_speciality`,
  `ema_medical_speciality`, `emdn_category`, `is_implantable`,
  `is_active`, `is_software`: Scored across true medical devices
  (`expected_is_medical_device == "yes"`).

## Usage

``` r
evaluateDeviceClassifier(chat, data, batchSize = 1L, maxRetries = 3L)
```

## Arguments

- chat:

  An ellmer `Chat` object to evaluate.

- data:

  A labelled benchmark `data.frame` or `tibble` containing
  `device_name`, `expected_is_medical_device`, `expected_device_class`,
  `expected_fda_medical_speciality`, `expected_mhra_medical_speciality`,
  `expected_ema_medical_speciality`, `expected_emdn_category`,
  `expected_is_implantable`, `expected_is_active`, and
  `expected_is_software`.

- batchSize:

  Integer passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

- maxRetries:

  Integer passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

## Value

A list (invisibly) with three elements:

- `summary`: A `tibble` with `column`, `n_evaluated`, `n_correct`, and
  `accuracy` (percentage) for each of the nine columns.

- `tasks`: A named list of nine evaluated vitals `Task` objects
  (`is_medical_device`, `device_class`, `fda_medical_speciality`,
  `mhra_medical_speciality`, `ema_medical_speciality`, `emdn_category`,
  `is_implantable`, `is_active`, `is_software`). Use
  [`vitals::vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html)
  to combine them or `res$tasks$device_class$view()` to inspect a
  specific column.

- `results`: A `tibble` combining `data` with the model's predictions
  and a logical `any_incorrect` flag (`TRUE` if any evaluated column for
  that row did not match the expected value).

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)
library(vitals)

chat <- chat_google_gemini(model = "gemini-2.5-flash")
eval_res <- evaluateDeviceClassifier(chat, benchmark_data, batchSize = 20L)
eval_res$summary
dplyr::filter(eval_res$results, any_incorrect)
} # }
```
