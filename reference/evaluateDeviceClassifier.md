# Evaluate an LLM on the medical device classification benchmark

Uses the vitals package to evaluate a ellmer `chat` model's accuracy
with
[`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md)
against a labelled benchmark dataset.

Runs
[`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md)
once and produces a separate vitals `Task` and accuracy score (`"C"` =
Correct, `"I"` = Incorrect) for each of the five classification columns:

- `is_medical_device`: Scored across all rows in `data`.

- `device_class`, `medical_speciality`, `is_implantable`, `is_active`:
  Scored across true medical devices (`expected_is_device == "yes"`).

## Usage

``` r
evaluateDeviceClassifier(chat, data, batchSize = 1L, maxRetries = 3L)
```

## Arguments

- chat:

  An ellmer `Chat` object to evaluate.

- data:

  A labelled benchmark `data.frame` or `tibble` containing
  `device_name`, `expected_is_device`, `expected_class`,
  `expected_speciality`, `expected_is_implantable`, and
  `expected_is_active`.

- batchSize:

  Integer passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

- maxRetries:

  Integer passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

## Value

A list (invisibly) with three elements:

- `summary`: A `tibble` with `column`, `n_evaluated`, `n_correct`, and
  `accuracy` (percentage) for each of the five columns.

- `tasks`: A named list of five evaluated vitals `Task` objects
  (`is_medical_device`, `device_class`, `medical_speciality`,
  `is_implantable`, `is_active`). Use
  [`vitals::vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html)
  to combine them or `res$tasks$device_class$view()` to inspect a
  specific column.

- `results`: A `tibble` combining `data` with the model's predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)
library(vitals)

chat <- chat_google_gemini(model = "gemini-2.5-flash")
eval_res <- evaluate_device_classifier(chat, name = "gemini-2.5-flash")
eval_res$summary
} # }
```
