# Evaluate an LLM on the medical device classification benchmark

Uses the vitals package to evaluate an ellmer `Chat` model's accuracy
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
evaluateDeviceClassifier(
  chat,
  data,
  batchSize = 1L,
  maxRetries = 3L,
  name = "medical_device_classification",
  view = FALSE
)
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

  Integer scalar passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

- maxRetries:

  Integer scalar passed to
  [`classifyMedicalDevices()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/classifyMedicalDevices.md).

- name:

  Character scalar prefix for the vitals `Task` names (e.g.,
  `"gemini-2.5-flash"`). Defaults to `"medical_device_classification"`.

- view:

  Logical scalar. Whether to open the vitals Inspect log viewer for the
  `is_medical_device` task after evaluation. Defaults to `FALSE`.

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

# Per-column accuracy table
eval_res$summary

# Combine all 5 vitals tasks into one tibble
do.call(vitals_bind, eval_res$tasks)

# Inspect errors for a specific column
eval_res$tasks$device_class$get_samples()
} # }
```
