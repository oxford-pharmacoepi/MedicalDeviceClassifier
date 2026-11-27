# Map free-text medical device names to standard OMOP concept IDs

Queries a ragnar vector store of standard OMOP Device concepts to map
each `device_name` in `data` to a single standard OMOP `concept_id`.

- Any device name with a top retrieval hit within `maxCosineDistance` is
  mapped automatically.

- For all remaining device names without an automatic match:

  - When `chat` is `NULL`, they are mapped to `0L`.

  - When an ellmer `Chat` object is supplied and `expandQueries` is
    `TRUE`, the LLM first translates brand names or abbreviations into a
    generic clinical device description (flagging non-devices directly
    as `0L`), retrieves additional candidates using that description,
    and then selects the single best `concept_id` (or `0L`) from the
    combined candidate list.

## Usage

``` r
mapDeviceConcepts(
  data,
  store,
  chat = NULL,
  maxCosineDistance = 0.15,
  topK = 5L,
  expandQueries = TRUE,
  maxRetries = 3L
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing a `device_name` column which
  will be used to map devices.

- store:

  A `RagnarStore` object created by
  [`buildDeviceConceptStore()`](https://oxford-pharmacoepi.github.io/MedicalDeviceClassifier/reference/buildDeviceConceptStore.md)
  or opened via
  [`ragnar::ragnar_store_connect()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.html).

- chat:

  Optional ellmer `Chat` object (such as from
  `ellmer::chat_openai(model = "gpt-4o-mini")` or
  `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`)
  used to review any device names not automatically matched within
  `maxCosineDistance`. Defaults to `NULL` (retrieval-only).

- maxCosineDistance:

  Numeric. Maximum `cosine_distance` required for an automatic match
  (lower is stricter). Defaults to `0.15`.

- topK:

  Integer. Maximum number of unique candidate concepts to retrieve per
  search pass (raw term and, if enabled, expanded description) for LLM
  review. Defaults to `5L`.

- expandQueries:

  Logical. If `TRUE` (default) and `chat` is provided, uses `chat` to
  generate a generic clinical description for unmatched terms and runs a
  second retrieval pass before final selection.

- maxRetries:

  Integer. Maximum number of LLM attempts per step before falling back.
  Defaults to `3L`.

## Value

A `tibble` with the original rows and columns of `data`, plus:

- `concept_id`: Integer standard OMOP `concept_id` (`0L` if unmapped or
  no confident match is found).

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ellmer)

devices <- tibble(
  device_id = 1:3,
  device_name = c(
    "Medtronic Micra AV",
    "drug-eluting coronary stent",
    "Diprobase cream"
  )
)

local_chat <- chat_ollama(
  model = "qwen2.5:7b",
  api_args = list(temperature = 0)
)

res <- mapDeviceConcepts(
  data = devices,
  store = store,
  chat = local_chat,
  maxCosineDistance = 0.15,
  expandQueries = TRUE
)
} # }
```
