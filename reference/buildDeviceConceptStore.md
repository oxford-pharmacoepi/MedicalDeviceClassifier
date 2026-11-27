# Build a Ragnar Embedding Store for Standard OMOP Device Concepts

Extracts valid, standard concepts in the `"Device"` domain from an OMOP
CDM reference (for use with the `device_exposure` table) along with
synonyms and then builds a searchable `ragnar` vector index.

## Usage

``` r
buildDeviceConceptStore(
  cdm,
  location = ":memory:",
  embed = ragnar::embed_ollama(),
  overwrite = FALSE,
  targetVocab = "SNOMED"
)
```

## Arguments

- cdm:

  A `cdm_reference` object.

- location:

  Character scalar specifying the DuckDB file path for the store, or
  `":memory:"` for an in-memory store.

- embed:

  An embedding function passed to
  [`ragnar::ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.html),
  such as
  [`ragnar::embed_ollama()`](https://ragnar.tidyverse.org/reference/embed_ollama.html)
  or
  [`ragnar::embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.html).

- overwrite:

  Logical. If `TRUE`, overwrites an existing store at `location`.
  Defaults to `FALSE`.

- targetVocab:

  Standard vocabulary IDs to include

## Value

A `RagnarStore` object indexed for retrieval.

## Examples

``` r
if (FALSE) { # \dontrun{
store <- buildDeviceConceptStore(
  cdm = cdm,
  location = "omop_devices.ragnar.duckdb",
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
)
} # }
```
