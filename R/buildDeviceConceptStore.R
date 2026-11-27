#' Build a Ragnar Embedding Store for Standard OMOP Device Concepts
#'
#' Extracts valid, standard concepts in the `"Device"` domain from an OMOP CDM
#' reference (for use with the `device_exposure` table) along with synonyms and
#'then builds a searchable `ragnar` vector index.
#'
#' @param cdm A `cdm_reference` object.
#' @param location Character scalar specifying the DuckDB file path for the
#'   store, or `":memory:"` for an in-memory store.
#' @param embed An embedding function passed to [ragnar::ragnar_store_create()],
#'   such as [ragnar::embed_ollama()] or [ragnar::embed_openai()].
#' @param overwrite Logical. If `TRUE`, overwrites an existing store at
#'   `location`. Defaults to `FALSE`.
#' @param targetVocab Standard vocabulary IDs to include
#'
#' @return A `RagnarStore` object indexed for retrieval.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' store <- buildDeviceConceptStore(
#'   cdm = cdm,
#'   location = "omop_devices.ragnar.duckdb",
#'   embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
#' )
#' }
buildDeviceConceptStore <- function(cdm,
                                    location = ":memory:",
                                    embed = ragnar::embed_ollama(),
                                    overwrite = FALSE,
                                    targetVocab = "SNOMED") {

  tictoc::tic("buildDeviceConceptStore (total)")
  on.exit(tictoc::toc(), add = TRUE)

  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  tictoc::tic("Extract device concepts from CDM")
  device_concepts <- getStandardDevicesTbl(
    cdm = cdm,
    vocabId = targetVocab) |>
    dplyr::select(
      "text",
      "concept_id",
      "standard_concept_name",
      "vocabulary_id",
      "concept_class_id",
      "source_type"
    )
  tictoc::toc()

  extra_cols <- tibble::tibble(
    concept_id = integer(),
    standard_concept_name = character(),
    vocabulary_id = character(),
    concept_class_id = character(),
    source_type = character()
  )

  store <- ragnar::ragnar_store_create(
    location = location,
    embed = embed,
    overwrite = overwrite,
    extra_cols = extra_cols,
    version = 1L
  )

  tictoc::tic("Embed and insert concepts into Ragnar store")
  n_rows <- nrow(device_concepts)
  batch_indices <- split(seq_len(n_rows), ceiling(seq_len(n_rows) / 500))
  n_batches <- length(batch_indices)

  cli::cli_progress_bar(
    name = "Embedding & inserting device concepts",
    total = n_batches,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} batches ({cli::pb_percent}) | ETA: {cli::pb_eta}"
  )

  for (idx in batch_indices) {
    ragnar::ragnar_store_insert(store, device_concepts[idx, , drop = FALSE])
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  tictoc::toc()

  tictoc::tic("Build Ragnar search index")
  ragnar::ragnar_store_build_index(store)
  tictoc::toc()

  store
}

getStandardDevicesTbl <- function(cdm, vocabId) {
  standard_devices_lazy <- cdm$concept |>
    dplyr::filter(
      .data$domain_id == "Device",
      .data$standard_concept == "S",
      is.na(.data$invalid_reason),
      vocabulary_id %in% vocabId
    ) |>
    dplyr::select(
      "concept_id",
      standard_concept_name = "concept_name",
      "vocabulary_id",
      "concept_class_id"
    )

  standard_devices <- standard_devices_lazy |>
    dplyr::collect() |>
    dplyr::mutate(
      concept_id = as.integer(.data$concept_id),
      standard_concept_name = as.character(.data$standard_concept_name),
      vocabulary_id = as.character(.data$vocabulary_id),
      concept_class_id = as.character(.data$concept_class_id)
    )

  n_standard_devices <- nrow(standard_devices)
  if (n_standard_devices == 0L) {
    cli::cli_abort("No standard device concepts found in the CDM reference.")
  }
  cli::cli_inform("{n_standard_devices} standard device concepts found in the CDM reference.")

  device_ids <- dplyr::select(standard_devices_lazy, "concept_id")

  synonyms <- cdm$concept_synonym |>
    dplyr::filter(.data$language_concept_id == 4180186L) |>
    dplyr::semi_join(device_ids, by = "concept_id") |>
    dplyr::select("concept_id", text = "concept_synonym_name") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::transmute(
      concept_id = as.integer(.data$concept_id),
      text = as.character(.data$text),
      source_type = "synonym"
    )

  terms <- dplyr::bind_rows(
    dplyr::transmute(
      standard_devices,
      .data$concept_id,
      text = .data$standard_concept_name,
      source_type = "standard"
    ),
    synonyms
  ) |>
    dplyr::mutate(
      text = cleanSnomedDeviceName(.data$text),
      text_norm = tolower(.data$text)
    ) |>
    dplyr::filter(!is.na(.data$text), nzchar(.data$text)) |>
    dplyr::distinct(.data$concept_id, .data$text_norm, .keep_all = TRUE) |>
    dplyr::select(-"text_norm")

  standard_devices |>
    dplyr::inner_join(terms, by = "concept_id")
}

cleanSnomedDeviceName <- function(x) {
  snomed_tag_regex <- paste0(
    "\\s*\\((?:physical object|device|substance|product|procedure|",
    "finding|disorder|observable entity|qualifier value|body structure)\\)$"
  )

  x |>
    as.character() |>
    stringr::str_remove(snomed_tag_regex) |>
    stringr::str_squish()
}
