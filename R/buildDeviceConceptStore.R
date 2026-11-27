#' Build a Ragnar Embedding Store for Standard OMOP Device Concepts
#'
#' Extracts valid, standard concepts in the `"Device"` domain from an OMOP CDM
#' reference (for use with the `device_exposure` table) along with synonyms,
#' non-standard `"Maps to"` source terms, and first-level parent context, then
#' builds a searchable `ragnar` vector index.
#'
#' @param cdm A `cdm_reference` object.
#' @param location Character scalar specifying the DuckDB file path for the
#'   store, or `":memory:"` for an in-memory store.
#' @param embed An embedding function passed to [ragnar::ragnar_store_create()],
#'   such as [ragnar::embed_ollama()] or [ragnar::embed_openai()].
#' @param overwrite Logical. If `TRUE`, overwrites an existing store at
#'   `location`. Defaults to `FALSE`.
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
                                    overwrite = FALSE) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  device_concepts <- getStandardDevicesTbl(
    cdm = cdm
  ) |>
    dplyr::mutate(
      text = dplyr::if_else(
        !is.na(.data$parent_context),
        paste0(.data$text, " (", .data$parent_context, ")"),
        .data$text
      )
    ) |>
    dplyr::select(
      "text",
      "concept_id",
      "standard_concept_name",
      "vocabulary_id",
      "concept_class_id",
      "source_type"
    )

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

  ragnar::ragnar_store_insert(store, device_concepts)
  ragnar::ragnar_store_build_index(store)

  store
}
getStandardDevicesTbl <- function(cdm) {
  standard_devices_lazy <- cdm$concept |>
    dplyr::filter(
      .data$domain_id == "Device",
      .data$standard_concept == "S",
      is.na(.data$invalid_reason)
    ) |>
    dplyr::select(
      "concept_id",
      standard_concept_name = "concept_name",
      "vocabulary_id",
      "concept_class_id"
    ) |>
    dplyr::compute()

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

  parents <- cdm$concept_ancestor |>
    dplyr::filter(.data$min_levels_of_separation == 1L) |>
    dplyr::semi_join(
      device_ids,
      by = dplyr::join_by("descendant_concept_id" == "concept_id")
    ) |>
    dplyr::inner_join(
      dplyr::select(cdm$concept, "concept_id", parent_name = "concept_name"),
      by = dplyr::join_by("ancestor_concept_id" == "concept_id")
    ) |>
    dplyr::distinct(.data$descendant_concept_id, .data$parent_name) |>
    dplyr::collect() |>
    dplyr::summarise(
      parent_context = paste(unique(.data$parent_name), collapse = "; "),
      .by = "descendant_concept_id"
    ) |>
    dplyr::transmute(
      concept_id = as.integer(.data$descendant_concept_id),
      parent_context = as.character(.data$parent_context)
    )

  synonyms <- cdm$concept_synonym |>
    dplyr::semi_join(device_ids, by = "concept_id") |>
    dplyr::select("concept_id", text = "concept_synonym_name") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::transmute(
      concept_id = as.integer(.data$concept_id),
      text = as.character(.data$text),
      source_type = "synonym"
    )

  maps_to <- cdm$concept_relationship |>
    dplyr::filter(
      .data$relationship_id == "Maps to",
      is.na(.data$invalid_reason),
      .data$concept_id_1 != .data$concept_id_2
    ) |>
    dplyr::semi_join(
      device_ids,
      by = dplyr::join_by("concept_id_2" == "concept_id")
    ) |>
    dplyr::inner_join(
      dplyr::select(cdm$concept, "concept_id", text = "concept_name"),
      by = dplyr::join_by("concept_id_1" == "concept_id")
    ) |>
    dplyr::select(concept_id = "concept_id_2", "text") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::transmute(
      concept_id = as.integer(.data$concept_id),
      text = as.character(.data$text),
      source_type = "maps_to"
    )

  terms <- dplyr::bind_rows(
    dplyr::transmute(
      standard_devices,
      .data$concept_id,
      text = .data$standard_concept_name,
      source_type = "standard"
    ),
    synonyms,
    maps_to
  ) |>
    dplyr::filter(!is.na(.data$text), nzchar(.data$text)) |>
    dplyr::distinct(.data$concept_id, .data$text, .keep_all = TRUE)

  standard_devices |>
    dplyr::inner_join(terms, by = "concept_id") |>
    dplyr::left_join(parents, by = "concept_id")
}
