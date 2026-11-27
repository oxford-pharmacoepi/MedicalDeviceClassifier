#' Map free-text medical device names to standard OMOP concept IDs
#'
#' @description
#' Queries a \pkg{ragnar} vector store of standard OMOP Device concepts to map
#' each `device_name` in `data` to a single standard OMOP `concept_id`.
#'
#' * Any device name with a top retrieval hit within `maxCosineDistance` is
#'   mapped automatically.
#' * For all remaining device names without an automatic match:
#'   - When `chat` is `NULL`, they are mapped to `0L`.
#'   - When an \pkg{ellmer} `Chat` object is supplied and `expandQueries` is
#'     `TRUE`, the LLM first translates brand names or abbreviations into a
#'     generic clinical device description (flagging non-devices directly as
#'     `0L`), retrieves additional candidates using that description, and then
#'     selects the single best `concept_id` (or `0L`) from the combined
#'     candidate list.
#'
#' @param data A `data.frame` or `tibble` containing a `device_name` column
#'   which will be used to map devices.
#' @param store A `RagnarStore` object created by [buildDeviceConceptStore()]
#'   or opened via [ragnar::ragnar_store_connect()].
#' @param chat Optional \pkg{ellmer} `Chat` object (such as from
#'   `ellmer::chat_openai(model = "gpt-4o-mini")` or
#'   `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`)
#'   used to review any device names not automatically matched within
#'   `maxCosineDistance`. Defaults to `NULL` (retrieval-only).
#' @param maxCosineDistance Numeric. Maximum `cosine_distance` required for an
#'   automatic match (lower is stricter). Defaults to `0.15`.
#' @param topK Integer. Maximum number of unique candidate concepts to retrieve
#'   per search pass (raw term and, if enabled, expanded description) for LLM
#'   review. Defaults to `5L`.
#' @param expandQueries Logical. If `TRUE` (default) and `chat` is provided,
#'   uses `chat` to generate a generic clinical description for unmatched terms
#'   and runs a second retrieval pass before final selection.
#' @param maxRetries Integer. Maximum number of LLM attempts per step before
#'   falling back. Defaults to `3L`.
#'
#' @return A `tibble` with the original rows and columns of `data`, plus:
#'   \itemize{
#'     \item `concept_id`: Integer standard OMOP `concept_id` (`0L` if unmapped
#'       or no confident match is found).
#'   }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ellmer)
#'
#' devices <- tibble(
#'   device_id = 1:3,
#'   device_name = c(
#'     "Medtronic Micra AV",
#'     "drug-eluting coronary stent",
#'     "Diprobase cream"
#'   )
#' )
#'
#' local_chat <- chat_ollama(
#'   model = "qwen2.5:7b",
#'   api_args = list(temperature = 0)
#' )
#'
#' res <- mapDeviceConcepts(
#'   data = devices,
#'   store = store,
#'   chat = local_chat,
#'   maxCosineDistance = 0.15,
#'   expandQueries = TRUE
#' )
#' }
#' @importFrom rlang .data
#' @export
mapDeviceConcepts <- function(data,
                              store,
                              chat = NULL,
                              maxCosineDistance = 0.15,
                              topK = 5L,
                              expandQueries = TRUE,
                              maxRetries = 3L) {
  if (!is.data.frame(data) || !"device_name" %in% names(data)) {
    cli::cli_abort(
      "{.arg data} must be a data.frame or tibble containing a {.col device_name} column."
    )
  }

  if ("concept_id" %in% names(data)) {
    cli::cli_abort(
      "{.arg data} already contains output column {.col concept_id}. Rename or remove it before mapping."
    )
  }

  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be {.val NULL} or an {.pkg ellmer} {.cls Chat} object.",
      "i" = "Create one with e.g. {.code ellmer::chat_ollama(model = \"qwen2.5:7b\")}."
    ))
  }

  if (!is.numeric(maxCosineDistance) || length(maxCosineDistance) != 1L ||
      is.na(maxCosineDistance) || maxCosineDistance < 0) {
    cli::cli_abort("{.arg maxCosineDistance} must be a single non-negative number.")
  }

  topK <- as.integer(topK)
  if (length(topK) != 1L || is.na(topK) || topK < 1L) {
    cli::cli_abort("{.arg topK} must be a single positive integer.")
  }

  if (!is.logical(expandQueries) || length(expandQueries) != 1L || is.na(expandQueries)) {
    cli::cli_abort("{.arg expandQueries} must be {.val TRUE} or {.val FALSE}.")
  }

  maxRetries <- as.integer(maxRetries)
  if (length(maxRetries) != 1L || is.na(maxRetries) || maxRetries < 1L) {
    cli::cli_abort("{.arg maxRetries} must be a single positive integer.")
  }

  work_df <- dplyr::as_tibble(data) |>
    dplyr::mutate(..clean_device_name.. = stringr::str_squish(as.character(.data$device_name)))

  unique_concepts <- work_df |>
    dplyr::filter(!is.na(.data$..clean_device_name..), .data$..clean_device_name.. != "") |>
    dplyr::distinct(.data$..clean_device_name..) |>
    dplyr::pull(.data$..clean_device_name..)

  n_total <- nrow(work_df)
  n_unique <- length(unique_concepts)

  if (n_unique == 0L) {
    cli::cli_alert_warning(
      "No non-empty {.col device_name} values found in {.arg data}. Returning {.val 0L} for {.col concept_id}."
    )
    return(
      work_df |>
        dplyr::select(-"..clean_device_name..") |>
        dplyr::mutate(concept_id = 0L)
    )
  }

  retrieve_candidates <- function(text, k) {
    res <- ragnar::ragnar_retrieve(store, text = text, top_k = k * 3L)
    if (nrow(res) == 0L || !"concept_id" %in% names(res)) {
      return(tibble::tibble())
    }
    if (!"cosine_distance" %in% names(res)) {
      res$cosine_distance <- NA_real_
    }
    if (!"bm25" %in% names(res)) {
      res$bm25 <- NA_real_
    }
    res |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::arrange(.data$cosine_distance, dplyr::desc(.data$bm25)) |>
      dplyr::distinct(.data$concept_id, .keep_all = TRUE) |>
      dplyr::slice_head(n = k)
  }

  cli::cli_alert_info(
    "Retrieving candidates for {.val {n_unique}} unique concept{?s} across {.val {n_total}} row{?s}."
  )

  raw_candidates <- dplyr::tibble(..clean_device_name.. = unique_concepts) |>
    dplyr::mutate(
      cands = purrr::map(.data$..clean_device_name.., \(x) retrieve_candidates(x, k = topK))
    )

  auto_accepted <- raw_candidates |>
    tidyr::unnest("cands") |>
    dplyr::slice_head(n = 1L, by = "..clean_device_name..") |>
    dplyr::filter(
      !is.na(.data$cosine_distance),
      .data$cosine_distance <= maxCosineDistance
    ) |>
    dplyr::transmute(
      .data$..clean_device_name..,
      concept_id = as.integer(.data$concept_id)
    )

  if (is.null(chat)) {
    lookup <- auto_accepted
  } else {
    to_review <- raw_candidates |>
      dplyr::anti_join(auto_accepted, by = "..clean_device_name..")

    n_review <- nrow(to_review)

    if (n_review == 0L) {
      cli::cli_alert_info(
        "All {.val {nrow(auto_accepted)}} concept{?s} matched within {.arg maxCosineDistance}; skipping LLM."
      )
      lookup <- auto_accepted
    } else {
      cli::cli_alert_info(
        "Auto-accepted {.val {nrow(auto_accepted)}} concept{?s}; sending {.val {n_review}} unmatched concept{?s} to LLM (expandQueries: {.val {expandQueries}}, max retries: {.val {maxRetries}})."
      )

      describe_chat <- chat$clone()
      describe_chat$set_turns(list())
      describe_chat$set_system_prompt(.device_describer_system_prompt())

      describe_schema <- ellmer::type_object(
        "Generic clinical device description for vector search.",
        status = ellmer::type_enum(
          values = c("device", "not_a_device", "unknown"),
          description = "'device' if recognised as a medical device, 'not_a_device' if clearly a drug, cream, food, or non-device, or 'unknown' if unrecognisable."
        ),
        description = ellmer::type_string(
          description = "Concise generic clinical device name/description without brand names (e.g., 'leadless implantable cardiac pacemaker'), or empty string if status is not 'device'."
        )
      )

      select_chat <- chat$clone()
      select_chat$set_turns(list())
      select_chat$set_system_prompt(.device_mapper_system_prompt())

      extract_call_cost <- function(ch) {
        if (!is.function(ch$get_cost)) {
          return(NA_real_)
        }
        cost_val <- tryCatch(
          as.numeric(ch$get_cost(include = "all")),
          error = function(e) NA_real_
        )
        if (length(cost_val) == 1L && !is.na(cost_val)) cost_val else NA_real_
      }

      format_cost <- function(cost_num) {
        if (is.na(cost_num)) {
          return("N/A")
        }
        if (cost_num > 0 && cost_num < 0.01) {
          return(sprintf("$%.4f", cost_num))
        }
        sprintf("$%.2f", cost_num)
      }

      out_ids <- integer(n_review)
      n_errors <- 0L
      total_cost <- 0
      any_cost_available <- FALSE
      cost_str <- "$0.00"

      add_cost <- function(ch) {
        attempt_cost <- extract_call_cost(ch)
        if (!is.na(attempt_cost)) {
          total_cost <<- total_cost + attempt_cost
          any_cost_available <<- TRUE
          cost_str <<- format_cost(total_cost)
        } else if (!any_cost_available) {
          cost_str <<- "N/A"
        }
      }

      pb_format <- paste0(
        "{cli::pb_bar} {cli::pb_percent} | ",
        "{cli::pb_current}/{cli::pb_total} | ",
        "{cli::pb_extra$cost} | ",
        "Err: {cli::pb_extra$errors} | ",
        "ETA: {cli::pb_eta}"
      )

      pb_id <- cli::cli_progress_bar(
        total = n_review,
        format = pb_format,
        extra = list(cost = cost_str, errors = n_errors),
        clear = TRUE,
        auto_terminate = TRUE
      )

      on.exit(
        cli::cli_progress_done(id = pb_id, result = "clear"),
        add = TRUE
      )

      for (i in seq_len(n_review)) {
        q <- to_review$..clean_device_name..[[i]]
        cands <- to_review$cands[[i]]
        generated_desc <- NA_character_
        skip_selection <- FALSE

        if (expandQueries) {
          for (attempt in seq_len(maxRetries)) {
            ch_desc <- describe_chat$clone()
            res_desc <- tryCatch(
              ch_desc$chat_structured(
                paste0("Device term: ", q),
                type = describe_schema,
                echo = "none"
              ),
              error = function(e) NULL
            )
            add_cost(ch_desc)

            if (is.list(res_desc) && !is.null(res_desc$status)) {
              st <- tolower(as.character(res_desc$status[[1]]))
              if (st == "not_a_device") {
                out_ids[[i]] <- 0L
                skip_selection <- TRUE
                break
              }
              if (st == "device" && !is.null(res_desc$description)) {
                desc_txt <- stringr::str_squish(as.character(res_desc$description[[1]]))
                if (!is.na(desc_txt) && nzchar(desc_txt)) {
                  generated_desc <- desc_txt
                  expanded_cands <- retrieve_candidates(generated_desc, k = topK)
                  if (nrow(expanded_cands) > 0L) {
                    cands <- dplyr::bind_rows(expanded_cands, cands) |>
                      dplyr::distinct(.data$concept_id, .keep_all = TRUE)
                  }
                }
                break
              }
              if (st == "unknown") {
                break
              }
            }

            if (attempt < maxRetries) {
              Sys.sleep(0.5 * attempt)
            }
          }
        }

        if (!skip_selection) {
          if (nrow(cands) == 0L) {
            out_ids[[i]] <- 0L
          } else {
            valid_ids <- c(0L, unique(as.integer(cands$concept_id)))

            select_schema <- ellmer::type_object(
              "Selected OMOP standard concept ID for a medical device name.",
              reasoning = ellmer::type_string(
                description = "One brief sentence identifying the device type/component and why the chosen candidate (or '0') fits best."
              ),
              concept_id = ellmer::type_enum(
                values = as.character(valid_ids),
                description = "The single best matching OMOP concept_id from the candidate list, or '0' if none accurately match."
              )
            )

            label_col <- if ("standard_concept_name" %in% names(cands)) {
              cands$standard_concept_name
            } else {
              cands$text
            }

            candidate_lines <- paste0(
              "- ", cands$concept_id, ": ", label_col,
              " [", cands$vocabulary_id, " | matched: ", cands$text, "]"
            )

            desc_line <- if (!is.na(generated_desc)) {
              paste0("Inferred generic description: ", generated_desc, "\n")
            } else {
              ""
            }

            prompt_text <- paste0(
              "Device term: ", q, "\n",
              desc_line,
              "\nCandidates:\n", paste(candidate_lines, collapse = "\n")
            )

            picked <- NA_integer_
            for (attempt in seq_len(maxRetries)) {
              ch_sel <- select_chat$clone()
              res_sel <- tryCatch(
                ch_sel$chat_structured(prompt_text, type = select_schema, echo = "none"),
                error = function(e) NULL
              )
              add_cost(ch_sel)

              if (is.list(res_sel) && !is.null(res_sel$concept_id)) {
                val <- suppressWarnings(as.integer(res_sel$concept_id[[1]]))
                if (!is.na(val) && val %in% valid_ids) {
                  picked <- val
                  break
                }
              }

              if (attempt < maxRetries) {
                Sys.sleep(0.5 * attempt)
              }
            }

            if (is.na(picked)) {
              n_errors <- n_errors + 1L
              out_ids[[i]] <- 0L
            } else {
              out_ids[[i]] <- picked
            }
          }
        }

        cli::cli_progress_update(
          set = i,
          extra = list(cost = cost_str, errors = n_errors),
          id = pb_id,
          force = (i == n_review)
        )
      }

      cli::cli_progress_done(id = pb_id)
      on.exit(NULL)

      final_cost_msg <- if (any_cost_available) {
        paste0(" | Total cost: ", format_cost(total_cost))
      } else {
        ""
      }

      n_succeeded <- n_review - n_errors
      if (n_errors == 0L) {
        cli::cli_alert_success(
          "Reviewed {.val {n_review}}/{.val {n_review}} unmatched concept{?s} ({.val {n_succeeded}} succeeded, {.val 0} errors{final_cost_msg})."
        )
      } else {
        cli::cli_alert_warning(
          "Reviewed {.val {n_review}}/{.val {n_review}} unmatched concept{?s} ({.val {n_succeeded}} succeeded, {.val {n_errors}} error{?s} set to {.val 0L}{final_cost_msg})."
        )
      }

      llm_resolved <- dplyr::tibble(
        ..clean_device_name.. = to_review$..clean_device_name..,
        concept_id = out_ids
      )

      lookup <- dplyr::bind_rows(auto_accepted, llm_resolved)
    }
  }

  work_df |>
    dplyr::left_join(lookup, by = "..clean_device_name..") |>
    dplyr::mutate(concept_id = dplyr::coalesce(.data$concept_id, 0L)) |>
    dplyr::select(-"..clean_device_name..")
}


.device_describer_system_prompt <- function() {
  paste(
    "You are a clinical terminology specialist translating hospital procurement and EHR product names into generic medical device descriptions for vocabulary search.",
    "Given a device term:",
    "- If it is a recognised medical device (including brand names, model names, or clinical abbreviations), set status to 'device' and return a concise 3-8 word generic clinical description without brand or manufacturer names (e.g., 'Medtronic Micra AV' -> 'leadless implantable cardiac pacemaker').",
    "- If it is clearly not a medical device (e.g., pharmaceutical, cream, nutritional supplement, or general non-medical item), set status to 'not_a_device' and description to ''.",
    "- If the term is an unrecognisable internal code or you are not confident what it is, set status to 'unknown' and description to ''. Do not guess.",
    sep = "\n"
  )
}


.device_mapper_system_prompt <- function() {
  paste(
    "You are an expert clinical terminology specialist mapping free-text medical device names to standard OMOP CDM Device concepts.",
    "You will receive an original device term, an optional inferred generic description, and a list of candidate OMOP concepts.",
    "Select the single `concept_id` from the candidate list that best represents the original device term, or '0' if no candidate is suitable.",
    "",
    "Rules:",
    "- Ground truth: Base your decision on the original device term. Treat the inferred description as a hint only; ignore it if it contradicts or oversimplifies the original term.",
    "- Component vs. system: If the term names a specific component (e.g., screw, plate, femoral head, tibial insert), map to that component rather than the overarching implant system. Map to a system concept only when the term refers to a whole system or does not specify a component.",
    "- Specificity vs. unstated attributes: Prefer the most specific candidate whose clinical type and anatomical site match the term. Do NOT pick a candidate with a contradicting attribute (e.g., wrong anatomical site, anterior vs. posterior, bioabsorbable vs. metallic). If a more specific candidate includes a standard attribute that is neither confirmed nor contradicted (e.g., sterile, uncoated), prefer the accurate parent concept if one is listed; otherwise choose the closest clinically valid match.",
    "- When to return '0': Return '0' if the term is not a medical device (e.g., drug, cream, food), if the term is unrecognisable, or if every candidate is a different device type or wrong anatomical site. Do NOT return '0' merely because only a slightly broader valid parent concept is available.",
    "- Output: You MUST return either one of the exact `concept_id` values listed in the candidates, or '0'.",
    sep = "\n"
  )
}
