#' Evaluate concept mapping on a labelled medical device benchmark
#'
#' @description
#' Uses the \pkg{vitals} package to evaluate [mapDeviceConcepts()] against a
#' labelled benchmark dataset with ground-truth OMOP `concept_id` values.
#'
#' Runs [mapDeviceConcepts()] once and produces a \pkg{vitals} `Task` with
#' exact-match accuracy (`"C"` = Correct, `"I"` = Incorrect) comparing predicted
#' `concept_id` values against the benchmark `concept_id` (renamed to
#' `expected_concept_id` in the returned results).
#'
#' Works with both LLM-assisted runs (`chat` supplied) and retrieval-only runs
#' (`chat = NULL`).
#'
#' @param data A labelled benchmark `data.frame` or `tibble` containing
#'   `concept_id`, `device_brand_name`, `device_manufacturer_name`, and
#'   `device_description`
#' @param store A `RagnarStore` object passed to [mapDeviceConcepts()].
#' @param chat Optional \pkg{ellmer} `Chat` object passed to
#'   [mapDeviceConcepts()]. Defaults to `NULL` (retrieval-only).
#' @param maxCosineDistance Numeric passed to [mapDeviceConcepts()].
#' @param topK Integer passed to [mapDeviceConcepts()].
#' @param expandQueries Logical passed to [mapDeviceConcepts()].
#' @param maxRetries Integer passed to [mapDeviceConcepts()].
#'
#' @return A list (invisibly) with three elements:
#'   \itemize{
#'     \item `summary`: A `tibble` with `subset`, `n_evaluated`, `n_correct`,
#'       `n_unmapped` (predicted `0L`), `n_wrong_mapping` (predicted a non-zero
#'       `concept_id` that did not match `expected_concept_id`), and `accuracy`
#'       (percentage). Includes `"overall"` and, when present, `"expected_mapped"`
#'       (`expected_concept_id != 0L`) and `"expected_unmapped"`
#'       (`expected_concept_id == 0L`).
#'     \item `task`: An evaluated \pkg{vitals} `Task` object. Use
#'       `res$task$view()` to inspect individual predictions or
#'       `vitals::vitals_bind()` to compare multiple models/configurations.
#'     \item `results`: A `tibble` combining `data` (with ground truth renamed
#'       to `expected_concept_id`), the constructed `device_name`, the predicted
#'       `concept_id`, and a logical `incorrect` flag.
#'   }
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(vitals)
#'
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' eval_res <- evaluateDeviceConceptMapper(
#'   data = benchmark_concepts,
#'   store = store,
#'   chat = chat
#' )
#' eval_res$summary
#' dplyr::filter(eval_res$results, incorrect)
#' }
#'
#' @importFrom rlang .data
#' @export
evaluateDeviceConceptMapper <- function(data,
                                        store,
                                        chat = NULL,
                                        includeManufacturer = TRUE,
                                        maxCosineDistance = 0.15,
                                        topK = 5L,
                                        expandQueries = TRUE,
                                        maxRetries = 3L) {
  rlang::check_installed(
    "vitals",
    reason = "to evaluate concept mapping with evaluateDeviceConceptMapper()."
  )
  vitals::vitals_log_dir_set(tempdir())

  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort(
      "{.arg chat} must be {.val NULL} or an {.pkg ellmer} {.cls Chat} object."
    )
  }

  if (!is.logical(includeManufacturer) || length(includeManufacturer) != 1L ||
      is.na(includeManufacturer)) {
    cli::cli_abort("{.arg includeManufacturer} must be {.val TRUE} or {.val FALSE}.")
  }

  req_cols <- c("concept_id", "device_brand_name",
                "device_manufacturer_name", "device_description")
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg data} is missing required benchmark column{?s}: {.col {missing_cols}}."
    )
  }

  name <- if (is.null(chat)) "retrieval_only" else chat$get_model()

 input_df <- dplyr::as_tibble(data) |>
    dplyr::rename(expected_concept_id = "concept_id") |>
    dplyr::mutate(
      expected_concept_id = as.integer(.data$expected_concept_id),
      device_name = purrr::pmap_chr(
        list(.data$device_brand_name, .data$device_manufacturer_name, .data$device_description),
        \(b, m, d) {
          vals <- stringr::str_squish(as.character(c(b, m, d)))
          paste(vals[!is.na(vals) & nzchar(vals)], collapse = " - ")
        }
      )
    )

  preds <- mapDeviceConcepts(
    data = input_df,
    store = store,
    chat = chat,
    maxCosineDistance = maxCosineDistance,
    topK = topK,
    expandQueries = expandQueries,
    maxRetries = maxRetries
  )

  exact_scorer <- function(samples) {
    match_vec <- !is.na(samples$result) &
      !is.na(samples$target) &
      samples$result == samples$target

    list(
      score = factor(
        ifelse(match_vec, "C", "I"),
        levels = c("I", "C"),
        ordered = TRUE
      )
    )
  }

  make_replay_solver <- function(cached_preds) {
    force(cached_preds)

    function(inputs, ..., solver_chat = NULL) {
      if (is.null(solver_chat)) {
        return(list(result = cached_preds))
      }

      chats <- Map(function(inp, pred) {
        ch <- solver_chat$clone()
        ch$set_turns(list(
          ellmer::Turn(
            role = "user",
            contents = list(ellmer::ContentText(as.character(inp)))
          ),
          ellmer::Turn(
            role = "assistant",
            contents = list(
              ellmer::ContentText(ifelse(is.na(pred), "NA", as.character(pred)))
            )
          )
        ))
        ch
      }, inputs, cached_preds)

      list(
        result = cached_preds,
        solver_chat = unname(chats)
      )
    }
  }

  eval_dataset <- preds |>
    dplyr::mutate(
      input = as.character(.data$device_name),
      target = as.character(.data$expected_concept_id)
    )

  tsk <- vitals::Task$new(
    dataset = eval_dataset,
    solver = make_replay_solver(as.character(preds$concept_id)),
    scorer = exact_scorer,
    name = paste0(name, "_concept_id")
  )

  if (is.null(chat)) {
    tsk$eval(dir = tempdir(), view = FALSE)
  } else {
    tsk$eval(solver_chat = chat, dir = tempdir(), view = FALSE)
  }

  is_match <- !is.na(preds$concept_id) &
    !is.na(preds$expected_concept_id) &
    preds$concept_id == preds$expected_concept_id

  preds <- dplyr::mutate(preds, incorrect = !is_match)

  summarise_subset <- function(df, label) {
    n_eval <- nrow(df)
    n_corr <- sum(!df$incorrect, na.rm = TRUE)
    n_unmapped <- sum(!is.na(df$concept_id) & df$concept_id == 0L)
    n_wrong <- sum(df$incorrect & !is.na(df$concept_id) & df$concept_id != 0L)

    dplyr::tibble(
      subset = label,
      n_evaluated = n_eval,
      n_correct = n_corr,
      n_unmapped = n_unmapped,
      n_wrong_mapping = n_wrong,
      accuracy = if (n_eval > 0L) round(100 * n_corr / n_eval, 1) else NA_real_
    )
  }

  summary_rows <- list(summarise_subset(preds, "overall"))

  has_mapped <- any(!is.na(preds$expected_concept_id) & preds$expected_concept_id != 0L)
  has_unmapped <- any(!is.na(preds$expected_concept_id) & preds$expected_concept_id == 0L)

  if (has_mapped && has_unmapped) {
    summary_rows <- c(
      summary_rows,
      list(
        summarise_subset(
          dplyr::filter(preds, .data$expected_concept_id != 0L),
          "expected_mapped"
        ),
        summarise_subset(
          dplyr::filter(preds, .data$expected_concept_id == 0L),
          "expected_unmapped"
        )
      )
    )
  }

  summary_tbl <- purrr::list_rbind(summary_rows)

  cli::cli_h2("Concept mapping evaluation summary ({name})")
  for (r in seq_len(nrow(summary_tbl))) {
    cli::cli_alert_info(
      paste0(
        "{.field {summary_tbl$subset[[r]]}}: ",
        "{.val {summary_tbl$n_correct[[r]]}}/{.val {summary_tbl$n_evaluated[[r]]}} ",
        "({.val {summary_tbl$accuracy[[r]]}}%) | ",
        "unmapped: {.val {summary_tbl$n_unmapped[[r]]}}, ",
        "wrong mapping: {.val {summary_tbl$n_wrong_mapping[[r]]}}"
      )
    )
  }

  invisible(list(
    summary = summary_tbl,
    task = tsk,
    results = preds
  ))
}
