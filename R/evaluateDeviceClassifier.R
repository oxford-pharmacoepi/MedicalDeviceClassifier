#' Evaluate an LLM on the medical device classification benchmark
#'
#' @description
#' Uses the \pkg{vitals} package to evaluate an \pkg{ellmer} `Chat` model's
#' accuracy with [classifyMedicalDevices()] against a labelled benchmark
#' dataset.
#'
#' Runs [classifyMedicalDevices()] once and produces a separate \pkg{vitals}
#' `Task` and accuracy score (`"C"` = Correct, `"I"` = Incorrect) for each of
#' the nine classification columns:
#' * `is_medical_device`: Scored across all rows in `data`.
#' * `device_class`, `fda_medical_speciality`, `mhra_medical_speciality`,
#'   `ema_medical_speciality`, `emdn_category`, `is_implantable`, `is_active`,
#'   `is_software`: Scored across true medical devices
#'   (`expected_is_medical_device == "yes"`).
#'
#' @param chat An \pkg{ellmer} `Chat` object to evaluate.
#' @param data A labelled benchmark `data.frame` or `tibble` containing
#'   `device_name`, `expected_is_medical_device`, `expected_device_class`,
#'   `expected_fda_medical_speciality`, `expected_mhra_medical_speciality`,
#'   `expected_ema_medical_speciality`, `expected_emdn_category`,
#'   `expected_is_implantable`, `expected_is_active`, and
#'   `expected_is_software`.
#' @param batchSize Integer passed to [classifyMedicalDevices()].
#' @param maxRetries Integer passed to [classifyMedicalDevices()].
#'
#' @return A list (invisibly) with three elements:
#'   \itemize{
#'     \item `summary`: A `tibble` with `column`, `n_evaluated`, `n_correct`,
#'       and `accuracy` (percentage) for each of the nine columns.
#'     \item `tasks`: A named list of nine evaluated \pkg{vitals} `Task`
#'       objects (`is_medical_device`, `device_class`,
#'       `fda_medical_speciality`, `mhra_medical_speciality`,
#'       `ema_medical_speciality`, `emdn_category`, `is_implantable`,
#'       `is_active`, `is_software`). Use `vitals::vitals_bind()` to combine
#'       them or `res$tasks$device_class$view()` to inspect a specific column.
#'     \item `results`: A `tibble` combining `data` with the model's predictions
#'       and a logical `any_incorrect` flag (`TRUE` if any evaluated column
#'       for that row did not match the expected value).
#'   }
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(vitals)
#'
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' eval_res <- evaluateDeviceClassifier(chat, benchmark_data, batchSize = 20L)
#' eval_res$summary
#' dplyr::filter(eval_res$results, any_incorrect)
#' }
#'
#' @importFrom rlang .data
#' @export
evaluateDeviceClassifier <- function(chat,
                                     data,
                                     batchSize = 1L,
                                     maxRetries = 3L) {

  rlang::check_installed(
    "vitals",
    reason = "to evaluate models with evaluateDeviceClassifier()."
  )
  vitals::vitals_log_dir_set(tempdir())

  if (!inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.")
  }
  name <- chat$get_model()

  col_map <- c(
    is_medical_device       = "expected_is_medical_device",
    device_class            = "expected_device_class",
    fda_medical_speciality  = "expected_fda_medical_speciality",
    mhra_medical_speciality = "expected_mhra_medical_speciality",
    ema_medical_speciality  = "expected_ema_medical_speciality",
    emdn_category           = "expected_emdn_category",
    is_implantable          = "expected_is_implantable",
    is_active               = "expected_is_active",
    is_software             = "expected_is_software"
  )

  req_cols <- c("device_name", unname(col_map))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg data} is missing required benchmark column{?s}: {.col {missing_cols}}."
    )
  }

  preds <- classifyMedicalDevices(
    chat = chat,
    data = dplyr::as_tibble(data),
    maxRetries = maxRetries,
    batchSize = batchSize
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

    function(inputs, ..., solver_chat) {
      chats <- Map(function(inp, pred) {
        ch <- solver_chat$clone()
        ch$set_turns(list(
          ellmer::Turn(
            role = "user",
            contents = list(ellmer::ContentText(as.character(inp)))
          ),
          ellmer::Turn(
            role = "assistant",
            contents = list(ellmer::ContentText(ifelse(is.na(pred), "NA", as.character(pred))))
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

  tasks <- vector("list", length(col_map))
  names(tasks) <- names(col_map)
  summary_rows <- vector("list", length(col_map))

  for (i in seq_along(col_map)) {
    pred_col <- names(col_map)[[i]]
    exp_col <- col_map[[i]]

    eval_df <- if (pred_col == "is_medical_device") {
      preds
    } else {
      dplyr::filter(preds, .data$expected_is_medical_device == "yes")
    }

    eval_dataset <- eval_df |>
      dplyr::mutate(
        input = as.character(.data$device_name),
        target = as.character(.data[[exp_col]])
      )

    tsk <- vitals::Task$new(
      dataset = eval_dataset,
      solver = make_replay_solver(as.character(eval_df[[pred_col]])),
      scorer = exact_scorer,
      name = paste0(name, "_", pred_col)
    )

    tsk$eval(
      solver_chat = chat,
      dir = tempdir(),
      view = FALSE
    )

    tasks[[pred_col]] <- tsk

    scored <- tsk$get_samples()
    n_eval <- nrow(scored)
    n_corr <- sum(scored$score == "C", na.rm = TRUE)

    summary_rows[[i]] <- dplyr::tibble(
      column = pred_col,
      n_evaluated = n_eval,
      n_correct = n_corr,
      accuracy = if (n_eval > 0L) round(100 * n_corr / n_eval, 1) else NA_real_
    )
  }

  is_expected_device <- !is.na(preds$expected_is_medical_device) &
    preds$expected_is_medical_device == "yes"

  any_incorrect <- rep(FALSE, nrow(preds))
  for (i in seq_along(col_map)) {
    pred_col <- names(col_map)[[i]]
    exp_col <- col_map[[i]]

    pred_vals <- as.character(preds[[pred_col]])
    exp_vals <- as.character(preds[[exp_col]])

    col_match <- !is.na(pred_vals) & !is.na(exp_vals) & pred_vals == exp_vals
    in_scope <- if (pred_col == "is_medical_device") {
      rep(TRUE, nrow(preds))
    } else {
      is_expected_device
    }

    any_incorrect <- any_incorrect | (in_scope & !col_match)
  }

  preds <- dplyr::mutate(preds, any_incorrect = any_incorrect)
  summary_tbl <- purrr::list_rbind(summary_rows)

  cli::cli_h2("Evaluation summary ({name})")
  for (r in seq_len(nrow(summary_tbl))) {
    cli::cli_alert_info(
      "{.col {summary_tbl$column[[r]]}}: {.val {summary_tbl$n_correct[[r]]}}/{.val {summary_tbl$n_evaluated[[r]]}} ({.val {summary_tbl$accuracy[[r]]}}%)"
    )
  }

  invisible(list(
    summary = summary_tbl,
    tasks = tasks,
    results = preds
  ))
}
