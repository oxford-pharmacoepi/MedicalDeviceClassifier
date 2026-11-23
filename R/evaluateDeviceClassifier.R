#' Evaluate an LLM on the medical device classification benchmark
#'
#' @description
#' Uses the \pkg{vitals} package to evaluate a \pkg{ellmer} `chat` model's
#' accuracy with [classifyMedicalDevices()] against a labelled benchmark
#' dataset.
#'
#' Runs [classifyMedicalDevices()] once and produces a separate \pkg{vitals}
#' `Task` and accuracy score (`"C"` = Correct, `"I"` = Incorrect) for each of
#' the five classification columns:
#' * `is_medical_device`: Scored across all rows in `data`.
#' * `device_class`, `medical_speciality`, `is_implantable`, `is_active`:
#'   Scored across true medical devices (`expected_is_device == "yes"`).
#'
#' @param chat An \pkg{ellmer} `Chat` object to evaluate.
#' @param data A labelled benchmark `data.frame` or `tibble` containing
#'   `device_name`, `expected_is_device`, `expected_class`,
#'   `expected_speciality`, `expected_is_implantable`, and `expected_is_active`.
#' @param batchSize Integer passed to [classifyMedicalDevices()].
#' @param maxRetries Integer passed to [classifyMedicalDevices()].
#'
#' @return A list (invisibly) with three elements:
#'   \itemize{
#'     \item `summary`: A `tibble` with `column`, `n_evaluated`, `n_correct`,
#'       and `accuracy` (percentage) for each of the five columns.
#'     \item `tasks`: A named list of five evaluated \pkg{vitals} `Task`
#'       objects (`is_medical_device`, `device_class`, `medical_speciality`,
#'       `is_implantable`, `is_active`). Use `vitals::vitals_bind()` to combine
#'       them or `res$tasks$device_class$view()` to inspect a specific column.
#'     \item `results`: A `tibble` combining `data` with the model's predictions.
#'   }
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(vitals)
#'
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' eval_res <- evaluate_device_classifier(chat, name = "gemini-2.5-flash")
#' eval_res$summary
#' }
#'
#' @importFrom rlang .data
#' @export
evaluateDeviceClassifier <- function(chat,
                                     data,
                                     batchSize = 1L,
                                     maxRetries = 3L) {

  rlang::check_installed("vitals", reason = "to evaluate models with evaluate_device_classifier().")
  vitals::vitals_log_dir_set(tempdir())

  if (!inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.")
  }
  name <- chat$get_model()

  req_cols <- c(
    "device_name",
    "expected_is_device",
    "expected_class",
    "expected_speciality",
    "expected_is_implantable",
    "expected_is_active"
  )
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

  col_map <- c(
    is_medical_device  = "expected_is_device",
    device_class       = "expected_class",
    medical_speciality = "expected_speciality",
    is_implantable     = "expected_is_implantable",
    is_active          = "expected_is_active"
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

  tasks <- vector("list", length(col_map))
  names(tasks) <- names(col_map)
  summary_rows <- vector("list", length(col_map))

  for (i in seq_along(col_map)) {
    pred_col <- names(col_map)[[i]]
    exp_col <- col_map[[i]]
    eval_df <- if (pred_col == "is_medical_device") {
      preds
    } else {
      dplyr::filter(preds, .data$expected_is_device == "yes")
    }

    eval_dataset <- eval_df |>
      dplyr::mutate(
        input = as.character(.data$device_name),
        target = as.character(.data[[exp_col]])
      )

    cached_preds <- as.character(eval_df[[pred_col]])

    replay_solver <- function(inputs, ..., solver_chat) {
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
    tsk <- vitals::Task$new(
      dataset = eval_dataset,
      solver = replay_solver,
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
      accuracy = round(100 * n_corr / n_eval, 1)
    )
  }

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
