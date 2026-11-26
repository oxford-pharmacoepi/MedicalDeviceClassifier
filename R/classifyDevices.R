#' Classify medical devices
#'
#' @description
#' Uses an \pkg{ellmer} `Chat` object (e.g., `ellmer::chat_google_gemini()` or
#' `ellmer::chat_ollama()`) to classify device data on the following:
#'
#' * **Is a device:** Flags whether each item is truly a medical device
#'   (`"yes"` or `"no"`), excluding non-device products such as creams,
#'   pharmaceuticals, integral drug-device medicines, and nutritional feeds.
#' * **Class:** Assigns medical devices to an EU/UK MDR risk class
#'   (`Class I`, `Class IIa`, `Class IIb`, `Class III`) or EU/UK IVDR risk class
#'   (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`, `IVD (Class D)`).
#' * **FDA medical speciality:** Assigns true medical devices to an FDA clinical
#'   speciality (18 clinical specialities plus `Other`).
#' * **MHRA medical speciality:** Assigns true medical devices to an MHRA
#'   speciality (10 clinical specialities plus `Other`).
#' * **EMA medical speciality:** Assigns true medical devices to an EMA Medical
#'   Device Expert Panel (12 thematic panels plus `Other`).
#' * **EMDN category:** Assigns true medical devices to a top-level European
#'   Medical Device Nomenclature (EMDN) category (23 lettered categories, A-Z
#'   including Annex XVI Category X, plus `Other`).
#' * **Implantable:** Flags whether medical devices are implantable (`"yes"`
#'   or `"no"`).
#' * **Active status:** Flags whether medical devices are active/powered
#'   (`"yes"` or `"no"`).
#' * **Software status:** Flags whether medical devices are standalone Software
#'   as a Medical Device / SaMD (`"yes"` or `"no"`).
#'
#' @param chat An \pkg{ellmer} `Chat` object (such as from
#'   `ellmer::chat_openai(model = "gpt-4o-mini")` or
#'   `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).
#' @param data A `data.frame` or `tibble` containing a `device_name` column
#'   which will be used to classify devices.
#' @param maxRetries Maximum number of attempts for any concept before returning
#'   `NA_character_`.
#' @param batchSize Number of unique device names to classify
#'   per LLM request. Keep this low (e.g., `1L`-`5L`) for smaller local models
#'   to maintain structured-output reliability, and higher (e.g., `20L`-`50L`)
#'   for larger frontier models to reduce request overhead and latency.
#'
#' @return A `tibble` with the original rows and columns of `data`, plus:
#'   \itemize{
#'     \item `is_medical_device`: Character (`"yes"`, `"no"`, or `NA_character_`).
#'     \item `device_class`: Character (`"Class I"`, `"Class IIa"`,
#'       `"Class IIb"`, `"Class III"`, `"IVD (Class A)"`, `"IVD (Class B)"`,
#'       `"IVD (Class C)"`, `"IVD (Class D)"`, or `NA_character_` if not a device
#'       or failed).
#'     \item `fda_medical_speciality`: Character (one of the 19 FDA medical
#'       specialities, or `NA_character_` if not a device or failed).
#'     \item `mhra_medical_speciality`: Character (one of the 11 MHRA medical
#'       specialities, or `NA_character_` if not a device or failed).
#'     \item `ema_medical_speciality`: Character (one of the 13 EMA medical
#'       specialities, or `NA_character_` if not a device or failed).
#'     \item `emdn_category`: Character (one of the 23 EMDN top-level categories
#'       or `"Other"`, or `NA_character_` if not a device or failed).
#'     \item `is_implantable`: Character (`"yes"`, `"no"`, or `NA_character_`
#'       if not a device or failed).
#'     \item `is_active`: Character (`"yes"`, `"no"`, or `NA_character_` if not
#'       a device or failed).
#'     \item `is_software`: Character (`"yes"`, `"no"`, or `NA_character_` if
#'       not a device or failed).
#'   }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ellmer)
#'
#' devices <- tibble(
#'   device_id = 1:5,
#'   device_name = c(
#'     "Medtronic Micra AV",
#'     "Stryker Triathlon CR Femoral Component",
#'     "FreeStyle Optium blood glucose test strips",
#'     "BD Alaris GP Volumetric Pump",
#'     "Diprobase cream"
#'   )
#' )
#'
#' # Using a cloud model (e.g., Google Gemini or OpenAI)
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' res <- classifyMedicalDevices(chat, devices)
#'
#' # Or using a local model via Ollama
#' local_chat <- chat_ollama(
#'   model = "qwen2.5:7b",
#'   api_args = list(temperature = 0)
#' )
#' res_local <- classifyMedicalDevices(
#'   local_chat,
#'   devices,
#'   batchSize = 1L
#' )
#' }
#' @importFrom rlang .data
#' @export
classifyMedicalDevices <- function(chat,
                                   data,
                                   maxRetries = 3L,
                                   batchSize = 1L) {

  if (!inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.",
      "i" = "Create one with e.g. {.code ellmer::chat_ollama(model = \"qwen2.5:7b\")}."
    ))
  }

  if (!is.data.frame(data) || !"device_name" %in% names(data)) {
    cli::cli_abort(
      "{.arg data} must be a data.frame or tibble containing a {.col device_name} column."
    )
  }

  output_cols <- c(
    "is_medical_device",
    "device_class",
    "fda_medical_speciality",
    "mhra_medical_speciality",
    "ema_medical_speciality",
    "emdn_category",
    "is_implantable",
    "is_active",
    "is_software"
  )
  existing_cols <- intersect(output_cols, names(data))
  if (length(existing_cols) > 0L) {
    cli::cli_abort(
      "{.arg data} already contains output column{?s}: {.col {existing_cols}}. Rename or remove {?it/them} before classifying."
    )
  }

  maxRetries <- as.integer(maxRetries)
  if (length(maxRetries) != 1L || is.na(maxRetries) || maxRetries < 1L) {
    cli::cli_abort("{.arg maxRetries} must be a single positive integer.")
  }

  batchSize <- as.integer(batchSize)
  if (length(batchSize) != 1L || is.na(batchSize) || batchSize < 1L) {
    cli::cli_abort("{.arg batchSize} must be a single positive integer.")
  }

  allowed_yes_no <- c("yes", "no")
  allowed_classes <- c(
    "Class I",
    "Class IIa",
    "Class IIb",
    "Class III",
    "IVD (Class A)",
    "IVD (Class B)",
    "IVD (Class C)",
    "IVD (Class D)"
  )
  allowed_fda_specialities <- c(
    "Anesthesiology",
    "Cardiovascular",
    "Chemistry",
    "Dental",
    "Ear, Nose, and Throat (ENT)",
    "Gastroenterology and Urology",
    "General and Plastic Surgery",
    "General Hospital",
    "Hematology",
    "Immunology",
    "Neurology",
    "Obstetrical and Gynecological",
    "Ophthalmic",
    "Orthopedic",
    "Pathology",
    "Physical Medicine",
    "Radiology",
    "Toxicology",
    "Other"
  )
  allowed_mhra_specialities <- c(
    "Oncology, Haematology & Thrombotic Disease",
    "Immunology, Biocompatibility & Non-Clinical",
    "Vaccines, Infectious Diseases & Diagnostics",
    "Senses, Movement & Pain",
    "Neuropsychiatric Disorders, Cognition & Mental Health",
    "Musculoskeletal, Trauma & Cosmetic",
    "Respiratory, Imaging & Critical Care",
    "Cardiovascular & Fluid Management",
    "Metabolic & Renal Systems",
    "Gastrointestinal, Nutrition, Endocrine & Fertility",
    "Other"
  )
  allowed_ema_specialities <- c(
    "Orthopaedics, traumatology, rehabilitation, rheumatology",
    "Circulatory system",
    "Neurology",
    "Respiratory system, anaesthesiology, intensive care",
    "Endocrinology and diabetes",
    "General and plastic surgery and dentistry",
    "Obstetrics and gynaecology, including reproductive medicine",
    "Gastroenterology and hepatology",
    "Nephrology and urology",
    "Ophthalmology",
    "In vitro diagnostic medical devices",
    "Paediatrics and rare diseases",
    "Other"
  )
  allowed_emdn_categories <- c(
    "A - Devices for Administration, Withdrawal and Collection",
    "B - Haematology and Haemotransfusion Devices",
    "C - Cardiocirculatory System Devices",
    "D - Disinfectants, Antiseptics, Sterilising Agents and Detergents",
    "F - Dialysis Devices",
    "G - Gastrointestinal Devices",
    "H - Suture Devices",
    "J - Active-Implantable Devices",
    "K - Endotherapy and Electrosurgical Devices",
    "L - Reusable Surgical Instruments",
    "M - Devices for General and Specialist Dressings",
    "N - Nervous and Medullary Systems Devices",
    "P - Implantable Prosthetic and Osteosynthesis Devices",
    "Q - Dental, Ophthalmologic and ENT Devices",
    "R - Respiratory and Anaesthesia Devices",
    "S - Sterilisation Devices",
    "T - Patient Protective Equipment and Incontinence Aids",
    "U - Devices for Urogenital System",
    "V - Various Medical Devices",
    "W - In Vitro Diagnostic Medical Devices",
    "X - Products Without an Intended Medical Purpose (Annex XVI)",
    "Y - Devices for Persons with Disabilities",
    "Z - Medical Equipment and Related Accessories, Software and Consumables",
    "Other"
  )

  batch_schema <- ellmer::type_object(
    "Batch classification results for a numbered list of candidate medical device items.",
    items = ellmer::type_array(
      description = "One classification object for each numbered item in the prompt.",
      items = ellmer::type_object(
        id = ellmer::type_integer(
          description = "The exact integer ID (1, 2, ...) matching the input list."
        ),
        is_medical_device = ellmer::type_enum(
          values = allowed_yes_no,
          description = "'yes' if the item is truly a regulated medical device (including in vitro diagnostics, MDR Annex XVI devices, and devices with an ancillary medicinal substance); 'no' if it is a pharmaceutical, integral drug-device medicinal product, topical cream/emollient, nutritional feed, cosmetic, or other non-device product."
        ),
        device_type = ellmer::type_string(
          description = "3 to 6 word summary of what the item is (e.g., 'breast reconstruction tissue expander', 'blood glucose test strips', 'topical emollient cream (non-device)')."
        ),
        fda_medical_speciality = ellmer::type_enum(
          values = c(allowed_fda_specialities, "Not Applicable"),
          description = "Primary FDA medical speciality category for the device, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        mhra_medical_speciality = ellmer::type_enum(
          values = c(allowed_mhra_specialities, "Not Applicable"),
          description = "Primary MHRA medical speciality category for the device, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        ema_medical_speciality = ellmer::type_enum(
          values = c(allowed_ema_specialities, "Not Applicable"),
          description = "Primary EMA Medical Device Expert Panel speciality for the device, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        emdn_category = ellmer::type_enum(
          values = c(allowed_emdn_categories, "Not Applicable"),
          description = "Top-level EMDN category for the device, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        device_class = ellmer::type_enum(
          values = c(allowed_classes, "Not Applicable"),
          description = "Regulatory risk class (Class I, Class IIa, Class IIb, Class III for general medical devices; IVD (Class A), IVD (Class B), IVD (Class C), IVD (Class D) for in vitro diagnostics), or 'Not Applicable' if is_medical_device is 'no'."
        ),
        is_implantable = ellmer::type_enum(
          values = c(allowed_yes_no, "Not Applicable"),
          description = "'yes' if the device is intended to be totally or partially introduced into the human body via clinical intervention and remain in place, 'no' otherwise, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        is_active = ellmer::type_enum(
          values = c(allowed_yes_no, "Not Applicable"),
          description = "'yes' if the device relies on electrical energy, a power source, or standalone software to function, 'no' if passive/mechanical, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        is_software = ellmer::type_enum(
          values = c(allowed_yes_no, "Not Applicable"),
          description = "'yes' if the device is standalone Software as a Medical Device (SaMD), a medical app, or a standalone AI/clinical software tool; 'no' for physical hardware (even with embedded firmware) or passive devices; 'Not Applicable' if is_medical_device is 'no'."
        )
      )
    )
  )

  base_chat <- chat$clone()
  base_chat$set_turns(list())
  base_chat$set_system_prompt(.device_classifier_system_prompt())

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
      "No non-empty {.col device_name} values found in {.arg data}. Returning {.val NA} columns."
    )
    return(
      work_df |>
        dplyr::select(-"..clean_device_name..") |>
        dplyr::mutate(
          is_medical_device = NA_character_,
          device_class = NA_character_,
          fda_medical_speciality = NA_character_,
          mhra_medical_speciality = NA_character_,
          ema_medical_speciality = NA_character_,
          emdn_category = NA_character_,
          is_implantable = NA_character_,
          is_active = NA_character_,
          is_software = NA_character_
        )
    )
  }

  batches <- split(
    unique_concepts,
    ceiling(seq_along(unique_concepts) / batchSize)
  )

  cli::cli_alert_info(
    "Found {.val {n_unique}} unique concept{?s} across {.val {n_total}} row{?s} ({.val {length(batches)}} batch{?es} of up to {.val {batchSize}}; max retries: {.val {maxRetries}})."
  )

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

  classify_batch <- function(concepts_vec) {
    n_b <- length(concepts_vec)
    out_is_device <- rep(NA_character_, n_b)
    out_class <- rep(NA_character_, n_b)
    out_fda_spec <- rep(NA_character_, n_b)
    out_mhra_spec <- rep(NA_character_, n_b)
    out_ema_spec <- rep(NA_character_, n_b)
    out_emdn_cat <- rep(NA_character_, n_b)
    out_implantable <- rep(NA_character_, n_b)
    out_active <- rep(NA_character_, n_b)
    out_software <- rep(NA_character_, n_b)
    pending_idx <- seq_len(n_b)
    batch_cost <- 0
    has_cost_data <- FALSE

    required_fields <- c(
      "id",
      "is_medical_device",
      "device_class",
      "fda_medical_speciality",
      "mhra_medical_speciality",
      "ema_medical_speciality",
      "emdn_category",
      "is_implantable",
      "is_active",
      "is_software"
    )

    for (attempt in seq_len(maxRetries)) {
      if (length(pending_idx) == 0L) break

      curr_concepts <- concepts_vec[pending_idx]
      prompt_lines <- paste0(seq_along(curr_concepts), ": ", curr_concepts)
      prompt_text <- paste(
        "Evaluate each of the following numbered items, determine whether it is truly a medical device ('yes' or 'no'), and return the matching id for every item:",
        paste(prompt_lines, collapse = "\n"),
        sep = "\n"
      )

      ch <- base_chat$clone()
      res <- tryCatch(
        {
          ch$chat_structured(
            prompt_text,
            type = batch_schema,
            echo = "none"
          )
        },
        error = function(e) NULL
      )

      attempt_cost <- extract_call_cost(ch)
      if (!is.na(attempt_cost)) {
        batch_cost <- batch_cost + attempt_cost
        has_cost_data <- TRUE
      }

      items_df <- if (is.list(res) && !is.null(res$items)) res$items else NULL

      if (is.data.frame(items_df) && all(required_fields %in% names(items_df))) {
        for (r in seq_len(nrow(items_df))) {
          item_id <- suppressWarnings(as.integer(items_df$id[[r]]))
          is_dev <- tolower(as.character(items_df$is_medical_device[[r]]))
          d_cls <- as.character(items_df$device_class[[r]])
          fda_spc <- as.character(items_df$fda_medical_speciality[[r]])
          mhra_spc <- as.character(items_df$mhra_medical_speciality[[r]])
          ema_spc <- as.character(items_df$ema_medical_speciality[[r]])
          emdn_cat <- as.character(items_df$emdn_category[[r]])
          is_imp <- tolower(as.character(items_df$is_implantable[[r]]))
          is_act <- tolower(as.character(items_df$is_active[[r]]))
          is_sw <- tolower(as.character(items_df$is_software[[r]]))

          if (!is.na(item_id) &&
              item_id >= 1L &&
              item_id <= length(curr_concepts) &&
              !is.na(is_dev) && is_dev %in% allowed_yes_no) {
            orig_pos <- pending_idx[[item_id]]

            if (is_dev == "no") {
              out_is_device[[orig_pos]] <- "no"
              out_class[[orig_pos]] <- NA_character_
              out_fda_spec[[orig_pos]] <- NA_character_
              out_mhra_spec[[orig_pos]] <- NA_character_
              out_ema_spec[[orig_pos]] <- NA_character_
              out_emdn_cat[[orig_pos]] <- NA_character_
              out_implantable[[orig_pos]] <- NA_character_
              out_active[[orig_pos]] <- NA_character_
              out_software[[orig_pos]] <- NA_character_
            } else if (!is.na(d_cls) && d_cls %in% allowed_classes &&
                       !is.na(fda_spc) && fda_spc %in% allowed_fda_specialities &&
                       !is.na(mhra_spc) && mhra_spc %in% allowed_mhra_specialities &&
                       !is.na(ema_spc) && ema_spc %in% allowed_ema_specialities &&
                       !is.na(emdn_cat) && emdn_cat %in% allowed_emdn_categories &&
                       !is.na(is_imp) && is_imp %in% allowed_yes_no &&
                       !is.na(is_act) && is_act %in% allowed_yes_no &&
                       !is.na(is_sw) && is_sw %in% allowed_yes_no) {
              out_is_device[[orig_pos]] <- "yes"
              out_class[[orig_pos]] <- d_cls
              out_fda_spec[[orig_pos]] <- fda_spc
              out_mhra_spec[[orig_pos]] <- mhra_spc
              out_ema_spec[[orig_pos]] <- ema_spc
              out_emdn_cat[[orig_pos]] <- emdn_cat
              out_implantable[[orig_pos]] <- is_imp
              out_active[[orig_pos]] <- is_act
              out_software[[orig_pos]] <- is_sw
            }
          }
        }
      }

      pending_idx <- which(
        is.na(out_is_device) |
          (out_is_device == "yes" &
             (is.na(out_class) |
                is.na(out_fda_spec) |
                is.na(out_mhra_spec) |
                is.na(out_ema_spec) |
                is.na(out_emdn_cat) |
                is.na(out_implantable) |
                is.na(out_active) |
                is.na(out_software)))
      )

      if (length(pending_idx) > 0L && attempt < maxRetries) {
        Sys.sleep(0.5 * attempt)
      }
    }

    list(
      df = dplyr::tibble(
        ..clean_device_name.. = concepts_vec,
        is_medical_device = out_is_device,
        device_class = out_class,
        fda_medical_speciality = out_fda_spec,
        mhra_medical_speciality = out_mhra_spec,
        ema_medical_speciality = out_ema_spec,
        emdn_category = out_emdn_cat,
        is_implantable = out_implantable,
        is_active = out_active,
        is_software = out_software
      ),
      cost = if (has_cost_data) batch_cost else NA_real_
    )
  }

  n_processed <- 0L
  n_errors <- 0L
  total_cost <- 0
  any_cost_available <- FALSE
  cost_str <- "$0.00"
  results_list <- vector("list", length(batches))

  # Compact format so cost stays visible even in narrow RStudio/Positron panes
  pb_format <- paste0(
    "{cli::pb_bar} {cli::pb_percent} | ",
    "{cli::pb_current}/{cli::pb_total} | ",
    "{cli::pb_extra$cost} | ",
    "Err: {cli::pb_extra$errors} | ",
    "ETA: {cli::pb_eta}"
  )

  pb_id <- cli::cli_progress_bar(
    total = n_unique,
    format = pb_format,
    extra = list(cost = cost_str, errors = n_errors),
    clear = TRUE,
    auto_terminate = TRUE
  )

  on.exit(
    cli::cli_progress_done(id = pb_id, result = "clear"),
    add = TRUE
  )

  for (b_idx in seq_along(batches)) {
    batch_out <- classify_batch(batches[[b_idx]])
    batch_res <- batch_out$df
    results_list[[b_idx]] <- batch_res

    if (!is.na(batch_out$cost)) {
      total_cost <- total_cost + batch_out$cost
      any_cost_available <- TRUE
      cost_str <- format_cost(total_cost)
    } else if (!any_cost_available) {
      cost_str <- "N/A"
    }

    n_processed <- n_processed + nrow(batch_res)
    batch_errors <- sum(
      is.na(batch_res$is_medical_device) |
        (batch_res$is_medical_device == "yes" &
           (is.na(batch_res$device_class) |
              is.na(batch_res$fda_medical_speciality) |
              is.na(batch_res$mhra_medical_speciality) |
              is.na(batch_res$ema_medical_speciality) |
              is.na(batch_res$emdn_category) |
              is.na(batch_res$is_implantable) |
              is.na(batch_res$is_active) |
              is.na(batch_res$is_software)))
    )
    n_errors <- n_errors + batch_errors

    cli::cli_progress_update(
      set = n_processed,
      extra = list(cost = cost_str, errors = n_errors),
      id = pb_id,
      force = (n_processed == n_unique)
    )
  }

  cli::cli_progress_done(id = pb_id)
  on.exit(NULL)

  lookup <- purrr::list_rbind(results_list)
  n_succeeded <- n_unique - n_errors

  final_cost_msg <- if (any_cost_available) {
    paste0(" | Total cost: ", format_cost(total_cost))
  } else {
    ""
  }

  if (n_errors == 0L) {
    cli::cli_alert_success(
      "Processed {.val {n_unique}}/{.val {n_unique}} unique concept{?s} ({.val {n_succeeded}} succeeded, {.val 0} errors{final_cost_msg})."
    )
  } else {
    cli::cli_alert_warning(
      "Processed {.val {n_unique}}/{.val {n_unique}} unique concept{?s} ({.val {n_succeeded}} succeeded, {.val {n_errors}} error{?s} set to {.val NA}{final_cost_msg})."
    )
  }

  work_df |>
    dplyr::left_join(lookup, by = "..clean_device_name..") |>
    dplyr::select(-"..clean_device_name..")
}


.device_classifier_system_prompt <- function() {
  paste(
    "You are an expert EU/UK MDR, IVDR, EMA, EMDN, MHRA, and FDA medical device regulatory specialist.",
    "You will receive a numbered list of product names from routinely collected clinical data (e.g., NHS dm+d, hospital procurement, or registry records, which may include brand names, manufacturer names, catalogue abbreviations, or non-device products).",
    "For EVERY numbered item in the list, return an object with:",
    "  1. `id`: The exact integer ID matching the input line.",
    "  2. `is_medical_device`: 'yes' if the item is truly a regulated medical device (including in vitro diagnostic devices, EU MDR Annex XVI products, and devices incorporating an ancillary medicinal substance), or 'no' if it is NOT a medical device.",
    "  3. `device_type`: A concise 3-6 word description identifying what the item is.",
    "  4. `fda_medical_speciality`: The FDA regulatory speciality category (choose strictly from the 19 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  5. `mhra_medical_speciality`: The MHRA regulatory speciality category (choose strictly from the 11 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  6. `ema_medical_speciality`: The EMA Medical Device Expert Panel speciality (choose strictly from the 13 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  7. `emdn_category`: The top-level EMDN category (choose strictly from the 24 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  8. `device_class`: The EU/UK MDR risk class (`Class I`, `Class IIa`, `Class IIb`, or `Class III`) for general medical devices, OR the EU/UK IVDR risk class (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`, or `IVD (Class D)`) for in vitro diagnostic devices, or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  9. `is_implantable`: 'yes' or 'no' (or 'Not Applicable' if `is_medical_device` is 'no').",
    " 10. `is_active`: 'yes' or 'no' (or 'Not Applicable' if `is_medical_device` is 'no').",
    " 11. `is_software`: 'yes' or 'no' (or 'Not Applicable' if `is_medical_device` is 'no').",
    "Evaluate each item independently -- never let adjacent items in a batch bias the current item.",
    "",
    "### PART 0: MEDICAL DEVICE ELIGIBILITY (`is_medical_device`)",
    "- Return `is_medical_device: 'no'` (with `fda_medical_speciality`, `mhra_medical_speciality`, `ema_medical_speciality`, `emdn_category`, `device_class`, `is_implantable`, `is_active`, and `is_software` all set to `'Not Applicable'`) for:",
    "  * Topical creams, ointments, emollients, lotions, gels, bath additives, shampoos, barrier creams, and sunscreens (e.g., Diprobase, Epaderm, Aveeno, Aqueous cream, Hydrocortisone cream).",
    "  * Pharmaceuticals and medicinal products: tablets, capsules, oral solutions, injections, IV drug infusions, vaccines, lozenges, suppositories, and medicated eye/ear/nasal drops containing an active pharmaceutical ingredient (e.g., antibiotic, steroid, antihistamine, or prostaglandin drops).",
    "  * Integral drug-device combinations regulated as medicines (where the device forms a single non-reusable product solely to administer a medicinal product): pre-filled drug syringes, adrenaline/epinephrine auto-injectors (e.g., EpiPen, Jext), pre-filled insulin/GLP-1 pens, and drug-containing inhalers/nebuliser ampoules.",
    "  * Nutritional and dietary products: oral nutritional supplements, enteral feeds/formulas (e.g., Ensure, Fortisip), infant formulas, thickeners, gluten-free foods, vitamins/minerals.",
    "  * Cosmetics, toiletries, soaps, toothpastes, mouthwashes, staff apparel/uniforms (including disposable or reusable surgical scrub suits), paper hand towels, consumer fitness watches/trackers without a regulated medical purpose, and general non-medical consumables.",
    "- Return `is_medical_device: 'yes'` for true regulated medical devices whose primary mode of action is physical, mechanical, or diagnostic rather than pharmacological/metabolic, including:",
    "  * Implants, surgical instruments, catheters, empty syringes/needles, wound dressings/bandages, ostomy/incontinence appliances, compression hosiery, diagnostic test strips/meters, laboratory/point-of-care IVD assays and analysers, ventilators, imaging systems, standalone medical software (SaMD), and internal surgical haemostats/sealants (e.g., Floseal, Surgicel, Tisseel).",
    "  * Physical-action lubricating/irrigating eye, ear, and nasal drops/sprays regulated as medical devices (e.g., sodium hyaluronate/carbomer lubricating eye drops such as Hylo-Forte, Thealoz Duo, Clinitas; isotonic/hypertonic saline nasal sprays; olive-oil or sodium-bicarbonate earwax drops).",
    "  * Medical devices incorporating an ancillary medicinal substance (EU MDR Rule 14, typically Class III): drug-eluting coronary/vascular stents, antibiotic-loaded bone cement, heparin-coated catheters, and silver/iodine/honey antimicrobial wound dressings.",
    "  * EU MDR Annex XVI devices (e.g., dermal fillers, non-corrective cosmetic contact lenses).",
    "",
    "### PART 1A: FDA MEDICAL SPECIALITY RULES (`fda_medical_speciality`; Classify by Regulatory Anatomy/Function, Not Surgeon Type)",
    "- Anesthesiology: ICU/transport ventilators, anaesthesia workstations, vaporisers, endotracheal tubes, epidural needles, pulse oximeters/capnographs (NOT tracheostomy tubes or LMAs, which belong in ENT).",
    "- Cardiovascular: Pacemakers, ICDs, cardiac resynchronisation devices, heart valves, vascular/coronary stents, angioplasty balloons, cardiac guidewires, ablation catheters, dedicated haemodynamic/ECG monitors, vascular grafts (e.g., Medtronic Cardiac, Edwards, Boston Scientific, Abbott Vascular -- NOT multi-parameter bedside/ICU patient monitors, which belong in General Hospital).",
    "- Chemistry: Clinical chemistry analysers, blood gas analysers, blood glucose meters/strips, blood ketone test strips, HbA1c/lipid/electrolyte analysers, urine chemistry dipsticks (NOT empty urine specimen pots/containers, which belong in Pathology).",
    "- Dental: Dental implants, dental handpieces, orthodontic brackets/wires, dental amalgam/composites, synthetic dental bone grafts, endodontic files.",
    "- Ear, Nose, and Throat (ENT): Hearing aids, cochlear implants, tympanostomy tubes, nasal dilators/packing, saline nasal sprays, earwax drops, laryngeal mask airways (LMAs), tracheostomy tubes, ENT endoscopes, otoscopes.",
    "- Gastroenterology and Urology: GI endoscopes, capsule endoscopy systems, colonoscopes, ERCP catheters, lithotripters, haemodialysis/CRRT machines and dialysers, Foley/urinary catheters, ureteral stents, nephrostomy tubes, enteral feeding tubes (PEG/NG), ostomy bags/appliances.",
    "- General and Plastic Surgery: Breast implants, breast tissue expanders, dermal fillers, surgical mesh (hernia/soft-tissue), sutures, skin/internal staplers, wound dressings/NPWT, electrosurgical generators/pencils, laparoscopic trocars, scalpels, sterile surgical gloves, internal absorbable haemostatic agents and surgical sealants (e.g., Mentor, Allergan/Natrelle, Ethicon, Floseal, Surgicel, Tisseel -- NOT dural grafts/matrices, which belong in Neurology).",
    "- General Hospital: Hospital beds, multi-parameter bedside/ICU patient monitors (e.g., Philips IntelliVue), infusion pumps, syringe pumps, IV administration sets/cannulas, clinical thermometers, hypodermic syringes/needles, non-sterile examination gloves, patient lifts, sharps containers, neonatal incubators/phototherapy units.",
    "- Hematology: Coagulation/INR analysers and test strips, automated blood cell counters (CBC), haemoglobinometers, blood grouping/cross-matching reagents, blood collection/transfusion bags, dedicated blood bank/immunohematology centrifuges (NOT general laboratory centrifuges, which belong in Pathology).",
    "- Immunology: Viral/bacterial serological tests, PCR/NAT infectious disease assays (e.g., HIV, Hepatitis, SARS-CoV-2, influenza), blood culture media, microbiology susceptibility plates, autoimmune/allergy antibody detection kits.",
    "- Neurology: Devices interacting directly with brain, spinal cord tissue, dura mater, nerves, or CSF, plus psychiatric/mental-health digital therapeutics (FDA regulates psychiatric SaMD under Neurology): deep brain stimulators (DBS), spinal cord/vagus nerve stimulators, CSF shunts, dural grafts/regeneration matrices (e.g., DuraGen), intracranial pressure monitors, aneurysm coils/flow diverters, EEGs, intraoperative neuromonitoring (e.g., NIM), CBT/mental-health SaMD -- NEVER structural spinal bone screws, rods, or fusion cages.",
    "- Obstetrical and Gynecological: Foetal monitors (CTG), contraceptive IUDs, vaginal pessaries, colposcopes, vaginal speculums, obstetrical forceps/vacuum extractors, hysteroscopes, endometrial ablation devices, home pregnancy/ovulation test kits.",
    "- Ophthalmic: Contact lenses, contact lens care/disinfecting solutions, lubricating/ocular-surface medical device eye drops (e.g., sodium hyaluronate), intraocular lenses (IOLs), phacoemulsification systems, ophthalmic surgical lasers, tonometers, viscoelastics, ophthalmic surgical knives.",
    "- Orthopedic: STRICTLY internal/surgical musculoskeletal, bone, joint, and structural spine hardware (including spine implants placed by neurosurgeons): total/partial hip, knee, shoulder, or ankle joint replacements; bone fixation plates, screws, and intramedullary nails; pedicle screw/rod systems; spinal interbody fusion cages and disc prostheses; bone cement; arthroscopy instruments (e.g., DePuy Synthes, Stryker, Zimmer Biomet, NuVasive, Globus Medical, Smith+Nephew -- NEVER external braces/orthoses, which belong in Physical Medicine).",
    "- Pathology: Histopathology tissue processors, microtomes, cryostats, histology slide stainers, immunohistochemistry (IHC) reagents/stainers, cytology processors, general clinical laboratory centrifuges, blood collection tubes (e.g., Vacutainer), and specimen collection containers/pots.",
    "- Physical Medicine: Powered/manual wheelchairs, TENS units, neuromuscular electrical stimulators, therapeutic ultrasound/diathermy units, traction equipment, external limb prostheses/orthoses/braces (e.g., knee/ankle/wrist braces), crutches/walkers.",
    "- Radiology: CT scanners, MRI systems, X-ray machines, fluoroscopy C-arms, diagnostic ultrasound systems, mammography units, nuclear medicine/PET scanners, radiation therapy linear accelerators, radiology AI/image-analysis software -- NOT catheters or stents placed under imaging guidance, and NOT physical therapy ultrasound units.",
    "- Toxicology: Drug-of-abuse screening cups/strips, therapeutic drug monitoring (TDM) assays, blood alcohol/ethanol test kits, heavy metal/poisoning diagnostic assays.",
    "- Other: Sterilization/CSSD autoclaves, instrument disinfectants, general facility equipment, cross-speciality software, or non-specific consumables that cannot be mapped to any of the 18 clinical specialities above.",
    "",
    "### PART 1B: MHRA MEDICAL SPECIALITY RULES (`mhra_medical_speciality`)",
    "- Oncology, Haematology & Thrombotic Disease: Tumour ablation systems, radiotherapy/oncology devices, apheresis/adsorption systems, organ preservation/perfusion systems, coagulation/INR testing and thrombosis-management devices, blood bank/transfusion bags, and automated haematology analysers/systems.",
    "- Immunology, Biocompatibility & Non-Clinical: Biocompatibility-related implant/material devices, wound-contact materials and dressings, absorbable haemostatic matrices/sealants, combination-device material systems, and general surgical sutures/biomaterials.",
    "- Vaccines, Infectious Diseases & Diagnostics: IVD diagnostic analysers (NOT haematology/coagulation analysers or glucose/ketone IVDs), lateral-flow/rapid diagnostic tests (including pregnancy/infectious disease kits), PCR/microbiology assays, specimen collection devices (e.g., Vacutainers, swabs, sample pots), and pathology/histology diagnostics.",
    "- Senses, Movement & Pain: Spinal cord/peripheral neurostimulators (NOT DBS or VNS systems, which belong in Neuropsychiatric Disorders, Cognition & Mental Health), TENS units, cochlear implants, hearing aids, ENT airways (tracheostomy tubes, LMAs), ENT instruments/sprays, ophthalmic/contact-lens devices and solutions, lubricating medical device eye drops, intraocular lenses (IOLs), and ophthalmic surgical systems.",
    "- Neuropsychiatric Disorders, Cognition & Mental Health: Deep-brain stimulation (DBS) systems, vagus nerve stimulation (VNS) systems, cognitive/neurological monitoring devices (EEG, ICP monitors, intraoperative neuromonitoring), CSF shunts, dural grafts, neurovascular aneurysm coils, and digital mental-health/neurological SaMD.",
    "- Musculoskeletal, Trauma & Cosmetic: Total/partial joint replacements, intramedullary nails, bone plates/screws, spinal pedicle screws/fusion cages, maxillofacial/dental implants and instruments, external orthoses/braces, breast implants/tissue expanders, and dermal fillers/cosmetic implants.",
    "- Respiratory, Imaging & Critical Care: ICU/transport ventilators, CPAP/BiPAP systems, anaesthesia machines/workstations, endotracheal tubes, pulse oximeters/capnographs, multi-parameter ICU/bedside patient monitors (NOT foetal/maternal CTG monitors, which belong in Gastrointestinal, Nutrition, Endocrine & Fertility), neonatal incubators and neonatal phototherapy systems, and diagnostic imaging systems/AI imaging software (CT, MRI, X-ray, ultrasound).",
    "- Cardiovascular & Fluid Management: Pacemakers, ICDs, cardiovascular/vascular stents, prosthetic heart valves/TAVI, angioplasty balloons, vascular grafts, IV infusion/syringe pumps, IV administration sets/cannulas, and hypodermic syringes.",
    "- Metabolic & Renal Systems: Continuous glucose monitors (CGMs), blood glucose/ketone meters and test strips, insulin pumps, haemodialysis/CRRT systems, urinary/Foley/renal catheters, ureteral stents, and nephrostomy tubes.",
    "- Gastrointestinal, Nutrition, Endocrine & Fertility: Hernia/pelvic surgical mesh, non-hormonal intrauterine devices (coils/IUDs), obstetric/gynaecological devices (including foetal/maternal CTG monitors, pessaries, speculums, and contraception apps), GI endoscopes, enteral feeding tubes (PEG/NG), and ostomy appliances.",
    "- Other: Sterilisation/CSSD autoclaves, instrument disinfectants, electrosurgical generators, general laparoscopic trocars, reusable surgical hand instruments (e.g., scalpels), gloves, hospital beds, wheelchairs, general facility equipment, or cross-speciality consumables that do not map cleanly to the 10 MHRA specialities above.",
    "",
    "### PART 1C: EMA MEDICAL SPECIALITY RULES (`ema_medical_speciality`)",
    "- Orthopaedics, traumatology, rehabilitation, rheumatology: Joint replacements, bone plates/screws, intramedullary nails, spinal pedicle screws/fusion cages, wheelchairs, external orthoses/braces, TENS/rehabilitation devices.",
    "- Circulatory system: Pacemakers, ICDs, heart valves/TAVI, coronary/vascular stents, angioplasty balloons, vascular grafts, cardiac ablation catheters, ECG/haemodynamic monitors, cardiac/arrhythmia SaMD.",
    "- Neurology: Deep brain stimulators (DBS), spinal cord/vagus nerve stimulators, CSF shunts, dural grafts, aneurysm coils, EEGs, ICP monitors, intraoperative neuromonitoring, stroke/neurological AI software, and digital mental-health/psychiatric CBT SaMD.",
    "- Respiratory system, anaesthesiology, intensive care: ICU/transport ventilators, CPAP/BiPAP, anaesthesia workstations, endotracheal/tracheostomy tubes, LMAs, pulse oximeters/capnographs, ICU multi-parameter patient monitors, ICU infusion pumps (NOT general-purpose hypodermic syringes, needles, or peripheral IV cannulas, which belong in Other).",
    "- Endocrinology and diabetes: Continuous glucose monitors (CGMs), insulin pumps, empty insulin pens/needles, and non-IVD endocrine delivery/monitoring devices (NOTE: blood glucose/ketone test strips and meters are IVDs and belong in `In vitro diagnostic medical devices`).",
    "- General and plastic surgery and dentistry: Breast implants/tissue expanders, dermal fillers, hernia/soft-tissue surgical mesh, sutures, staplers, wound dressings/NPWT, internal surgical haemostats/sealants, electrosurgical units, laparoscopic trocars, reusable surgical scalpels, sterile surgical gloves, and all dental implants/instruments/materials.",
    "- Obstetrics and gynaecology, including reproductive medicine: Contraceptive IUDs, foetal monitors (CTG), vaginal pessaries/speculums, hysteroscopes, endometrial ablation devices, and assisted-reproduction/fertility devices.",
    "- Gastroenterology and hepatology: GI endoscopes, colonoscopes, ERCP catheters, enteral feeding tubes (PEG/NG), ostomy appliances, and liver/biliary stents.",
    "- Nephrology and urology: Haemodialysis/CRRT machines and dialysers, Foley/urinary catheters, ureteral stents, nephrostomy tubes, and lithotripters.",
    "- Ophthalmology: Contact lenses, contact lens care/disinfecting solutions, lubricating medical device eye drops, intraocular lenses (IOLs), phacoemulsification systems, ophthalmic lasers, tonometers, and ophthalmic surgical instruments.",
    "- In vitro diagnostic medical devices: STRICTLY devices classified as `IVD (Class A)` through `IVD (Class D)` examining human specimens outside the body, including blood glucose/ketone meters and test strips, coagulation/INR strips, home pregnancy tests, clinical chemistry/haematology analysers, infectious disease/PCR assays, and IVD blood/urine specimen collection tubes and pots (NEVER MDR blood transfusion/collection bags, which belong in Other).",
    "- Paediatrics and rare diseases: Devices specifically designed or indicated for neonatal/paediatric use (e.g., neonatal incubators, neonatal phototherapy systems, paediatric-specific implants/catheters) or orphan/rare-disease indications.",
    "- Other: General hospital/facility equipment, general-purpose hypodermic syringes/needles and peripheral IV cannulas, non-sterile examination gloves, blood transfusion/collection bags, imaging/radiology systems (CT, MRI, X-ray, ultrasound), ENT/hearing devices (hearing aids, cochlear implants, otoscopes), sterilisation autoclaves/disinfectants, or cross-speciality consumables that do not map cleanly to the 12 EMA panels above.",
    "",
    "### PART 1D: EMDN CATEGORY RULES (`emdn_category`; Classify by Device Type/Technology)",
    "- A - Devices for Administration, Withdrawal and Collection: Needles, syringes, IV cannulas, infusion/administration sets, ostomy bags, surgical drainage sets, enteral feeding sets.",
    "- B - Haematology and Haemotransfusion Devices: Blood collection/transfusion bags, apheresis sets, autotransfusion consumables, blood warmers.",
    "- C - Cardiocirculatory System Devices: Non-active vascular/coronary catheters, angioplasty balloons, cardiac guidewires, ablation catheters, extracorporeal circulation consumables (NOT active implantable pacemakers/ICDs [J] or passive heart valves/stents/grafts [P]).",
    "- D - Disinfectants, Antiseptics, Sterilising Agents and Detergents: Chemical disinfectants, instrument detergents, cold-sterilising solutions for medical devices (NOT contact lens care/disinfecting solutions, which belong in Q).",
    "- F - Dialysis Devices: Haemodialysis/peritoneal dialysis filters, dialysers, dialysis tubing/catheters, CRRT consumables (NOT powered haemodialysis/CRRT machines [Z]).",
    "- G - Gastrointestinal Devices: Enteral feeding tubes (PEG/NG), GI catheters/probes, endoscopic GI consumables, biliary/oesophageal stents.",
    "- H - Suture Devices: Absorbable/non-absorbable surgical sutures, surgical staplers, skin clips, ligating clips.",
    "- J - Active-Implantable Devices: Pacemakers, ICDs, CRT devices, deep brain stimulators (DBS), spinal cord/vagus nerve stimulators, cochlear implants, implantable drug pumps.",
    "- K - Endotherapy and Electrosurgical Devices: Laparoscopic trocars, electrosurgical pencils/electrodes, harmonic/ultrasonic scalpels, endoscopic biopsy forceps/snares.",
    "- L - Reusable Surgical Instruments: Reusable scalpels, forceps, clamps, retractors, surgical scissors, reusable orthopaedic/dental hand instruments.",
    "- M - Devices for General and Specialist Dressings: Bandages, gauzes, transparent film dressings, NPWT dressings, compression hosiery, internal absorbable haemostats and surgical sealants (e.g., Floseal, Surgicel, Tisseel).",
    "- N - Nervous and Medullary Systems Devices: Non-active neurosurgical consumables, CSF shunts, dural grafts/matrices, intracranial pressure probes, neurovascular aneurysm coils.",
    "- P - Implantable Prosthetic and Osteosynthesis Devices: Non-active implants with a medical purpose -- joint replacements, bone plates/screws, intramedullary nails, spinal cages/pedicle screws, heart valves, vascular stents/grafts, reconstructive breast implants/expanders, surgical mesh, intraocular lenses (IOLs), dental implants.",
    "- Q - Dental, Ophthalmologic and ENT Devices: Corrective contact lenses, contact lens care/disinfecting solutions, lubricating medical device eye drops, dental handpieces (electric/air-driven), dental fillings/composites/orthodontics, ophthalmic viscoelastics/knives, tympanostomy tubes, nasal packing, saline nasal sprays, ENT consumables (NOT IOLs [P], cochlear implants [J], purely cosmetic non-corrective contact lenses [X], or powered/battery diagnostic instruments such as otoscopes or intraoral scanners [Z]).",
    "- R - Respiratory and Anaesthesia Devices: Endotracheal/tracheostomy tubes, LMAs, breathing circuits, oxygen masks, humidifiers, epidural/spinal anaesthesia needles/kits (NOT powered ICU ventilators/anaesthesia workstations [Z]).",
    "- S - Sterilisation Devices: Sterilisation wraps, pouches, biological/chemical indicators, sterilisation containers (NOT powered autoclaves [Z]).",
    "- T - Patient Protective Equipment and Incontinence Aids: Surgical/examination gloves, surgical drapes/gowns, masks, absorbent incontinence pads, protective barriers.",
    "- U - Devices for Urogenital System: Foley/urinary catheters, ureteral stents, nephrostomy tubes, contraceptive IUDs, vaginal pessaries/speculums, obstetric delivery consumables.",
    "- V - Various Medical Devices: General-purpose medical consumables not fitting A-U (e.g., clinical thermometers, stethoscopes, sharps containers, ultrasound gel, lubricating gels).",
    "- W - In Vitro Diagnostic Medical Devices: ALL IVD assays, test strips/meters (glucose, INR, pregnancy), reagents, calibrators, IVD analysers, and IVD specimen collection tubes/pots.",
    "- X - Products Without an Intended Medical Purpose (Annex XVI): Regulated EU MDR Annex XVI devices with purely aesthetic or non-medical intent -- non-corrective cosmetic contact lenses, aesthetic-only facial/dermal fillers, purely aesthetic body-modification implants, aesthetic liposuction/lipolysis/lipoplasty equipment, aesthetic high-intensity electromagnetic/IPL/laser hair-removal or tattoo-removal devices, and non-medical transcranial brain stimulation equipment.",
    "- Y - Devices for Persons with Disabilities: Wheelchairs, walkers, crutches, external limb prostheses, external orthoses/braces, hearing aids.",
    "- Z - Medical Equipment and Related Accessories, Software and Consumables: Powered capital equipment, powered/battery handheld diagnostic instruments (e.g., otoscopes, intraoral scanners), and standalone medical software (SaMD) -- CT/MRI/X-ray/ultrasound scanners, ICU ventilators, anaesthesia machines, haemodialysis/CRRT machines, infusion/syringe pumps, bedside patient monitors, defibrillators, electrosurgical generators, hospital beds, autoclaves, TENS units, clinical/AI SaMD.",
    "- Other: Fallback only if a valid medical device cannot be placed in any of the 23 categories above.",
    "",
    "### PART 2A: EU/UK MDR RISK CLASS RULES (General Medical Devices)",
    "- Class I: ONLY strictly non-invasive devices touching intact skin or simple reusable surgical/dental hand instruments (EU MDR Rule 1 & Rule 6: e.g., hospital beds, wheelchairs, walking frames, manual scalpels, forceps, reusable stainless steel dental mouth mirrors, stethoscopes, non-sterile external intact-skin bandages, vaginal speculums). NEVER assign Class I to any implantable, absorbable, sterile invasive, wound micro-environment film dressing, biological/animal-derived agent, or active body-orifice diagnostic device (such as intraoral scanners or otoscopes).",
    "- Class IIa: Transient/short-term invasive devices (<30 days) including active or disposable devices inserted into body orifices (EU MDR Rule 5: e.g., intraoral scanners, otoscopes, laryngeal masks, tracheostomy tubes), sterile transparent film dressings managing wound/catheter-site micro-environments (EU MDR Rule 4: e.g., 3M Tegaderm), lubricating eye drops (EU MDR Rule 5/21), simple vascular/urinary access tubing, dental fillings, general medical device sterilisers/autoclaves (EU MDR Rule 16 -- NOT high-level disinfectants or final sterilisers specifically for invasive surgical devices, which are Class IIb), and standard active diagnostic/monitoring devices or SaMD informing clinical decisions without immediate life-threatening risk (EU MDR Rule 10 & Rule 11: e.g., ALL non-ionising diagnostic ultrasound systems [GE Voluson, Philips EPIQ], intraoperative nerve monitors [NIM], ECG/foetal monitors, TENS units, radiology/stroke triage AI software [Aidoc, Brainomix], ECG/arrhythmia screening software [KardiaAI], and digital mental-health CBT software [Sleepio]), plus hypodermic syringes, IV cannulas, Foley catheters, hearing aids, and contact lenses.",
    "- Class IIb: Long-term implantable osteosynthesis/soft-tissue/ophthalmic devices (including intraocular lenses [IOLs], which are EU MDR Class IIb, NOT Class III), active vital-signs/critical monitoring devices where parameter variations pose immediate patient danger (EU MDR Rule 10: e.g., ICU/bedside multi-parameter patient monitors [Philips IntelliVue], pulse oximeters/CO-oximeters [Masimo]), SaMD for contraception or whose decisions could cause serious deterioration or surgical intervention (EU MDR Rule 11), high-energy active therapeutic/life-support devices (EU MDR Rule 9: e.g., therapeutic ultrasound units, ICU ventilators, infusion pumps, electrosurgical units, defibrillators), high-level chemical disinfectants and final sterilisers specifically intended for invasive medical devices/endoscopes (EU MDR Rule 16: e.g., Cidex OPA, invasive instrument autoclaves), contact lens disinfecting solutions (EU MDR Rule 16), and ionising/high-field radiology equipment (e.g., bone plates/screws, pedicle screw/rod systems, intramedullary nails, synthetic surgical mesh, ureteral stents, X-ray/CT/MRI scanners, blood bags).",
    "- Class III: Highest-risk life-sustaining, active implantable, absorbable, or biological/medicinal devices (EU MDR Rules 8, 14, 18): all active implantable devices (pacemakers, ICDs, deep brain stimulators, spinal cord/vagus nerve stimulators, cochlear implants), cardiovascular implants (heart valves, coronary/vascular stents, aneurysm coils), devices incorporating an ancillary medicinal substance (drug-eluting stents, antibiotic-loaded bone cement, antimicrobial silver/iodine dressings), CNS/brain implants (CSF shunts, dural grafts), total/partial joint replacements (hip, knee, shoulder), spinal interbody cages and total disc replacements, breast implants and breast tissue expanders, contraceptive IUDs, absorbable sutures (e.g., Vicryl, PDS, Monocryl), and internal absorbable surgical haemostats, sealants, or collagen/gelatin/thrombin matrices (e.g., Floseal, Surgicel, Tisseel, Tachosil).",
    "",
    "### PART 2B: EU/UK IVDR RISK CLASS RULES (In Vitro Diagnostic Devices)",
    "Use `IVD (Class A)` through `IVD (Class D)` ONLY for devices that examine human specimens (blood, urine, saliva, swabs, tissue) outside the body:",
    "- IVD (Class A): Low individual and public health risk; ALL general laboratory instruments and automated IVD analysers themselves (e.g., Sysmex haematology analysers, clinical chemistry analysers -- IVDR Rule 5(b)), blood collection tubes, specimen receptacles/urine sample pots, buffer/wash solutions, and standard microbiological culture media.",
    "- IVD (Class B): Moderate individual risk / low public health risk (IVDR Rule 6); home pregnancy/ovulation tests, urine chemistry dipsticks, drug-of-abuse screening cups/strips, routine clinical chemistry ethanol/alcohol assays (e.g., CEDIA Ethanol), and general clinical chemistry/thyroid/vitamin assays.",
    "- IVD (Class C): High individual risk / moderate public health risk; blood glucose meters and blood glucose/ketone test strips, coagulation/INR self-testing monitors and strips (e.g., CoaguChek), companion diagnostics, cancer screening/staging markers (e.g., PSA, FOBT/FIT), prenatal/congenital screening, and non-Class-D infectious disease assays (e.g., SARS-CoV-2, influenza, STI tests).",
    "- IVD (Class D): Highest public health and individual risk; tests for life-threatening transmissible agents in blood/transplant screening or high-consequence pathogens (e.g., HIV, Hepatitis B, Hepatitis C, HTLV, Syphilis, Ebola) and high-risk ABO/Rh/Kell blood grouping and tissue typing.",
    "",
    "### PART 2C: IMPLANTABLE, ACTIVE, AND SOFTWARE RULES",
    "- `is_implantable`: 'yes' ONLY if the device is intended to be totally or partially introduced into the human body (including body orifices) via surgical/clinical intervention and remain in place after the procedure (or remain >=30 days), including joint replacements, bone screws/plates, spinal cages, jawbone dental implants, stents, pacemakers, heart valves, breast implants/expanders, surgical mesh, IOLs, IUDs, vaginal pessaries, cochlear implants, dural grafts, long-term enteral feeding tubes (PEG), and internal absorbable surgical haemostats/sealants. 'no' for external devices, IVDs, syringes, dressings, eye/ear/nasal drops, catheters/tubes used short-term (<30 days, e.g., Foley or NG tubes), capital equipment, standalone software, and tooth-surface dental restorative materials (e.g., dental fillings, composite resins, crowns, or orthodontic brackets).",
    "- `is_active`: 'yes' if the device depends on a source of electrical energy (mains or battery) or any power source other than the human body or gravity, OR is standalone medical software (SaMD) -- e.g., pacemakers, ICDs, infusion pumps, ventilators, imaging scanners, diagnostic meters/analysers, digital battery-powered home test readers (e.g., Clearblue Digital), hearing aids, TENS units, electrosurgical generators, and SaMD. 'no' for passive mechanical devices, non-electronic implants (joint prostheses, stents, screws), passive test strips/dipsticks alone, syringes, catheters, lubricating eye drops, and wound dressings.",
    "- `is_software`: 'yes' ONLY if the item is standalone Software as a Medical Device (SaMD), a regulated mobile/web medical app, digital therapeutic, or standalone AI/clinical decision support software (NOTE: all SaMD must also have `is_active: 'yes'` and `is_implantable: 'no'`). 'no' for physical hardware devices -- EVEN IF they contain embedded firmware or software (such as pacemakers, ventilators, infusion pumps, or MRI scanners) -- as well as all passive devices and IVD consumables.",
    "",
    "### PART 3: CALIBRATION EXAMPLES",
    "- \"Diprobase cream\" -> is_medical_device: \"no\", device_type: \"Topical emollient cream (non-device)\", fda_medical_speciality: \"Not Applicable\", mhra_medical_speciality: \"Not Applicable\", ema_medical_speciality: \"Not Applicable\", emdn_category: \"Not Applicable\", device_class: \"Not Applicable\", is_implantable: \"Not Applicable\", is_active: \"Not Applicable\", is_software: \"Not Applicable\"",
    "- \"Ensure Plus milkshake style liquid\" -> is_medical_device: \"no\", device_type: \"Oral nutritional supplement drink (non-device)\", fda_medical_speciality: \"Not Applicable\", mhra_medical_speciality: \"Not Applicable\", ema_medical_speciality: \"Not Applicable\", emdn_category: \"Not Applicable\", device_class: \"Not Applicable\", is_implantable: \"Not Applicable\", is_active: \"Not Applicable\", is_software: \"Not Applicable\"",
    "- \"EpiPen 300 micrograms auto-injector\" -> is_medical_device: \"no\", device_type: \"Pre-filled adrenaline auto-injector medicine (non-device)\", fda_medical_speciality: \"Not Applicable\", mhra_medical_speciality: \"Not Applicable\", ema_medical_speciality: \"Not Applicable\", emdn_category: \"Not Applicable\", device_class: \"Not Applicable\", is_implantable: \"Not Applicable\", is_active: \"Not Applicable\", is_software: \"Not Applicable\"",
    "- \"Hylo-Forte 0.2% eye drops\" -> is_medical_device: \"yes\", device_type: \"Sodium hyaluronate lubricating eye drops\", fda_medical_speciality: \"Ophthalmic\", mhra_medical_speciality: \"Senses, Movement & Pain\", ema_medical_speciality: \"Ophthalmology\", emdn_category: \"Q - Dental, Ophthalmologic and ENT Devices\", device_class: \"Class IIa\", is_implantable: \"no\", is_active: \"no\", is_software: \"no\"",
    "- \"FreeStyle Optium blood glucose test strips\" -> is_medical_device: \"yes\", device_type: \"Self-testing blood glucose diagnostic strips\", fda_medical_speciality: \"Chemistry\", mhra_medical_speciality: \"Metabolic & Renal Systems\", ema_medical_speciality: \"In vitro diagnostic medical devices\", emdn_category: \"W - In Vitro Diagnostic Medical Devices\", device_class: \"IVD (Class C)\", is_implantable: \"no\", is_active: \"no\", is_software: \"no\"",
    "- \"CoaguChek XS PT test strips\" -> is_medical_device: \"yes\", device_type: \"Point-of-care coagulation INR test strips\", fda_medical_speciality: \"Hematology\", mhra_medical_speciality: \"Oncology, Haematology & Thrombotic Disease\", ema_medical_speciality: \"In vitro diagnostic medical devices\", emdn_category: \"W - In Vitro Diagnostic Medical Devices\", device_class: \"IVD (Class C)\", is_implantable: \"no\", is_active: \"no\", is_software: \"no\"",
    "- \"Clearblue Rapid Detection Pregnancy Test\" -> is_medical_device: \"yes\", device_type: \"Home urine hCG pregnancy test\", fda_medical_speciality: \"Obstetrical and Gynecological\", mhra_medical_speciality: \"Vaccines, Infectious Diseases & Diagnostics\", ema_medical_speciality: \"In vitro diagnostic medical devices\", emdn_category: \"W - In Vitro Diagnostic Medical Devices\", device_class: \"IVD (Class B)\", is_implantable: \"no\", is_active: \"no\", is_software: \"no\"",
    "- \"MENTOR CPX 2 Breast Tissue Expander\" -> is_medical_device: \"yes\", device_type: \"Breast reconstruction tissue expander implant\", fda_medical_speciality: \"General and Plastic Surgery\", mhra_medical_speciality: \"Musculoskeletal, Trauma & Cosmetic\", ema_medical_speciality: \"General and plastic surgery and dentistry\", emdn_category: \"P - Implantable Prosthetic and Osteosynthesis Devices\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", is_software: \"no\"",
    "- \"ReLine (Nuvasive, Inc.)\" -> is_medical_device: \"yes\", device_type: \"Posterior spinal pedicle screw fixation system\", fda_medical_speciality: \"Orthopedic\", mhra_medical_speciality: \"Musculoskeletal, Trauma & Cosmetic\", ema_medical_speciality: \"Orthopaedics, traumatology, rehabilitation, rheumatology\", emdn_category: \"P - Implantable Prosthetic and Osteosynthesis Devices\", device_class: \"Class IIb\", is_implantable: \"yes\", is_active: \"no\", is_software: \"no\"",
    "- \"Floseal Hemostatic Matrix\" -> is_medical_device: \"yes\", device_type: \"Absorbable bovine gelatin thrombin surgical haemostat\", fda_medical_speciality: \"General and Plastic Surgery\", mhra_medical_speciality: \"Immunology, Biocompatibility & Non-Clinical\", ema_medical_speciality: \"General and plastic surgery and dentistry\", emdn_category: \"M - Devices for General and Specialist Dressings\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", is_software: \"no\"",
    "- \"Stryker Triathlon CR Femoral Component\" -> is_medical_device: \"yes\", device_type: \"Total knee replacement femoral implant\", fda_medical_speciality: \"Orthopedic\", mhra_medical_speciality: \"Musculoskeletal, Trauma & Cosmetic\", ema_medical_speciality: \"Orthopaedics, traumatology, rehabilitation, rheumatology\", emdn_category: \"P - Implantable Prosthetic and Osteosynthesis Devices\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", is_software: \"no\"",
    "- \"BD Alaris GP Volumetric Pump\" -> is_medical_device: \"yes\", device_type: \"Intravenous fluid infusion pump\", fda_medical_speciality: \"General Hospital\", mhra_medical_speciality: \"Cardiovascular & Fluid Management\", ema_medical_speciality: \"Respiratory system, anaesthesiology, intensive care\", emdn_category: \"Z - Medical Equipment and Related Accessories, Software and Consumables\", device_class: \"Class IIb\", is_implantable: \"no\", is_active: \"yes\", is_software: \"no\"",
    "- \"Medtronic Micra AV\" -> is_medical_device: \"yes\", device_type: \"Leadless implantable cardiac pacemaker\", fda_medical_speciality: \"Cardiovascular\", mhra_medical_speciality: \"Cardiovascular & Fluid Management\", ema_medical_speciality: \"Circulatory system\", emdn_category: \"J - Active-Implantable Devices\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"yes\", is_software: \"no\"",
    "- \"Brainomix e-Stroke AI software\" -> is_medical_device: \"yes\", device_type: \"Standalone AI stroke CT imaging software\", fda_medical_speciality: \"Radiology\", mhra_medical_speciality: \"Respiratory, Imaging & Critical Care\", ema_medical_speciality: \"Neurology\", emdn_category: \"Z - Medical Equipment and Related Accessories, Software and Consumables\", device_class: \"Class IIa\", is_implantable: \"no\", is_active: \"yes\", is_software: \"yes\"",
    sep = "\n"
  )
}
