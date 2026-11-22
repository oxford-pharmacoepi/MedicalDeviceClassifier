#' Classify medical devices
#'
#' @description
#' Uses an \pkg{ellmer} `Chat` object (e.g., `ellmer::chat_google_gemini()` or
#' `ellmer::chat_ollama()`) to classify device data on the following:
#'
#' * **Is a device:** Flags whether each item is truly a medical device
#'   (`"yes"` or `"no"`), excluding non-device products such as creams,
#'   pharmaceuticals, and nutritional feeds.
#' * **Class:** Assigns medical devices to an EU/UK MDR risk class
#'   (`Class I`, `Class IIa`, `Class IIb`, `Class III`) or EU/UK IVDR risk class
#'   (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`, `IVD (Class D)`).
#' * **Medical speciality:** Assigns true medical devices to a clinical
#'   speciality (`Anesthesiology`, `Cardiovascular`, `Chemistry`, `Dental`,
#'   `Ear, Nose, and Throat (ENT)`, `Gastroenterology and Urology`,
#'   `General and Plastic Surgery`, `General Hospital`, `Hematology`,
#'   `Immunology`, `Neurology`, `Obstetrical and Gynecological`, `Ophthalmic`,
#'   `Orthopedic`, `Pathology`, `Physical Medicine`, `Radiology`, `Toxicology`,
#'   or `Other`).
#' * **Implantable:** Flags whether medical devices are  implantable (`"yes"`
#' or `"no"`).
#' * **Active status:** Flags whether medical devices are active/powered
#' (`"yes"` or `"no"`).
#' * **Confidence:** Classification confidence (`"high"`, `"medium"`,
#'   or `"low"`) for each evaluated item.
#'
#' @param chat An \pkg{ellmer} `Chat` object (such as from
#'   `ellmer::chat_openai(model = "gpt-4o-mini")` or
#'   `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).
#' @param data A `data.frame` or `tibble` containing a `device_name` column
#' which will be used to classify devices.
#' @param maxRetries Maximum number of attempts for any concept before returning
#' `NA_character_`
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
#'     \item `medical_speciality`: Character (one of the 19 medical specialities,
#'       or `NA_character_` if not a device or failed).
#'     \item `is_implantable`: Character (`"yes"`, `"no"`, or `NA_character_`
#'       if not a device or failed).
#'     \item `is_active`: Character (`"yes"`, `"no"`, or `NA_character_` if not
#'       a device or failed).
#'     \item `confidence`: Character (`"high"`, `"medium"`, `"low"`, or
#'       `NA_character_` if failed).
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
    "medical_speciality",
    "is_implantable",
    "is_active",
    "confidence"
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
  allowed_confidence <- c("high", "medium", "low")
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
  allowed_specialities <- c(
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
          description = "'yes' if the item is truly a regulated medical device (including in vitro diagnostics); 'no' if it is a pharmaceutical, topical cream/emollient, nutritional feed, cosmetic, or other non-device product."
        ),
        device_type = ellmer::type_string(
          description = "3 to 6 word summary of what the item is (e.g., 'breast reconstruction tissue expander', 'blood glucose test strips', 'topical emollient cream (non-device)')."
        ),
        medical_speciality = ellmer::type_enum(
          values = c(allowed_specialities, "Not Applicable"),
          description = "Primary medical speciality category for the device, or 'Not Applicable' if is_medical_device is 'no'."
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
        confidence = ellmer::type_enum(
          values = allowed_confidence,
          description = "Overall confidence in the classification ('high', 'medium', or 'low')."
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
          medical_speciality = NA_character_,
          is_implantable = NA_character_,
          is_active = NA_character_,
          confidence = NA_character_
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
    out_spec <- rep(NA_character_, n_b)
    out_implantable <- rep(NA_character_, n_b)
    out_active <- rep(NA_character_, n_b)
    out_confidence <- rep(NA_character_, n_b)
    pending_idx <- seq_len(n_b)
    batch_cost <- 0
    has_cost_data <- FALSE

    required_fields <- c(
      "id",
      "is_medical_device",
      "device_class",
      "medical_speciality",
      "is_implantable",
      "is_active",
      "confidence"
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
          m_spc <- as.character(items_df$medical_speciality[[r]])
          is_imp <- tolower(as.character(items_df$is_implantable[[r]]))
          is_act <- tolower(as.character(items_df$is_active[[r]]))
          conf <- tolower(as.character(items_df$confidence[[r]]))

          if (!is.na(item_id) &&
              item_id >= 1L &&
              item_id <= length(curr_concepts) &&
              !is.na(is_dev) && is_dev %in% allowed_yes_no &&
              !is.na(conf) && conf %in% allowed_confidence) {
            orig_pos <- pending_idx[[item_id]]

            if (is_dev == "no") {
              out_is_device[[orig_pos]] <- "no"
              out_class[[orig_pos]] <- NA_character_
              out_spec[[orig_pos]] <- NA_character_
              out_implantable[[orig_pos]] <- NA_character_
              out_active[[orig_pos]] <- NA_character_
              out_confidence[[orig_pos]] <- conf
            } else if (!is.na(d_cls) && d_cls %in% allowed_classes &&
                       !is.na(m_spc) && m_spc %in% allowed_specialities &&
                       !is.na(is_imp) && is_imp %in% allowed_yes_no &&
                       !is.na(is_act) && is_act %in% allowed_yes_no) {
              out_is_device[[orig_pos]] <- "yes"
              out_class[[orig_pos]] <- d_cls
              out_spec[[orig_pos]] <- m_spc
              out_implantable[[orig_pos]] <- is_imp
              out_active[[orig_pos]] <- is_act
              out_confidence[[orig_pos]] <- conf
            }
          }
        }
      }

      pending_idx <- which(
        is.na(out_is_device) |
          is.na(out_confidence) |
          (out_is_device == "yes" &
             (is.na(out_class) |
                is.na(out_spec) |
                is.na(out_implantable) |
                is.na(out_active)))
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
        medical_speciality = out_spec,
        is_implantable = out_implantable,
        is_active = out_active,
        confidence = out_confidence
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
        is.na(batch_res$confidence) |
        (batch_res$is_medical_device == "yes" &
           (is.na(batch_res$device_class) |
              is.na(batch_res$medical_speciality) |
              is.na(batch_res$is_implantable) |
              is.na(batch_res$is_active)))
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
    "You are an expert EU/UK MDR, IVDR, and FDA medical device regulatory specialist.",
    "You will receive a numbered list of product names from routinely collected clinical data (e.g., NHS dm+d, hospital procurement, or registry records, which may include brand names, manufacturer names, catalogue abbreviations, or non-device products).",
    "For EVERY numbered item in the list, return an object with:",
    "  1. `id`: The exact integer ID matching the input line.",
    "  2. `is_medical_device`: 'yes' if the item is truly a regulated medical device (including in vitro diagnostic devices), or 'no' if it is NOT a medical device.",
    "  3. `device_type`: A concise 3-6 word description identifying what the item is.",
    "  4. `medical_speciality`: The regulatory speciality category (choose strictly from the 19 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  5. `device_class`: The EU/UK MDR risk class (`Class I`, `Class IIa`, `Class IIb`, or `Class III`) for general medical devices, OR the EU/UK IVDR risk class (`IVD (Class A)`, `IVD (Class B)`, `IVD (Class C)`, or `IVD (Class D)`) for in vitro diagnostic devices, or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  6. `is_implantable`: 'yes' or 'no' (or 'Not Applicable' if `is_medical_device` is 'no').",
    "  7. `is_active`: 'yes' or 'no' (or 'Not Applicable' if `is_medical_device` is 'no').",
    "  8. `confidence`: 'high', 'medium', or 'low' reflecting certainty in the overall classification.",
    "Evaluate each item independently -- never let adjacent items in a batch bias the current item.",
    "",
    "### PART 0: MEDICAL DEVICE ELIGIBILITY (`is_medical_device`)",
    "- Return `is_medical_device: 'no'` (with `medical_speciality`, `device_class`, `is_implantable`, and `is_active` all set to `'Not Applicable'`) for:",
    "  * Topical creams, ointments, emollients, lotions, gels, bath additives, shampoos, barrier creams, and sunscreens (e.g., Diprobase, Epaderm, Aveeno, Aqueous cream, Hydrocortisone cream).",
    "  * Pharmaceuticals and medicinal products: tablets, capsules, oral solutions, injections, IV drug infusions, vaccines, eye/ear/nasal drops, inhalers containing drugs, lozenges, suppositories.",
    "  * Nutritional and dietary products: oral nutritional supplements, enteral feeds/formulas (e.g., Ensure, Fortisip), infant formulas, thickeners, gluten-free foods, vitamins/minerals.",
    "  * Cosmetics, toiletries, soaps, toothpastes, mouthwashes, and general non-medical consumables.",
    "- Return `is_medical_device: 'yes'` for true regulated medical devices (including IVDs) whose primary mode of action is physical/mechanical/diagnostic rather than pharmacological/metabolic, including: implants, surgical instruments, catheters, syringes/needles, wound dressings/bandages, ostomy/incontinence appliances, compression hosiery, diagnostic test strips/meters, laboratory/point-of-care IVD assays and analysers, ventilators, imaging systems, and internal surgical haemostats/sealants (e.g., Floseal, Surgicel, Tisseel).",
    "",
    "### PART 1: MEDICAL SPECIALITY RULES (Classify by Regulatory Anatomy/Function, Not Surgeon Type)",
    "- Anesthesiology: ICU/transport ventilators, anaesthesia workstations, vaporisers, endotracheal tubes, epidural needles, pulse oximeters/capnographs (NOT tracheostomy tubes or LMAs, which belong in ENT).",
    "- Cardiovascular: Pacemakers, ICDs, cardiac resynchronisation devices, heart valves, vascular/coronary stents, angioplasty balloons, cardiac guidewires, ablation catheters, dedicated haemodynamic/ECG monitors, vascular grafts (e.g., Medtronic Cardiac, Edwards, Boston Scientific, Abbott Vascular -- NOT multi-parameter bedside/ICU patient monitors, which belong in General Hospital).",
    "- Chemistry: Clinical chemistry analysers, blood gas analysers, blood glucose meters/strips, blood ketone test strips, HbA1c/lipid/electrolyte analysers, urine chemistry dipsticks (NOT empty urine specimen pots/containers, which belong in Pathology).",
    "- Dental: Dental implants, dental handpieces, orthodontic brackets/wires, dental amalgam/composites, synthetic dental bone grafts, endodontic files.",
    "- Ear, Nose, and Throat (ENT): Hearing aids, cochlear implants, tympanostomy tubes, nasal dilators/packing, laryngeal mask airways (LMAs), tracheostomy tubes, ENT endoscopes, otoscopes.",
    "- Gastroenterology and Urology: GI endoscopes, capsule endoscopy systems, colonoscopes, ERCP catheters, lithotripters, haemodialysis/CRRT machines and dialysers, Foley/urinary catheters, ureteral stents, nephrostomy tubes, enteral feeding tubes (PEG/NG), ostomy bags/appliances.",
    "- General and Plastic Surgery: Breast implants, breast tissue expanders, dermal fillers, surgical mesh (hernia/soft-tissue), sutures, skin/internal staplers, wound dressings/NPWT, electrosurgical generators/pencils, laparoscopic trocars, scalpels, sterile surgical gloves, internal absorbable haemostatic agents and surgical sealants (e.g., Mentor, Allergan/Natrelle, Ethicon, Floseal, Surgicel, Tisseel -- NOT dural grafts/matrices, which belong in Neurology).",
    "- General Hospital: Hospital beds, multi-parameter bedside/ICU patient monitors (e.g., Philips IntelliVue), infusion pumps, syringe pumps, IV administration sets/cannulas, clinical thermometers, hypodermic syringes/needles, non-sterile examination gloves, patient lifts, sharps containers.",
    "- Hematology: Coagulation/INR analysers and test strips, automated blood cell counters (CBC), haemoglobinometers, blood grouping/cross-matching reagents, dedicated blood bank/immunohematology centrifuges (NOT general laboratory centrifuges, which belong in Pathology).",
    "- Immunology: Viral/bacterial serological tests, PCR/NAT infectious disease assays (e.g., HIV, Hepatitis, SARS-CoV-2, influenza), blood culture media, microbiology susceptibility plates, autoimmune/allergy antibody detection kits.",
    "- Neurology: Devices interacting directly with brain, spinal cord tissue, dura mater, nerves, or CSF: deep brain stimulators (DBS), spinal cord/vagus nerve stimulators, CSF shunts, dural grafts/regeneration matrices (e.g., DuraGen), intracranial pressure monitors, aneurysm coils/flow diverters, EEGs, intraoperative neuromonitoring (e.g., NIM) -- NEVER structural spinal bone screws, rods, or fusion cages.",
    "- Obstetrical and Gynecological: Foetal monitors (CTG), contraceptive IUDs, vaginal pessaries, colposcopes, vaginal speculums, obstetrical forceps/vacuum extractors, hysteroscopes, endometrial ablation devices, home pregnancy/ovulation test kits.",
    "- Ophthalmic: Contact lenses, intraocular lenses (IOLs), phacoemulsification systems, ophthalmic surgical lasers, tonometers, viscoelastics, ophthalmic surgical knives.",
    "- Orthopedic: STRICTLY internal/surgical musculoskeletal, bone, joint, and structural spine hardware (including spine implants placed by neurosurgeons): total/partial hip, knee, shoulder, or ankle joint replacements; bone fixation plates, screws, and intramedullary nails; pedicle screw/rod systems; spinal interbody fusion cages and disc prostheses; bone cement; arthroscopy instruments (e.g., DePuy Synthes, Stryker, Zimmer Biomet, NuVasive, Globus Medical, Smith+Nephew -- NEVER external braces/orthoses, which belong in Physical Medicine).",
    "- Pathology: Histopathology tissue processors, microtomes, cryostats, histology slide stainers, immunohistochemistry (IHC) reagents/stainers, cytology processors, general clinical laboratory centrifuges, blood collection tubes (e.g., Vacutainer), and specimen collection containers/pots.",
    "- Physical Medicine: Powered/manual wheelchairs, TENS units, neuromuscular electrical stimulators, therapeutic ultrasound/diathermy units, traction equipment, external limb prostheses/orthoses/braces (e.g., knee/ankle/wrist braces), crutches/walkers.",
    "- Radiology: CT scanners, MRI systems, X-ray machines, fluoroscopy C-arms, diagnostic ultrasound systems, mammography units, nuclear medicine/PET scanners, radiation therapy linear accelerators -- NOT catheters or stents placed under imaging guidance, and NOT physical therapy ultrasound units.",
    "- Toxicology: Drug-of-abuse screening cups/strips, therapeutic drug monitoring (TDM) assays, blood alcohol/ethanol test kits, heavy metal/poisoning diagnostic assays.",
    "- Other: Sterilization/CSSD autoclaves, general facility equipment, cross-speciality software, or non-specific consumables that cannot be mapped to any of the 18 clinical specialities above.",
    "",
    "### PART 2A: EU/UK MDR RISK CLASS RULES (General Medical Devices)",
    "- Class I: ONLY strictly non-invasive devices touching intact skin or simple reusable surgical/dental hand instruments (EU MDR Rule 1 & Rule 6: e.g., hospital beds, wheelchairs, walking frames, manual scalpels, forceps, reusable stainless steel dental mouth mirrors, stethoscopes, non-sterile external intact-skin bandages, vaginal speculums). NEVER assign Class I to any implantable, absorbable, sterile invasive, wound micro-environment film dressing, biological/animal-derived agent, or active body-orifice diagnostic device (such as intraoral scanners or otoscopes).",
    "- Class IIa: Transient/short-term invasive devices (<30 days) including active or disposable devices inserted into body orifices (EU MDR Rule 5: e.g., intraoral scanners, otoscopes, laryngeal masks, tracheostomy tubes), sterile transparent film dressings managing wound/catheter-site micro-environments (EU MDR Rule 4: e.g., 3M Tegaderm), simple vascular/urinary access tubing, dental fillings, general medical device sterilisers/autoclaves (EU MDR Rule 16, unless specifically for final sterilisation of invasive surgical devices), and standard active diagnostic/monitoring devices without immediate vital-parameter danger (EU MDR Rule 10: e.g., ALL non-ionising diagnostic ultrasound systems [GE Voluson, Philips EPIQ], intraoperative nerve monitors [NIM], ECG/foetal monitors, TENS units), plus hypodermic syringes, IV cannulas, Foley catheters, hearing aids, and contact lenses.",
    "- Class IIb: Long-term implantable osteosynthesis/soft-tissue/ophthalmic devices (including intraocular lenses [IOLs], which are EU MDR Class IIb, NOT Class III), active vital-signs/critical monitoring devices where parameter variations pose immediate patient danger (EU MDR Rule 10: e.g., ICU/bedside multi-parameter patient monitors [Philips IntelliVue], pulse oximeters/CO-oximeters [Masimo]), high-energy active therapeutic/life-support devices (EU MDR Rule 9: e.g., therapeutic ultrasound units, ICU ventilators, infusion pumps, electrosurgical units, defibrillators), and ionising/high-field radiology equipment (e.g., bone plates/screws, pedicle screw/rod systems, intramedullary nails, synthetic surgical mesh, ureteral stents, X-ray/CT/MRI scanners, blood bags).",
    "- Class III: Highest-risk life-sustaining, active implantable, absorbable, or biological/medicinal devices (EU MDR Rules 8, 14, 18): all active implantable devices (pacemakers, ICDs, deep brain stimulators, spinal cord/vagus nerve stimulators, cochlear implants), cardiovascular implants (heart valves, coronary/vascular stents, aneurysm coils), CNS/brain implants (CSF shunts, dural grafts), total/partial joint replacements (hip, knee, shoulder), spinal interbody cages and total disc replacements, breast implants and breast tissue expanders, contraceptive IUDs, absorbable sutures (e.g., Vicryl, PDS, Monocryl), and internal absorbable surgical haemostats, sealants, or collagen/gelatin/thrombin matrices (e.g., Floseal, Surgicel, Tisseel, Tachosil).",
    "",
    "### PART 2B: EU/UK IVDR RISK CLASS RULES (In Vitro Diagnostic Devices)",
    "Use `IVD (Class A)` through `IVD (Class D)` ONLY for devices that examine human specimens (blood, urine, saliva, swabs, tissue) outside the body:",
    "- IVD (Class A): Low individual and public health risk; ALL general laboratory instruments and automated IVD analysers themselves (e.g., Sysmex haematology analysers, clinical chemistry analysers -- IVDR Rule 5(b)), blood collection tubes, specimen receptacles/urine sample pots, buffer/wash solutions, and standard microbiological culture media.",
    "- IVD (Class B): Moderate individual risk / low public health risk (IVDR Rule 6); home pregnancy/ovulation tests, urine chemistry dipsticks, drug-of-abuse screening cups/strips, routine clinical chemistry ethanol/alcohol assays (e.g., CEDIA Ethanol), and general clinical chemistry/thyroid/vitamin assays.",
    "- IVD (Class C): High individual risk / moderate public health risk; blood glucose meters and blood glucose/ketone test strips, coagulation/INR self-testing monitors and strips (e.g., CoaguChek), companion diagnostics, cancer screening/staging markers (e.g., PSA, FOBT/FIT), prenatal/congenital screening, and non-Class-D infectious disease assays (e.g., SARS-CoV-2, influenza, STI tests).",
    "- IVD (Class D): Highest public health and individual risk; tests for life-threatening transmissible agents in blood/transplant screening or high-consequence pathogens (e.g., HIV, Hepatitis B, Hepatitis C, HTLV, Syphilis, Ebola) and high-risk ABO/Rh/Kell blood grouping and tissue typing.",
    "",
    "### PART 2C: IMPLANTABLE, ACTIVE, AND CONFIDENCE RULES",
    "- `is_implantable`: 'yes' ONLY if the device is intended to be totally or partially introduced into the human body (including body orifices) via surgical/clinical intervention and remain in place after the procedure (or remain >=30 days), including joint replacements, bone screws/plates, spinal cages, jawbone dental implants, stents, pacemakers, heart valves, breast implants/expanders, surgical mesh, IOLs, IUDs, vaginal pessaries, cochlear implants, dural grafts, long-term enteral feeding tubes (PEG), and internal absorbable surgical haemostats/sealants. 'no' for external devices, IVDs, syringes, dressings, catheters/tubes used short-term (<30 days, e.g., Foley or NG tubes), capital equipment, and tooth-surface dental restorative materials (e.g., dental fillings, composite resins, crowns, or orthodontic brackets).",
    "- `is_active`: 'yes' if the device depends on a source of electrical energy (mains or battery) or any power source other than the human body or gravity, OR is standalone medical software (SaMD) -- e.g., pacemakers, ICDs, infusion pumps, ventilators, imaging scanners, diagnostic meters/analysers, digital battery-powered home test readers (e.g., Clearblue Digital), hearing aids, TENS units, electrosurgical generators. 'no' for passive mechanical devices, non-electronic implants (joint prostheses, stents, screws), passive test strips/dipsticks alone, syringes, catheters, and wound dressings.",
    "- `confidence`: 'high' when the product name clearly identifies a known device or non-device; 'medium' when the general category is clear but exact variant/risk class has minor ambiguity; 'low' when the string is cryptic, heavily abbreviated, truncated, or an unfamiliar brand.",
    "",
    "### PART 3: CALIBRATION EXAMPLES",
    "- \"Diprobase cream\" -> is_medical_device: \"no\", device_type: \"Topical emollient cream (non-device)\", medical_speciality: \"Not Applicable\", device_class: \"Not Applicable\", is_implantable: \"Not Applicable\", is_active: \"Not Applicable\", confidence: \"high\"",
    "- \"Ensure Plus milkshake style liquid\" -> is_medical_device: \"no\", device_type: \"Oral nutritional supplement drink (non-device)\", medical_speciality: \"Not Applicable\", device_class: \"Not Applicable\", is_implantable: \"Not Applicable\", is_active: \"Not Applicable\", confidence: \"high\"",
    "- \"FreeStyle Optium blood glucose test strips\" -> is_medical_device: \"yes\", device_type: \"Self-testing blood glucose diagnostic strips\", medical_speciality: \"Chemistry\", device_class: \"IVD (Class C)\", is_implantable: \"no\", is_active: \"no\", confidence: \"high\"",
    "- \"CoaguChek XS PT test strips\" -> is_medical_device: \"yes\", device_type: \"Point-of-care coagulation INR test strips\", medical_speciality: \"Hematology\", device_class: \"IVD (Class C)\", is_implantable: \"no\", is_active: \"no\", confidence: \"high\"",
    "- \"Clearblue Rapid Detection Pregnancy Test\" -> is_medical_device: \"yes\", device_type: \"Home urine hCG pregnancy test\", medical_speciality: \"Obstetrical and Gynecological\", device_class: \"IVD (Class B)\", is_implantable: \"no\", is_active: \"no\", confidence: \"high\"",
    "- \"MENTOR CPX 2 Breast Tissue Expander\" -> is_medical_device: \"yes\", device_type: \"Breast reconstruction tissue expander implant\", medical_speciality: \"General and Plastic Surgery\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", confidence: \"high\"",
    "- \"ReLine (Nuvasive, Inc.)\" -> is_medical_device: \"yes\", device_type: \"Posterior spinal pedicle screw fixation system\", medical_speciality: \"Orthopedic\", device_class: \"Class IIb\", is_implantable: \"yes\", is_active: \"no\", confidence: \"high\"",
    "- \"Floseal Hemostatic Matrix\" -> is_medical_device: \"yes\", device_type: \"Absorbable bovine gelatin thrombin surgical haemostat\", medical_speciality: \"General and Plastic Surgery\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", confidence: \"high\"",
    "- \"Stryker Triathlon CR Femoral Component\" -> is_medical_device: \"yes\", device_type: \"Total knee replacement femoral implant\", medical_speciality: \"Orthopedic\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"no\", confidence: \"high\"",
    "- \"BD Alaris GP Volumetric Pump\" -> is_medical_device: \"yes\", device_type: \"Intravenous fluid infusion pump\", medical_speciality: \"General Hospital\", device_class: \"Class IIb\", is_implantable: \"no\", is_active: \"yes\", confidence: \"high\"",
    "- \"Medtronic Micra AV\" -> is_medical_device: \"yes\", device_type: \"Leadless implantable cardiac pacemaker\", medical_speciality: \"Cardiovascular\", device_class: \"Class III\", is_implantable: \"yes\", is_active: \"yes\", confidence: \"high\"",
    sep = "\n"
  )
}
