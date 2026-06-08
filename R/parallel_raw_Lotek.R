#' Process multiple raw Lotek TXT files in parallel
#'
#' @description
#' Reads and processes a directory of raw Lotek TXT files exported by the WHS
#' Host batch conversion tool. Each file is processed using
#' `process_single_raw()`, and the work is parallelized across multiple CPU
#' cores with the \pkg{future}/\pkg{furrr} framework so that multiple raw files
#' can be handled simultaneously. Constant arguments are passed explicitly to
#' each worker, so no manual `clusterExport()` is required.
#'
#' @param input_path Path to directory containing raw `.TXT` files.
#' @param raw_files Character vector of TXT filenames to process. Defaults to
#'   all `.TXT` files in the directory.
#' @param output_path Directory where processed CSV files will be written.
#' @param output_prefix Optional prefix to add to output filenames.
#' @param AllowableTagCodes Optional vector of tag IDs to allow and remove all
#'   others. This is highly recommended whenever possible. It greatly increases
#'   processing speed and decreases file size and resource requirements.
#' @param tz Character; timezone for timestamp parsing (default `"Etc/GMT+8"`).
#' @param n_cores Number of CPU cores (future workers) to use for parallel
#'   processing.
#'
#' @return Invisibly returns `TRUE` after processing all files.
#'
#' @export
parallel_raw_Lotek <- function(
    input_path,
    raw_files = list.files(input_path, pattern = "\\.TXT$", ignore.case = TRUE),
    output_path,
    output_prefix = "",
    AllowableTagCodes = NULL,
    tz = "Etc/GMT+8",
    n_cores = max(1, round(parallel::detectCores() / 2))
) {

  # ---- Normalize paths (absolute paths are safer to hand to workers) ----
  input_dir_norm  <- normalizePath(input_path,  mustWork = FALSE)
  output_dir_norm <- normalizePath(output_path, mustWork = FALSE)

  # ---- Ensure output directory exists ----
  if (!dir.exists(output_dir_norm)) {
    message("Creating output directory: ", output_dir_norm)
    dir.create(output_dir_norm, recursive = TRUE)
  }

  # ---- Guard against an empty file list ----
  if (length(raw_files) == 0) {
    stop("No .TXT files found to process in: ", input_dir_norm)
  }

  # ---- Parallel plan (mirrors parallel_filter_Lotek) ----
  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # ---- Process raw files in parallel ----
  # Every value the worker needs is passed as a named argument to future_walk(),
  # which forwards it to each call of process_single_raw(). The arguments travel
  # with the call itself, so there is no dependence on environment serialization
  # or clusterExport(). future_walk() is used (not future_map) because we only
  # care about the side effect of writing the CSVs, not any return value.
  furrr::future_walk(
    .x                = raw_files,
    .f                = LotekFilter::process_single_raw,
    input_path        = input_dir_norm,
    output_path       = output_dir_norm,
    output_prefix     = output_prefix,
    AllowableTagCodes = AllowableTagCodes,
    tz                = tz,
    .progress         = TRUE,
    .options          = furrr::furrr_options(
      packages = c("LotekFilter", "dplyr", "readr", "broman")
    )
  )

  message("\nAll raw files processed.")
  invisible(TRUE)
}
