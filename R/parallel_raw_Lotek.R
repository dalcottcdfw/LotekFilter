#' Process multiple raw Lotek TXT files in parallel
#'
#' @description
#' Reads and processes a directory of raw Lotek `.TXT` files exported by the
#' WHS Host batch conversion tool. Each file is passed to
#' [process_single_raw()] and processed independently. Parallelization is
#' performed through the \pkg{future}/\pkg{furrr} framework, allowing multiple
#' raw files to be handled simultaneously. All required arguments are
#' forwarded to worker processes explicitly.
#'
#' Input `.TXT` files must be unencrypted WHS Host exports containing standard
#' Lotek receiver message blocks and detection tables.
#'
#' @param input_path Path to the directory containing raw `.TXT` files.
#' @param raw_files Character vector of `.TXT` filenames to process. Defaults to
#'   all `.TXT` files in `input_path` (case-insensitive).
#' @param output_path Directory where processed csv files will be written.
#'   Created if it does not already exist.
#' @param output_prefix Optional prefix added to the output csv filenames.
#' @param allowable_tagcodes Optional vector of hexadecimal tag IDs to retain.
#'   Records with tag IDs not present in this vector are removed. Highly
#'   recommended to reduce file size and increase processing speed.
#' @param tz Character string giving the timezone used for timestamp parsing.
#'   Defaults to `"Etc/GMT+8"`.
#' @param n_cores Number of CPU cores to use for parallel processing.
#'   Default is half the available cores (rounded to whole number).
#'
#' @return
#' Invisibly returns `TRUE` after all raw files are processed.
#' Writes one processed csv per input file into `output_path`.
#'
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_walk furrr_options
#'
#' @examples
#' \dontrun{
#' # Example directory containing raw TXT files
#' raw_dir <- system.file("extdata", package = "LotekFilter")
#'
#' parallel_raw_Lotek(
#'   input_path   = raw_dir,
#'   output_path  = tempdir(),
#'   output_prefix = "Processed_",
#'   allowable_tagcodes = c("1A2B", "4D5E"),
#'   tz = "Etc/GMT+8",
#'   n_cores = 2
#' )
#' }
#'
#' @export
parallel_raw_Lotek <- function(
    input_path,
    raw_files = list.files(input_path, pattern = "\\.TXT$", ignore.case = TRUE),
    output_path,
    output_prefix = "",
    allowable_tagcodes = NULL,
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

  # ---- Parallel plan (mirrors parallel_filter) ----
  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # ---- Process raw files in parallel ----
  # Every value the worker needs is passed as a named argument to future_walk(),
  # which forwards it to each call of process_single_raw(). The arguments travel
  # with the call itself, so there is no dependence on environment serialization
  # or clusterExport(). future_walk() is used (not future_map) because we only
  # care about the side effect of writing the csvs, not any return value.
  furrr::future_walk(
    .x                = raw_files,
    .f                = process_single_raw,
    input_path        = input_dir_norm,
    output_path       = output_dir_norm,
    output_prefix     = output_prefix,
    allowable_tagcodes = allowable_tagcodes,
    tz                = tz,
    .progress         = TRUE,
    .options          = furrr::furrr_options(
      packages = c("LotekFilter", "dplyr", "readr", "broman")
    )
  )

  message("\nAll raw files processed.")
  invisible(TRUE)
}
