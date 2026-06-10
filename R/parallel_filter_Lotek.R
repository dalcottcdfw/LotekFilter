#' Filter multiple Lotek detection files in parallel
#'
#' @description
#' Processes and filters multiple Lotek detection csv files in parallel. These
#' input files must contain at minimum the columns `DateTime`, `HexID`, and
#' `nominalPRI`, and are typically created by
#' [process_single_raw()] or [parallel_raw_Lotek()]. The function coordinates
#' parallel processing across available CPU cores and applies all Lotek
#' detection filtering criteria, optionally saving rejected detections and
#' producing a combined summary dataframe.
#'
#' @param input_files Character vector of csv file paths to process.
#' @param input_prefix Optional prefix used on input file names (to remove when
#'   constructing output names). Use `NA` to ignore.
#' @param output_prefix Prefix to prepend to all output filenames. Default is
#'   `"Filtered_"`.
#' @param output_path Directory where output files will be written. If the
#'   directory does not exist, it will be created.
#' @param allow_overwrite Logical; if `FALSE` (default), prevents accidental
#'   overwriting when input and output directories are the same and filename
#'   prefixes are ambiguous.
#' @param keep_rejected Logical; if `TRUE`, saves an additional csv per input
#'   file containing all rejected detections and their rejection reason.
#' @param n_cores Number of CPU cores to use for parallel processing. Defaults
#'   to half of the available cores.
#'
#' @param nominalPRI Optional numeric value giving a single nominal pulse rate
#'   interval (PRI) applied to all tags.
#' @param pri_table Optional dataframe providing tag-specific PRI values.
#' @param pri_tag_col Name of the column in `pri_table` containing hex tag IDs.
#' @param pri_value_col Name of the column in `pri_table` containing PRI values.
#'
#' @param detection_window Multiplier used to define the time window in which
#'   `min_detections` must occur. Default is 16.6 (Lotek standard).
#' @param min_detections Minimum number of detections required within the PRI
#'   window to retain a detection sequence. Default is 4.
#' @param sd_threshold Maximum allowable standard deviation of observed PRI.
#'   Default is 0.025.
#' @param multipath_threshold Time threshold (in seconds) used to remove
#'   detections suspected to be multipath reflections. Default is 0.3.
#' @param nominalPRI_threshold Proportion threshold used to evaluate deviation
#'   from nominal PRI. Default is 0.20 (20%).
#'
#' @return
#' A dataframe combining the summary results from all processed files. The
#' function also writes filtered csv files (and optionally rejected-detection
#' files) to `output_path`. The returned dataframe is also printed and returned
#' invisibly.
#'
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' input_files <- list.files(
#'   system.file("extdata", package = "LotekFilter"),
#'   pattern = "Processed_.*\\.csv$",
#'   full.names = TRUE
#' )
#'
#' summary_results <- parallel_filter_Lotek(
#'   input_files   = input_files,
#'   output_path   = tempdir(),
#'   output_prefix = "Filtered_",
#'   keep_rejected = FALSE,
#'   n_cores       = 2
#' )
#' }
#'
#' @export
parallel_filter_Lotek <- function(input_files,
                                  input_prefix = NA, # if the input files have a prefix that you want to remove when saving the output ("Raw_..." or "Prefiltered_...")
                                  output_prefix = "Filtered_",
                                  output_path,
                                  allow_overwrite = FALSE, # by default, warn user if input files might be overwritten by output files

                                  keep_rejected = FALSE, # TRUE if you want to save a copy of the rejected records with a reason for rejecting
                                  n_cores    = max(1, round(parallel::detectCores()/2)), # number of cores to use for parallel processing (1 input file per core)

                                  nominalPRI = NULL,           # optionally, provide a single numeric tag pulse rate interval to all tags OR:
                                  pri_table = NULL,            # provide name of tag pri lookup table AND
                                  pri_tag_col = NULL,          # provide name of hex tag code column in tag lookup table AND
                                  pri_value_col = NULL,        # provide name of column containing tag pulse rate interval in tags lookup table

                                  detection_window = 16.6,    # detection_window * nominmal PRI = time window that min_detections must occur to be kept. Arnold Ammann criteria = 16.6 for Lotek
                                  min_detections = 4,         # number of detections required within the detection window to be kept. Arnold Ammann criteria = 4 for Lotek.
                                  sd_threshold = 0.025,           # max threshold for standard deviation of observed PRI. Arnold Ammann criteria = 0.025
                                  multipath_threshold = 0.3,  # time threshold for multipath detections. Detections less than this seconds after first are removed. Arnold Ammann criteria = 0.3 sec
                                  nominalPRI_threshold = 0.2 # observed PRI must be within this amount of nominal PRI. Arnold Ammann criteria = 20% or 0.20.


) {
  settings <- list(
    nominalPRI = nominalPRI,
    pri_table = pri_table,
    pri_tag_col = pri_tag_col,
    pri_value_col = pri_value_col,
    detection_window = detection_window,
    min_detections = min_detections,
    sd_threshold = sd_threshold,
    multipath_threshold = multipath_threshold,
    nominalPRI_threshold = nominalPRI_threshold,

    output_path = output_path,
    input_prefix = input_prefix,
    output_prefix = output_prefix,

    keep_rejected = keep_rejected
  )

  # --- Prevent accidental overwrite of input files ---
  # Helper function to identify if prefix is blank ("", NA, or NULL)
  is_empty_prefix <- function(x) {
    is.null(x) || is.na(x) || x == ""
  }

  # Assume all input files live in the same directory; take the first as reference
  input_dir <- dirname(input_files[1])
  # --- Overwrite safety check ---
  if (!allow_overwrite) {
    if (normalizePath(input_dir) == normalizePath(output_path)) {
      no_input_prefix  <- is_empty_prefix(input_prefix)
      no_output_prefix <- is_empty_prefix(output_prefix)

      if (no_input_prefix && no_output_prefix) {
        stop("Overwrite risk ...")
      }
    }
  }

  # ---- Validate directories ----
  if (!dir.exists(output_path)) {
    message("output_path does not exist, creating: ", output_path)
    dir.create(output_path, recursive = TRUE)
  }

  # ---- Parallel plan ----
  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # ---- Parallel processing ----
  results <- furrr::future_map(
    input_files,
    .f = process_single_file,
    settings = settings,
    .options = furrr::furrr_options(
      packages = c("LotekFilter", "dplyr", "readr")
    )
  )

  summary_df <- dplyr::bind_rows(results)
    message("\nAll files processed.")
    print(summary_df)

    invisible(summary_df)

}
