#' filter multiple csv files that were pre-processed by parallel_raw_Lotek() or
#' process_single_raw() in parallel. Or manually reformat data files to match
#' required format (columns: DateTime, HexID, nominalPRI)
#' This is the main executable function that the user will deploy to filter data.
#' This function calls multiple helper functions that contain the actual filter
#' criteria. process_single_file() performs all of the processes necessary on
#' on a csv file that has been properly preformatted. parallel_filter_Lotek()
#' simply parallelizes the work that process_single_file() does.


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

  if (!allow_overwrite) {
    if (normalizePath(input_dir) == normalizePath(output_path)) {

      no_input_prefix  <- is_empty_prefix(input_prefix)
      no_output_prefix <- is_empty_prefix(output_prefix)

      if (no_input_prefix && no_output_prefix) {
        stop(
          "The output_path matches the input directory and no input_prefix or output_prefix was provided.\n",
          "This would overwrite the original input files.\n\n",
          "To avoid overwriting: provide an output_prefix or change output_path.\n",
          "To continue overwriting input files: change allow_overwrite to TRUE."
        )
      }
    }


  # ---- Validate directories ----
  if (!dir.exists(output_path)) {
    message("output_path does not exist, creating: ", output_path)
    dir.create(output_path, recursive = TRUE)
  }

  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)  # always restore on exit

  # ---- Process files in parallel ----
  results <- future_map(
    input_files,
    .f = process_single_file,         # function to process a single file that is being parallelized
    settings = settings,              # pass all filter settings/arguments
    keep_rejected = keep_rejected
  )

  # ---- Summarise results ----
  summary_df <- dplyr::bind_rows(results)
  message("\nAll files processed.")
  print(summary_df)

  invisible(summary_df)
}
