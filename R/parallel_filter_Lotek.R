#' filter multiple csv files that were pre-processed by parallel_raw_Lotek() or
#' process_single_raw() in parallel. Or manually reformat data files to match
#' required format (columns: DateTime, HexID, nominalPRI)
#' This is the main executable function that the user will deploy to filter data.
#' This function calls multiple helper functions that contain the actual filter
#' criteria. process_single_file() performs all of the processes necessary on
#' on a csv file that has been properly preformatted. parallel_filter_Lotek()
#' simply parallelizes the work that process_single_file() does.


parallel_filter_Lotek <- function(input_files,
                                    output_dir,
                                    input_file_prefix = NA, # if the input files have a prefix that you want to remove when saving the output ("Raw_..." or "Prefiltered_...")
                                    output_file_prefix = "Filtered_",
                                    # datetime_col, # name of the datetime column
                                    # tagid_col, # name of the tag ID column
                                    # pri_col, # name of the nominal tag PRI column
                                    keep_rejected = FALSE, # TRUE if you want to save a copy of the rejected records with a reason for rejecting
                                    n_cores    = max(1, round(parallel::detectCores()/2)) # number of cores to use for parallel processing (1 input file per core)
) {

  # ---- Validate directories ----
  if (!dir.exists(output_dir)) {
    message("output_dir does not exist, creating: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }

  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)  # always restore on exit

  # ---- Process files in parallel ----
  results <- furrr::future_map(
    input_files,
    .f = process_single_file,      # function for processing a single file to be parallelized
    output_dir   = output_dir,
    input_file_prefix = input_file_prefix,
    output_file_prefix = output_file_prefix,
    datetime_col = datetime_col,
    tagid_col    = tagid_col,
    pri_col      = pri_col,
    keep_rejected = keep_rejected,
    .options = furrr::furrr_options(seed = TRUE),  # required for parallel RNG safety
    .progress = TRUE                                # progress bar
  )

  # ---- Summarise results ----
  summary_df <- dplyr::bind_rows(results)
  message("\nAll files processed.")
  print(summary_df)

  invisible(summary_df)
}
