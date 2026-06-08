#' Process multiple raw Lotek TXT files in parallel
#'
#' @description
#' Reads and processes a directory of raw Lotek TXT files exported by the WHS
#' Host batch conversion tool. Each file is processed using
#' `process_single_raw()`, and the work is parallelized across multiple CPU
#' cores so that multiple raw files can be handled simultaneously.
#'
#' @param input_path Path to directory containing raw `.TXT` files.
#' @param raw_files Character vector of TXT filenames to process. Defaults to
#'   all `.TXT` files in the directory.
#' @param output_path Directory where processed CSV files will be written.
#' @param output_prefix Optional prefix to add to output filenames.
#' @param AllowableTagCodes Optional vector of tag IDs to allow and remove all
#' others. This is highly recommended whenever possible. It greatly increases
#' processing speed and decreases file size and resource requirements.
#' @param tz Character; timezone for timestamp parsing (default `"Etc/GMT+8"`).
#' @param n_cores Number of CPU cores to use for parallel processing.
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

  # Normalize file paths (good practice on Windows + OneDrive)
  input_dir_norm  <- normalizePath(input_path, mustWork = FALSE)
  output_dir_norm <- normalizePath(output_path, mustWork = FALSE)

  # Ensure output directory exists
  if (!dir.exists(output_dir_norm)) {
    message("Creating output directory: ", output_dir_norm)
    dir.create(output_dir_norm, recursive = TRUE)
  }

  # Start cluster
  cl <- parallel::makeCluster(n_cores)

  # Export needed variables and functions to each worker
  parallel::clusterExport(
    cl,
    varlist = c(
      "input_path",
      "output_path",
      "output_prefix",
      "AllowableTagCodes",
      "raw_files",
      "tz",
      "process_single_raw"
    ),
    envir = environment()
  )

  # Load required packages in each worker
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(readr)
    library(broman)
  })

  # Process raw files in parallel with a progress bar

  pbapply::pblapply(
    X = raw_files,
    FUN = function(f) {
      process_single_raw(
        raw_file = f,
        input_path = input_path,
        output_path = output_path,
        output_prefix = output_prefix,
        AllowableTagCodes = AllowableTagCodes,
        tz = tz
      )
    },
    cl = cl
  )


  # Shut down cluster
  parallel::stopCluster(cl)

  invisible(TRUE)
}
