#' parallel_raw_Lotek()
#' Function to process a directory of raw Lotek TXT files exported by WHS Host
#' batch file conversion tool. This function uses process_single_raw() as the primary
#' worker function but parallelizes it to use multiple cores to process multiple
#' input TXt files simulatneously.
#' NOTE: If input_path and output_path are the same and an output_prefix is not provided,
#' then the input files will be overwritten.

process_raw_Lotek_dets <- function(
    input_path,
    raw_files = list.files(input_path, pattern = "\\.TXT$"),
    output_path,
    output_prefix = "",
    AllowableTagCodes = NULL,
    tz = "Etc/GMT+8",
    n_cores = max(1, round(parallel::detectCores() / 2))
) {

  # Normalize file paths to avoid false mismatches (e.g., trailing slashes)
  input_dir_norm  <- normalizePath(input_path, mustWork = FALSE)
  output_dir_norm <- normalizePath(output_path, mustWork = FALSE)


  # File overwrite Protection:
    # if input and output directories match and no filename prefix files, then
    # input files will be overwritten.
    if (input_dir_norm == output_dir_norm && output_prefix == "") {

      message("WARNING: Your output directory is the same as your input directory, ",
              "and you did not provide an output filename prefix.")
      message("This will OVERWRITE your original input files.")

      # Prompt user for confirmation
      response <- readline(
        prompt = "Type 'YES' to continue and overwrite, or anything else to cancel: "
      )

      if (toupper(response) != "YES") {
        stop("Operation cancelled by user to prevent overwriting input data.")
      }

      message("Proceeding with overwrite as confirmed.")
    }

  # Execute function in parallel
  cl <- parallel::makeCluster(n_cores)

  # Export all needed variables to cluster workers
  # (each worker is a new environment and cannot see your current working environment)
  parallel::clusterExport(
    cl,
    varlist = c(
      "input_path",
      "output_path",
      "output_prefix",
      "AllowableTagCodes",
      "raw_files",
      "process_single_raw"
    ),
    envir = environment()
  )

  # Load required packages on each worker
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(readr)
    library(broman)
  })

  # Run in parallel (with progress bar)
  pbapply::pblapply(
    X = raw_files,
    FUN = process_single_raw,
    cl = cl
  )

  # Close cluster
  parallel::stopCluster(cl)
  invisible(TRUE)
}



# Example usage:
# TestInput = "C:\\Users\\DAlcott\\OneDrive - California Department of Fish and Wildlife\\Documents\\R Code Examples\\Practice\\FilterLotekFunctions\\RawLotek"
# TestOutput = "C:\\Users\\DAlcott\\OneDrive - California Department of Fish and Wildlife\\Documents\\R Code Examples\\Practice\\FilterLotekFunctions\\TestOutput"
#
#
# process_raw_Lotek_dets(input_path = TestInput,
#                        output_path = TestOutput,
#                        output_prefix = "Unfiltered",
#                        n_cores = parallel::detectCores()-2)
