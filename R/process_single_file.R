#' Process a single Lotek detection csv file
#'
#' @description
#' Processes a single reformatted Lotek detection csv file produced by
#' [parallel_raw_Lotek()] or [process_single_raw()]. This function applies all
#' filtering steps via [all_filter_steps()], writes the cleaned output file, and
#' returns a one-row summary tibble describing the results. It is primarily
#' intended to support [parallel_filter_Lotek()], but can also be used
#' independently when processing a single file.
#'
#' The input csv must contain (at minimum) the fields expected by
#' [all_filter_steps()], typically including `DateTime`, `HexID`, and any fields
#' used for PRI or multipath filtering.
#'
#' @param filepath Path to the input csv file.
#' @param settings A named list of filter settings created within
#'   [parallel_filter_Lotek()]. Must include `output_path`, `input_prefix`,
#'   `output_prefix`, and all filtering thresholds.
#'
#' @return
#' A one-row tibble summarizing:
#' * input filename
#' * output filename
#' * number of detections before and after filtering
#' * number rejected
#' * status (“success” or “failed”)
#' * any error message
#'
#' The cleaned detection csv is written to the output directory as a side
#' effect.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tibble
#'
#' @examples
#' \dontrun{
#' # Example single-file processing
#' f <- system.file("extdata", "Example_Lotek_Processed.csv",
#'                  package = "LotekFilter")
#'
#' settings <- list(
#'   output_path   = tempdir(),
#'   input_prefix  = "",
#'   output_prefix = "Filtered_",
#'   keep_rejected = FALSE
#' )
#'
#' process_single_file(f, settings = settings)
#' }
#'
#' @export
process_single_file <- function(filepath,
                                settings = settings) {

  output_path <- settings$output_path
  input_prefix <- settings$input_prefix
  output_prefix <- settings$output_prefix
  keep_rejected <- settings$keep_rejected

  filename  <- basename(filepath)

  # Remove input file prefix, if provided
  if (!is.na(input_prefix) && nzchar(input_prefix)) {
    filename <- sub(paste0("^", input_prefix), "", filename)
  }

  new_name  <- paste0(output_prefix, filename)

  out_path  <- file.path(output_path, new_name)

  # Wrap in tryCatch so one bad file doesn't abort the whole run
  tryCatch({

    Lotek_input_file <- readr::read_csv(filepath, show_col_types = FALSE)

    result <- all_filter_steps(Lotek_input_file,
                               settings = settings        # pass settings on
                               )

    # Handle both return types (list if keep_rejected TRUE, df if FALSE)
    if (is.list(result) && !is.data.frame(result)) {
      clean_df <- result$clean
    } else {
      clean_df <- result
    }

    write.csv(clean_df, out_path, row.names = F)

    # Return a one-row summary
    dplyr::tibble(
      input_file   = filename,
      output_file  = new_name,
      n_input      = nrow(Lotek_input_file),
      n_clean      = nrow(clean_df),
      n_rejected   = nrow(Lotek_input_file) - nrow(clean_df),
      status       = "success",
      error        = NA_character_
    )

  }, error = function(e) {
    warning("Failed to process: ", filename, "\n  Error: ", e$message)
    dplyr::tibble(
      input_file   = filename,
      output_file  = new_name,
      n_input      = NA_integer_,
      n_clean      = NA_integer_,
      n_rejected   = NA_integer_,
      status       = "failed",
      error        = e$message
    )
  })
}
