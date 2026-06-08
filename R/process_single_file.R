#' This function reads, formats, and calls necessary filter functions to fully
#' process a single csv file that was exported by parallel_raw_Lotek() or
#' process_single_raw(). This is the function that parallel_filter_Lotek()
#' parallelizes to increase processing speed. This function calls multiple
#' helper functions which actually execute the filter criteria.

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
