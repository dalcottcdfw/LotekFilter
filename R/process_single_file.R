#' This function reads, formats, and calls necessary filter functions to fully
#' process a single csv file that was exported by parallel_raw_Lotek() or 
#' process_single_raw(). This is the function that parallel_filter_Lotek() 
#' parallelizes to increase processing speed. This function calls multiple
#' helper functions which actually execute the filter criteria.

process_single_file <- function(filepath,
                                output_path,
                                input_file_prefix = NULL,
                                output_prefix = "",
                               
                                HexID,
                                nominalPRI,
                                keep_rejected) {
  
  filename  <- basename(filepath)
  new_name  <- paste0(output_prefix, filename)
  
  out_path  <- file.path(output_path, new_name)
  
  # Wrap in tryCatch so one bad file doesn't abort the whole run
  tryCatch({
    
    dat <- readr::read_csv(filepath, show_col_types = FALSE)
    
    result <- all_filter_steps(dat, keep_rejected = keep_rejected)
    
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
      n_input      = nrow(dat),
      n_clean      = nrow(clean_df),
      n_rejected   = nrow(dat) - nrow(clean_df),
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