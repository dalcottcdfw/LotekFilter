#' Execute the PRI filter
#'
#' @description
#' #' This function filters the detection records based on the nominalPRI of the tag.
#' It searches for min_dets detections within a time window of nominalPRI * detection_window seconds.
#' The function considers all combinations of min_dets that may occur within the
#' time window to see if any of them pass the filter criteria. This is done
#' to avoid a false-positive inside a set of true-positive detections from
#' causing a violation (such as PRI standard deviation)
#'
#' @param dets_df a dataframe containing JSATS detections.
#' @param settings a list of filter settings inherited from parallel_filter()
#'
#' @return a list containing detections that passed the pri filter and optionally a list of detections that did not
#'
#' @importFrom dplyr mutate select left_join
#' @importFrom rlang sym
pri_filter <- function(dets_df,
                       settings = settings
) {

  nominalPRI <- settings$nominalPRI
  pri_table <- settings$pri_table
  pri_tag_col <- settings$pri_tag_col
  pri_value_col <- settings$pri_value_col

  detection_window <- settings$detection_window
  min_detections <- settings$min_detections
  sd_threshold <- settings$sd_threshold
  nominalPRI_threshold <- settings$nominalPRI_threshold

  # first, identify the tag pulse rate interval:
  if (is.numeric(nominalPRI)) {

    # Case 1: fixed PRI for all tags
    dets_df <- dets_df |>
      dplyr::mutate(nominalPRI = nominalPRI) # add the provided nominalPRI value to all records

  } else if (!is.null(pri_table)) {
    # Case 2: lookup tag table supplied
    if (is.null(pri_tag_col) || is.null(pri_value_col)) {
      stop("If PRI pri_table is provided, both pri_tag_col and pri_value_col must also be specified.")
    }

    # Ensure required columns exist
    if (!pri_tag_col %in% names(pri_table)) {
      stop(paste("Column", pri_tag_col, "not found in pri_table."))
    }
    if (!pri_value_col %in% names(pri_table)) {
      stop(paste("Column", pri_value_col, "not found in pri_table."))
    }

    # Select only needed columns from the pri_table
    pri_table <- pri_table %>%
      dplyr::select(
        HexID = !!rlang::sym(pri_tag_col),
        nominalPRI = !!rlang::sym(pri_value_col)
      )


    # Join by HexID
    dets_df <- dets_df |>
      dplyr::left_join(pri_table, by = "HexID")

  } else {

    stop("You must supply either a numeric nominalPRI or a PRITable with pri_tag_col and pri_value_col")

  }

  # Extract PRI as a single value
  tag_pri <- unique(dets_df$nominalPRI) # list of unique values for PRI for this tag

  # Warning:
  if (length(tag_pri) != 1 || is.na(tag_pri)) {
    stop("Error in pri_filter(): nominalPRI must be a single value per tag.")
  }

  nominalPRI <- tag_pri # assign a single value for PRI for next steps

  times <- as.numeric(dets_df$DateTime)
  n     <- length(times)
  max_span <- detection_window * nominalPRI

  valid_rows <- logical(n)  # tracks which rows appear in at least one valid quad

  for (i in seq_len(n - (min_detections - 1))) {

    # Binary search: find all detections within max_span of anchor i
    window_end <- findInterval(times[i] + max_span, times)

    # Need at least 3 more candidates beyond anchor
    if (window_end < i + (min_detections - 1)) next

    candidates <- i:window_end

    # All possible combinations of 4 dets from i to window_end
    combos <- combn(candidates, min_detections)
    combos <- combos[, combos[1, ] == i, drop = FALSE]

    if (ncol(combos) == 0) next

    for (col in seq_len(ncol(combos))) {
      idx       <- combos[, col]
      intervals <- diff(times[idx])

      # Normalize each interval to nearest nominalPRI multiple
      multiples <- round(intervals / nominalPRI)

      # Skip if any interval is too short to normalize (would imply sub-nominalPRI gap
      # that survived multipath filter — shouldn't happen but guard anyway)
      if (any(multiples == 0)) next

      tdiffs <- intervals / multiples

      # Criterion 1: each normalized interval within 20% of nominalPRI
      if (any(abs(tdiffs - nominalPRI) >= nominalPRI * nominalPRI_threshold)) next

      # Criterion 2: SD of normalized intervals < 0.025
      if (sd(tdiffs) >= sd_threshold) next

      # This set is valid — mark all rows as keepers
      valid_rows[idx] <- TRUE
    }
  }

  clean    <- dets_df[valid_rows, ]
  rejected <- dets_df[!valid_rows, ]

  if (nrow(rejected) > 0) {
    rejected$rejection_reason <- "failed_false_positive_filter"
  }

  list(clean = clean, rejected = rejected)

}
