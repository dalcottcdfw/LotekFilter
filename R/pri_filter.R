#' This function filters the detection records based on the nominalPRI of the tag.
#' It searches for min_dets detections within a time window of nominalPRI * detection_window seconds.
#' The function considers all combinations of min_dets that may occur within the
#' time window to see if any of them pass the filter criteria. This is done
#' to avoid a false-positive inside a set of true-positive detections from
#' causing a violation (such as PRI standard deviation)

pri_filter <- function(dets_df,
                       nominalPRI = NULL,           # optionally, provide a single numeric tag pulse rate interval to all tags OR:
                       pri_table = NULL,            # provide name of tag pri lookup table AND
                       pri_tag_col = NULL,          # provide name of hex tag code column in tag lookup table AND
                       pri_value_col = NULL,        # provide name of column containing tag pulse rate interval in tags lookup table

                       detection_window = 16.6,    # detection_window * nominmal PRI = time window that min_detections must occur to be kept. Arnold Ammann criteria = 16.6 for Lotek
                       min_detections = 4,         # number of detections required within the detection window to be kept. Arnold Ammann criteria = 4 for Lotek.
                       sd_threshold = 0.025,           # max threshold for standard deviation of observed PRI. Arnold Ammann criteria = 0.025
                       multipath_threshold = 0.3,  # time threshold for multipath detections. Detections less than this seconds after first are removed. Arnold Ammann criteria = 0.3 sec
                       nominalPRI_threshold = 0.2, # observed PRI must be within this amount of nominal PRI. Arnold Ammann criteria = 20% or 0.20.


) {

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
    pri_table <- pri_table |>
      dplyr::select(
        HexID = !!rlang::sym(pri_tag_col), # rename to HexID
        nominalPRI = !!rlang::sym(PRI_value_col) # rename to nominalPRI
      )

    # Join by HexID
    dets_df <- dets_df |>
      dplyr::left_join(pri_table, by = "HexID")

  } else {

    stop("You must supply either a numeric nominalPRI or a PRITable with pri_tag_col and pri_value_col")

  }


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
