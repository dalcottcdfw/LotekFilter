#' This function performs all of the steps involved in filtering false-positive
#' detections from a Lotek JSATS receiver. The first few simpler filter steps
#' are coded directly in this function (e.g. multipath, minimum number of detections).
#' Then this function calls pri_filter() which performs the complex filter based
#' on the nominalPRI of the tag.
#'

### Broad filter function
# uses pri_filter()
all_filter_steps <- function(Lotek_input_file,
                             settings = settings) {

  multipath_threshold <- settings$multipath_threshold
  min_detections <- settings$min_detections
  keep_rejected <- settings$keep_rejected
  nominalPRI <- settings$nominalPRI
  pri_table <- settings$pri_table
  pri_tag_col <- settings$pri_tag_col
  pri_value_col <- settings$pri_value_col


  # ---- Step 1: Sort and compute time differences ----
  Lotek_input_file <- Lotek_input_file |>
    dplyr::arrange(HexID, DateTime) |>
    dplyr::group_by(HexID) |>
    dplyr::mutate(time_lag = as.numeric(difftime(DateTime, dplyr::lag(DateTime), units = "secs"))) |>
    dplyr::ungroup()

  # ---- Step 2: Remove multipath (time_lag < 0.3s) ----
  multipath_records <- dplyr::filter(Lotek_input_file, !is.na(time_lag) & time_lag < multipath_threshold)
  multipath_records$rejection_reason <- "multipath"

  working <- dplyr::filter(Lotek_input_file, is.na(time_lag) | time_lag >= multipath_threshold)

  # ---- Step 3: Remove tags with fewer than min_detections detections ----
  det_counts <- dplyr::summarise(
    dplyr::group_by(working, HexID),
    det_count = dplyr::n()
  )
  working <- dplyr::left_join(working, det_counts, by = "HexID")

  min_det_records <- dplyr::filter(working, det_count < min_detections)
  min_det_records$rejection_reason <- "min_detections"
  min_det_records <- dplyr::select(min_det_records, -det_count)

  working <- dplyr::filter(working, det_count >= min_detections)
  working <- dplyr::select(working, -det_count)

  # ---- Step 4: Distance matrix filter of min_detections within pri_threshold ----
  detected_tags <- unique(working$HexID) # each detected tag code
  clean_list    <- vector("list", length(detected_tags))
  rejected_list <- vector("list", length(detected_tags))

  for (i in seq_along(detected_tags)) {
    tag_df <- dplyr::filter(working, HexID == detected_tags[i]) # one tag code at a time
    tag_df <- dplyr::arrange(tag_df, HexID, DateTime)

    result <- pri_filter(tag_df, settings = settings)

    clean_list[[i]]    <- result$clean
    rejected_list[[i]] <- result$rejected
  }

  clean_df <- dplyr::bind_rows(clean_list)
  rejected_df <- dplyr::bind_rows(
    multipath_records,
    min_det_records,
    dplyr::bind_rows(rejected_list)
  )

  # ---- Tidy up helper columns ----
  clean_df    <- dplyr::select(clean_df,    -dplyr::all_of(c("time_lag")))
  rejected_df <- dplyr::select(rejected_df, -dplyr::all_of(c("time_lag")))


  if (keep_rejected) {
    return(list(clean = clean_df, rejected = rejected_df))
  } else {
    return(clean_df)
  }
}
