#' 
#### LEFT OFF HERE
# need to fix names and convert to stand alone function
# write description above
# next is doing the same for the pri filter <- what function calls this?
# then will need to carefully check consistency in argument names
# will they inherit properly?

### Broad filter function
# uses pri_filter()
all_filter_steps <- function(Lotek_input_file = input,
                                    datetime_col  = datetime_col,
                                    tagid_col     = tagid_col,
                                    pri_col       = pri_col,
                                    keep_rejected = TRUE) {
  
  # ---- Validate and capture column name arguments ----
  dt_col  <- rlang::ensym(datetime_col)   # DateTime column
  tag_col <- rlang::ensym(tagid_col)      # TagID column
  pri_col_sym <- rlang::ensym(pri_col)
  
  
  # ---- Rename key columns internally for clean code below ----
  # This avoids scattering !! injection throughout every dplyr call.
  # We rename to fixed internal names, process, then restore at the end.
  Lotek_input_file <- dplyr::rename(Lotek_input_file,
                                    .datetime = !!dt_col,
                                    .tagid    = !!tag_col,
                                    .pri = !!pri_col_sym)
  
  # ---- Step 1: Sort and compute time differences ----
  Lotek_input_file <- dplyr::arrange(Lotek_input_file, .tagid, .datetime)
  Lotek_input_file <- dplyr::group_by(Lotek_input_file, .tagid)
  Lotek_input_file <- dplyr::mutate(Lotek_input_file,
                                    td = as.numeric(difftime(.datetime,
                                                             dplyr::lag(.datetime),
                                                             units = "secs")))
  Lotek_input_file <- dplyr::ungroup(Lotek_input_file)
  
  # ---- Step 2: Remove multipath (td < 0.3s) ----
  multipath_records <- dplyr::filter(Lotek_input_file, !is.na(td) & td < 0.3)
  multipath_records$rejection_reason <- "multipath"
  
  working <- dplyr::filter(Lotek_input_file, is.na(td) | td >= 0.3)
  
  # ---- Step 3: Remove tags with fewer than 4 detections ----
  det_counts <- dplyr::summarise(
    dplyr::group_by(working, .tagid),
    det_count = dplyr::n()
  )
  working <- dplyr::left_join(working, det_counts, by = ".tagid")
  
  min_det_records <- dplyr::filter(working, det_count < 4)
  min_det_records$rejection_reason <- "min_detections"
  min_det_records <- dplyr::select(min_det_records, -det_count)
  
  working <- dplyr::filter(working, det_count >= 4)
  working <- dplyr::select(working, -det_count)
  
  # ---- Step 4: Distance matrix quad filter, per tag ----
  tags <- unique(working$.tagid)
  
  clean_list    <- vector("list", length(tags))
  rejected_list <- vector("list", length(tags))
  
  for (i in seq_along(tags)) {
    tag_df <- dplyr::filter(working, .tagid == tags[i])
    tag_df <- dplyr::arrange(tag_df, .datetime)
    
    # Each tag may have its own PRInominal if drawn from a column
    tag_pri <- unique(tag_df$.pri)
    if (length(tag_pri) > 1) {
      warning(paste0("Tag ", tags[i], " has multiple PRInominal values. ",
                     "Using the first value: ", tag_pri[1]))
      tag_pri <- tag_pri[1]
    }
    
    result <- pri_filter(tag_df, PRInominal = tag_pri)
    
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
  clean_df    <- dplyr::select(clean_df,    -dplyr::all_of(c("td", ".pri")))
  rejected_df <- dplyr::select(rejected_df, -dplyr::all_of(c("td", ".pri")))
  
  # ---- Restore original column names ----
  clean_df <- dplyr::rename(clean_df,
                            !!dt_col  := .datetime,
                            !!tag_col := .tagid)
  rejected_df <- dplyr::rename(rejected_df,
                               !!dt_col  := .datetime,
                               !!tag_col := .tagid)
  
  if (keep_rejected) {
    return(list(clean = clean_df, rejected = rejected_df))
  } else {
    return(clean_df)
  }
}