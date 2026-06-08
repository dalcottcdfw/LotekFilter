#' process a single raw Lotek .TXT file exported by Lotek's WHS Host
#' batch file conversion tool.
#' This function takes .TXT files created by WHS Host's batch file conversion tool
#' which conversion .JST files from the receiver to unecrypted, usable text files.
#' This function reformats the TXT files output why WHS host and optionally
#' filters to only keep detections from a list of allowabe tag codes.
#' This is recommended as it will greatly reduce file size and thus processing.
#' This function and package are designed to be run each input file independently
#' in parallel due to the potential large file sizes which can cause memory limitations
#' if multiple files are retained in a single R environment.

# ---- Helper function to process raw text files ----

process_single_raw <- function(raw_file,
                               input_path,
                               output_path,
                               output_prefix,
                               AllowableTagCodes,
                               tz) {


  # Read the raw text first to identify tag sections
  raw_text <- readr::read_lines(file.path(input_path, raw_file))

  tag_start <- grep("Tag Records:", raw_text) + 2
  tag_nmax  <- grep("Receiver Setup Messages:", raw_text) - tag_start
  if (length(tag_nmax) == 0) tag_nmax <- length(raw_text)

  # Read fixed width
  raw_dets <- readr::read_fwf(
    file.path(input_path, raw_file),
    skip      = tag_start,
    n_max     = tag_nmax,
    col_positions = fwf_positions(
      start = c(1, 11, 24, 39, 54, 63, 74),
      end   = c(9, 20, 32, 45, 58, 70, 79),
      col_names = c("Date","Time","SubSec","DecID","TagType","Sensor","SignalStr")
    )
  )

  # Convert DecID to Hex before filtering on HexID
  raw_dets <- raw_dets |>
    dplyr::mutate(HexID = broman::dec2hex(DecID))

  # Filter to keep only allowable tag codes (if provided, otherwise keep all records)
  if (!is.null(AllowableTagCodes)) {
    raw_dets <- raw_dets |>
      dplyr::filter(HexID %in% AllowableTagCodes)
  }

  # Add formatted date fields
  raw_dets <- raw_dets |>
    dplyr::mutate(Date = paste0(substr(Date, 1, 6), "20", substr(Date, 7, 8)),
                  DateTime  = as.POSIXct(paste(Date, Time),
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz = tz),
                  SubSec    = as.numeric(SubSec),
                  DateTime  = DateTime + SubSec
                  )

  # Save output files
  output_file <- file.path(output_path,
                       paste0(output_prefix, gsub("\\.TXT$", ".csv", raw_file))
  )

  write.csv(raw_dets, output_file, row.names = FALSE)
}
