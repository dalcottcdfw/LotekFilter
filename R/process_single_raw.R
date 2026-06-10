#' Process a single raw Lotek TXT file exported by WHS Host
#'
#' @description
#' Processes a single .TXT file produced by Lotek's WHS Host batch conversion
#' tool. These files are converted from encrypted .JST receiver files into
#' human-readable text. This function reformats the content, converts tag IDs,
#' optionally filters to a set of allowable tag codes, and writes a cleaned CSV
#' to disk. Because these files may be large, this function is intended to be
#' run independently for each input file (and can be parallelized).
#'
#' @param raw_file Character string giving the name of the input TXT file.
#' @param input_path Path to the folder containing \code{raw_file}.
#' @param output_path Path where the processed CSV file should be saved.
#' @param output_prefix Optional prefix added to the output file name.
#' @param allowable_tagcodes Character vector of hexadecimal tag codes to retain,
#'   or \code{NULL} to keep all tag detections.
#' @param tz Character string giving the timezone of the receiver clock.
#'
#' @return Writes a CSV file to \code{output_path}. Returns \code{NULL} (invisible).
#'
#' @importFrom readr read_lines read_fwf fwf_positions
#' @importFrom dplyr mutate filter
#' @importFrom broman dec2hex
#'
#' @examples
#' \dontrun{
#' raw_file <- "Example_Lotek_Raw.TXT"
#' process_single_raw(
#'   raw_file = raw_file,
#'   input_path = system.file("extdata", package = "LotekFilter"),
#'   output_path = tempdir(),
#'   output_prefix = "Processed_",
#'   allowable_tagcodes = c("1A2C"),
#'   tz = "Etc/GMT+8"
#' )
#' }
#'
#' @export
process_single_raw <- function(raw_file,
                               input_path,
                               output_path,
                               output_prefix,
                               allowable_tagcodes,
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
    col_positions = readr::fwf_positions(
      start = c(1, 11, 24, 39, 54, 63, 74),
      end   = c(9, 20, 32, 45, 58, 70, 79),
      col_names = c("Date","Time","SubSec","DecID","TagType","Sensor","SignalStr")
    )
  )

  # Convert DecID to Hex before filtering on HexID
  raw_dets <- raw_dets |>
    dplyr::mutate(HexID = broman::dec2hex(DecID))

  # Filter to keep only allowable tag codes (if provided, otherwise keep all records)
  if (!is.null(allowable_tagcodes)) {
    raw_dets <- raw_dets |>
      dplyr::filter(HexID %in% allowable_tagcodes)
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
