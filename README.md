
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LotekFilter

<!-- badges: start -->

<!-- badges: end -->

The goal of the LotekFilter R package is to process and filter JSATS
acoustic telemetry detection files from Lotek JSATS receivers. Lotek
JSATS receivers record .JST files, which must be converted to .TXT files
using Lotek’s WHS Host software’s ‘Batch data to text conversion’ tool.
The LotekFilter package is designed to accept .TXT files exported from
WHS and reformat them to a computationally friendy format. The
processing step extracts detection records from the mixed-data files and
saves an intermediate csv file. The LotekFilter package can then apply
standard or adjustable false-positive filtering criteria to remove
false-positive detections and export a filtered and formatted csv file
for each original receiver file. Each of these operations in LotekFilter
are designed to be executed in parallel using multiple CPU cores to
improve computational speed and efficiency. LotekFilter is set up to
process files in a directory and create intermediate files one at a
time, rather than read all files into the R environment. This is done
because raw Lotek JSATS input files are usually very large and most
machines will not have sufficient memory to process an entire collection
of receiver input files. The final processed and filtered files are
orders of magnitude smaller than the raw input files and can be compiled
into one final data frame in the R environment.

## Installation

You can install the development version of LotekFilter from
[GitHub](https://github.com/) using
devtools::install_github("dalcottcdfw/LotekFilter"), or:

``` r
# install.packages("pak")
pak::pak("dalcottcdfw/LotekFilter")
```

## Example

Brief example usage of LotekFilter’s two primary functions. See the
vignette for more detailed examples.

``` r
library(LotekFilter)
## Step 1 - Process raw text files:
tags <- read.csv("my_allowable_tag_codes.csv") # contains a column of allowable tag codes
output_dir = "C:/path/to/store/processed/outputs"

parallel_raw_Lotek(input_path = "C:/path/to/input/files",
                   raw_files = list.files("C:/path/to/input/files", pattern = "\\.TXT$"), 
                   output_path = output_dir,
                   output_prefix = "Processed_", # optional prefix to add to processed files
                   allowable_tagcodes = tags$TagID_Hex, # optional but recommended: pre-filter on set of allowable tag codes
                   tz = "Etc/GMT+8", # time zone that detections were recorded in
                   n_cores = parallel::detectCores()-2) # number of computer cores to use (always reserve at least 1 unused core)

## Step 2 - Filter false-positives:
tags <- read.csv("my_tag_PRIs.csv") # contains a column of hexidecimal format tag codes and a column of the tag's pulse rate interval

results_summary <- parallel_filter(
  input_files = list.files(pattern = ".csv"),
  input_prefix = "Processed_", # if the input files have a prefix that you want to remove when saving the output ("Processed_..." , "raw_...")
  output_prefix = "Filtered_",
  output_path = output_dir,
  keep_rejected = FALSE, # TRUE if you want to save a copy of the rejected records with a reason for rejecting
  n_cores      = parallel::detectCores()-2, # number of cores to use for parallel processing (1 input file per core, do not assign all cores)
  
  pri_table = tags,            # provide name of tag pri lookup table AND
  pri_tag_col = "TagID_Hex",          # provide name of hex tag code column in tag lookup table AND
  pri_value_col = "PRI_nominal",        # provide name of column containing tag pulse rate interval in tags lookup table
)
results_summary # view summary table of file processing


# Step 3 - combine all filtered files into one dataframe object in R environment
filtered_files <- list.files(output_dir, pattern = "Filtered_*")

# Read each filtered csv file and add a filename column
df_list <- lapply(filtered_files, function(file) {
  df <- read.csv(file)
  df$Filename <- basename(file) # add source file name for identifying receiver
  df
})

# Combine into one data frame
df <- do.call(rbind, df_list)
# df is a final dataframe object with all filtered detections
```
