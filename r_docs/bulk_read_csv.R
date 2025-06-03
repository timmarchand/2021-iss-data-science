# Load necessary packages
pacman::p_load(tidyverse)  # readr is part of tidyverse

# Set the path to your folder containing .csv files
folder_path <- "path/to/your/folder"  # <- Replace with your actual folder path

# Read all .csv files into a named list of tibbles
data_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_csv)

# To access a single data file from the list, use the name inside the file name [[]]
data_list[["mydatafile"]]  # Access data from "mydatafile.csv"

# Read and combine .csv files of the same structure -----
combined_data <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_csv) %>%
  bind_rows(.id = "source_file")

# Read files while handling problematic data ----

# Create a safe version of read_csv
safe_read_csv <- possibly(read_csv, otherwise = NULL)

# Read files with error handling
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all files into a list, skipping ones that fail
data_list <- csv_files %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(safe_read_csv)

# Filter out NULLs (failed reads)
data_list <- compact(data_list)  # Removes NULL elements

# Combine into one tibble
combined_data <- data_list %>%
  bind_rows(.id = "source_file")

# Read files and keep log of problematic data ----

safe_read_csv <- safely(read_csv)

results <- map(csv_files, safe_read_csv)

# Separate results into successes and failures
successes <- results %>%
  keep(~ is.null(.x$error)) %>%
  map("result") %>%
  set_names(tools::file_path_sans_ext(basename(csv_files[which(map_lgl(results, ~ is.null(.x$error)))])))

failures <- results %>%
  keep(~ !is.null(.x$error))

# Combine only successful ones
combined_data <- successes %>%
  bind_rows(.id = "source_file")

# Optional: Print failed files
failed_files <- csv_files[which(map_lgl(results, ~ !is.null(.x$error)))]
if (length(failed_files) > 0) {
  message("These CSV files could not be read:")
  print(failed_files)
}
