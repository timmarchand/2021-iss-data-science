# Load necessary packages
pacman::p_load(tidyverse, # as usual
               haven # for reading dta files)

# Set the path to your folder containing .dta files
folder_path <- "path/to/your/folder"  # <- Replace with your actual folder path

# Read all .dta files into a named list of tibbles
data_list <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_dta)

# To access a single data file from the list, use the name inside the file name  [[]]
data_list[["mydatafile"]]  # Access data from "mydatafile.dta"

# This works if all of the files have the same structure
combined_data <- data_list %>%
  bind_rows(.id = "source_file")  # Adds a column to track which file each row came from


# Read and combine .dta files of the same strucutre -----
# This works if all of the files have the same structure
combined_data <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_dta) %>%
  bind_rows(.id = "source_file")


# Read files while handling problematic data ----

# Create a safe version of read_dta
safe_read_dta <- possibly(read_dta, otherwise = NULL)

# Read files with error handling
dta_files <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE)

# Read all files into a list, skipping ones that fail
data_list <- dta_files %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(safe_read_dta)

# Filter out NULLs (failed reads)
data_list <- compact(data_list)  # Removes NULL elements

# Combine into one tibble
combined_data <- data_list %>%
  bind_rows(.id = "source_file")

## Read files and keeping log of problematic data ----

safe_read_dta <- safely(read_dta)

results <- map(dta_files, safe_read_dta)

# Separate results into successes and failures
successes <- results %>%
  keep(~ is.null(.x$error)) %>%
  map("result") %>%
  set_names(tools::file_path_sans_ext(basename(dta_files[which(map_lgl(results, ~ is.null(.x$error)))])))

failures <- results %>%
  keep(~ !is.null(.x$error))

# Combine only successful ones
combined_data <- successes %>%
  bind_rows(.id = "source_file")

# Optional: Print failed files
failed_files <- dta_files[which(map_lgl(results, ~ !is.null(.x$error)))]
if (length(failed_files) > 0) {
  message("These files could not be read:")
  print(failed_files)
}

