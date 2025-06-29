# required libraries
library(readr)
library(stringr)


# link should be the "url-of-the-google-sheet-you-want-importing"
read_gdoc_csv <- function(link) {
  sheet_id <- stringr::str_extract(link, "(?<=d/)[^/]+")
  tab_id <- stringr::str_extract(link, "(?<=gid=)[0-9]+")
  csv <- stringr::str_glue("https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv&gid={tab_id}")
  
  tryCatch({
    suppressWarnings({
      readr::read_csv(csv)
    })
  }, error = function(e) {
    if (grepl("401", e$message) || grepl("403", e$message) || grepl("permission", e$message, ignore.case = TRUE)) {
      message("\nERROR: Unable to access the Google Sheet (HTTP error 401/403)")
      message("This typically means the sheet isn't publicly accessible.")
      
      message("\nTO FIX THIS:")
      message("If you OWN this sheet:")
      message("1. Open the Google Sheet in your browser")
      message("2. Click 'Share' in the top-right corner")
      message("3. Click 'Change' next to 'Restricted'")
      message("4. Select 'Anyone with the link'")
      message("5. Set permission to 'Viewer' (not 'Editor')")
      message("6. Click 'Done'")
      
      message("\nIf you DO NOT OWN this sheet:")
      message("You'll need to ask the owner to complete the steps above to make it public.")
      message("Politely request they share it with 'Anyone with the link' as a 'Viewer'.")
      
      message("\nIMPORTANT NOTE: The URL should look like this when shared correctly:")
      message(paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit?gid=", tab_id))
      
      message("\nAfter permissions are updated, wait a minute and try again.")
    } else {
      message("Error: ", e$message)
    }
    return(invisible(NULL))
  })
}