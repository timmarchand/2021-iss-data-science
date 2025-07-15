## function code
read_gdoc_csv <- function(link, encoding = NULL) {
  # Validate input
  if (!grepl("^https://docs\\.google\.com/spreadsheets/d/", link)) {
    message("\nERROR: Invalid Google Sheets URL format")
    message("✖ Problem: The link doesn't look like a proper Google Sheets URL")
    message("✓ Solution: COPY DIRECTLY FROM YOUR BROWSER'S ADDRESS BAR when viewing the sheet")
    message("   DO NOT use the 'Share' → 'Copy link' button as this often gives the wrong format")
    message("\nRequired format: https://docs.google.com/spreadsheets/d/SHEET_ID/edit#gid=TAB_ID")
    return(invisible(NULL))
  }
  
  sheet_id <- stringr::str_extract(link, "(?<=d/)[^/]+")
  tab_id <- stringr::str_extract(link, "(?<=gid=)[0-9]+")
  
  # Handle missing tab_id with specific guidance
  if (is.na(tab_id)) {
    message("\nERROR: Missing sheet tab identifier (gid)")
    message("✖ Problem: The URL doesn't specify which worksheet tab to use")
    message("✓ Solution:")
    message("   1. Open the sheet in your browser")
    message("   2. CLICK ON THE SPECIFIC TAB you want to use")
    message("   3. COPY THE URL DIRECTLY FROM THE ADDRESS BAR (not Share → Copy link)")
    message("   4. The correct URL will contain '#gid=NUMBER' at the end")
    return(invisible(NULL))
  }
  
  if (is.na(sheet_id)) {
    message("\nERROR: Could not extract Sheet ID")
    message("✖ Problem: The URL format is unexpected")
    message("✓ Solution: Copy DIRECTLY from your browser address bar when viewing the sheet")
    message("   DO NOT use the 'Share' button's copied link")
    return(invisible(NULL))
  }
  
  csv <- stringr::str_glue("https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv&gid={tab_id}")
  
  tryCatch({
    if (!is.null(encoding)) {
      suppressWarnings({
        readr::read_csv(csv, locale = readr::locale(encoding = encoding))
      })
    } else {
      suppressWarnings({
        readr::read_csv(csv)
      })
    }
  }, error = function(e) {
    if (grepl("400", e$message)) {
      message("\nERROR: Bad Request (HTTP 400)")
      message("✖ Problem: The sheet might not exist or URL is malformed")
      message("✓ Solution:")
      message("   1. Verify the sheet exists and you have permission")
      message("   2. COPY THE URL DIRECTLY FROM YOUR BROWSER (not via Share button)")
      message("   3. Ensure it looks like: https://docs.google.com/.../edit#gid=NUMBER")
    } else if (grepl("401|403|permission", e$message, ignore.case = TRUE)) {
      message("\nERROR: Permission Denied (HTTP 401/403)")
      message("✖ Problem: The sheet isn't publicly accessible")
      message("✓ Solution:")
      message("   1. Open the sheet in your browser")
      message("   2. Click 'Share' → 'Change' → 'Anyone with the link'")
      message("   3. IMPORTANT: After changing permissions,")
      message("      COPY THE NEW URL DIRECTLY FROM THE ADDRESS BAR")
      message("      (Don't use the old link or Share button's copy)")
    } else if (!is.null(encoding) && grepl("invalid multibyte string", e$message)) {
      message("\nERROR: Encoding issue with specified encoding '", encoding, "'")
      message("✖ Problem: The specified encoding doesn't match the file's actual encoding")
      message("✓ Solution: Try different encodings. Common options include:")
      message("   - 'UTF-8' (default for most modern sheets)")
      message("   - 'CP932' (Japanese Shift-JIS)")
      message("   - 'ISO-8859-1' (Western European)")
      message("   - 'GB18030' (Simplified Chinese)")
    } else {
      message("\nERROR: ", e$message)
      message("✓ General solution: Try copying the URL DIRECTLY from your browser address bar")
      message("   The 'Share' button often gives links that don't work for data export")
    }
    return(invisible(NULL))
  })
}
