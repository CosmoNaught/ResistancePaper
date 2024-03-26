check_iso_codes <- function(iso_codes) {
  library(foresite)
  
  # Initialize a vector to store ISO codes that fail
  invalid_codes <- c()
  
  # Loop through each ISO code
  for (iso_code in iso_codes) {
    # Attempt to call get_site() with tryCatch to handle errors
    result <- tryCatch({
      get_site(iso_code)
      TRUE  # If get_site() succeeds
    }, error = function(e) {
      FALSE  # If get_site() fails
    })
    
    # If the result is FALSE, add the ISO code to the invalid_codes vector
    if (!result) {
      invalid_codes <- c(invalid_codes, iso_code)
    }
  }
  
  # Return TRUE if there are no invalid codes, otherwise return FALSE and the invalid codes
  if (length(invalid_codes) == 0) {
    return(TRUE)
  } else {
    return(list(FALSE, invalid_codes))
  }
}

