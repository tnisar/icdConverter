#' Convert ICD-10 Codes to Standard Decimal Format
#'
#' This function takes ICD-10 codes and converts them into the standard decimal format.
#'
#' @param icd_codes A character vector of ICD-10 codes.
#' @return A character vector of ICD-10 codes in standard decimal format.
#' @export
#' @examples
#' icd_to_decimal(c("A010", "B20"))
icd_to_decimal <- function(icd_codes) {
  # Ensure input is character
  icd_codes <- as.character(icd_codes)

  # Add a decimal point after the third character
  decimal_codes <- sapply(icd_codes, function(code) {
    if (nchar(code) > 3) {
      paste0(substr(code, 1, 3), ".", substr(code, 4, nchar(code)))
    } else {
      code
    }
  })

  return(decimal_codes)
}
