#' Shows valid date formats
#'
#' The `undid_date_formats` function returns a list of all valid date formats
#' that can be used within the package
#'
#'
#' @return A list of valid date formats
#'
#' @export
#'
#' @examples
#' undid_date_formats()
undid_date_formats <- function() {
  list(General_Formats = .undid_env$date_formats_general,
       R_Specific_Formats = .undid_env$date_formats_r)
}
