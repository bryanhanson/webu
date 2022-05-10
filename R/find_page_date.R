#'
#' Find Date on a Web Page
#'
#' Find the most recent date on a web page that is not in the future.
#' Looking for anything that can be converted to YMD.
#' Inspired by the Python package htmldate, but no code in common.
#'
#' @param string Character.  A single character string containing the contents of a web page.
#' @return YMD in ISO 8601 format.  Never returns a date in the future.
#'
#' @importFrom stats na.omit
#' @importFrom lubridate parse_date_time today
#' @importFrom stringr str_extract str_extract_all
#' @export
#'
#' @examples
#' res <- find_page_date(flatten_web_page("http://dirk.eddelbuettel.com/cranberries/cran/new/"))
#'
#' @tests tinytest
#' url <- "https://chemospec.org/posts/2020-01-01-Intro-F4S/2021-01-01-Intro-F4S.html"
#' res <- find_page_date(flatten_web_page(url))
#' expect_equal(res, lubridate::date("2020-01-01"))
#'
find_page_date <- function(string) {

  # Commmon items
  eng_months <- c(
    "January", "Jan", "February", "Feb", "March", "Mar",
    "April", "Apr", "May", "June", "Jun", "July", "Jul",
    "August", "Aug", "September", "Sept", "October", "Oct",
    "November", "Nov", "December", "Dec"
  )

  year_pat <- paste(2000:2050, collapse = "|")

  # Date patterns (add new patterns here)
  # ISO 8601 format
  iso <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"

  # 2020/01/01
  ymd_slash_num <- "[0-9]{4}/[0-9]{2}/[0-9]{2}"

  # 2020/Jan/01 2020/January/01
  ymd_slash_eng <- paste0(
    "(",
    year_pat,
    ")/(",
    paste(eng_months, collapse = "|"),
    ").?/[0-9]{1,2}"
  )

  # January 1, 2020   January 1 2020   January 1st 2020   Jan. 1st 2020   formats
  mdy_space_eng <- paste0(
    "(",
    paste(eng_months, collapse = "|"),
    ").?\\s+[0-9]{1,2}(st|nd|rd|th)?,?\\s+(",
    year_pat,
    ")"
  )

  # 1 January 2020   1 January, 2020  formats
  dmy_space_eng <- paste0(
    "[0-9]{1,2}(st|nd|rd|th)?\\s+(",
    paste(eng_months, collapse = "|"),
    ").?\\s+(",
    year_pat,
    ")"
  )

  # Combine all the patterns here (add new pattern names here)
  all_pats <- c(iso, ymd_slash_num, ymd_slash_eng, mdy_space_eng, dmy_space_eng)

  # Find the date(s)
  mydate <- rep(NA_character_, length(string))

  for (i in 1:length(all_pats)) {
    tmp <- str_extract(string, all_pats[i]) # Finds just the first date, but unlikely to be more than one
    mydate <- ifelse(is.na(tmp), mydate, tmp) # Will overwrite results found by earlier patterns - ?
  }

  # Clean things up
  mydate <- na.omit(mydate)
  if (length(mydate) == 0L) {
    return(NA_character_)
  } # a way to detect nothing found
  attributes(mydate) <- NULL # remove omit tags
  mydate <- unique(mydate) # format could be nearly anything at this point
  # add new pattern formats next line
  mydate <- parse_date_time(mydate, orders = c("ymd", "b!dy", "db!y"))
  mydate <- date(mydate) # just Y-M-D as ISO 8601, no time
  mydate <- mydate[order(as.Date(mydate), decreasing = TRUE)]
  mydate <- mydate[mydate <= today()] # no dates in the future!
  mydate <- mydate[1]
}
