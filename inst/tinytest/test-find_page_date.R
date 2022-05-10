# File created by roxut; edit the function definition file, not this file

# Test found in find_page_date.R:19 (file:line)
  
url <- "https://chemospec.org/posts/2020-01-01-Intro-F4S/2021-01-01-Intro-F4S.html"
res <- find_page_date(flatten_web_page(url))
expect_equal(res, lubridate::date("2020-01-01"))
