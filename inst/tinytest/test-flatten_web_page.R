# File created by roxut; edit the function definition file, not this file

# Test found in flatten_web_page.R:16 (file:line)
  
url <- "https://chemospec.org/posts/2020-01-01-Intro-F4S/2021-01-01-Intro-F4S.html"
res <- flatten_web_page(url)
expect_inherits(res, "character")
