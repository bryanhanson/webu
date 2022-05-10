# File created by roxut; edit the function definition file, not this file

# Test found in get_github_dates.R:20 (file:line)
  
expect_inherits(get_github_dates("https://github.com/bryanhanson/ChemoSpec", "commits"), "Date")
expect_inherits(get_github_dates("https://github.com/bryanhanson/ChemoSpec", "issues"), "Date")
