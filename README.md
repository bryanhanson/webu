[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)]()

## What is webu?

`webu` is an `R` package with utility functions for working on the web.  The main uses are:

* Searching Github repositories for topics.
* Seaching PyPi.org repositories for topics.

### Installing webu from Github:

````r
install.packages("remotes")
library("remotes")
install_github(repo = "bryanhanson/webu@master")
library("webu")
````

If you use `@some_other_branch` you can download other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### License Information

`webu` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://www.gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
