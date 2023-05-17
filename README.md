# proteomics-protigy-revamp

### Useful resources

-   [Basics for R shiny modules](https://shiny.posit.co/r/articles/improve/modules/)
-   [More info on Shiny modules](https://mastering-shiny.org/scaling-modules.html)
-   [Basics for shiny code in an R package](https://mastering-shiny.org/scaling-packaging.html)
-   [Basics for testing your app](https://mastering-shiny.org/scaling-testing.html)
-   R package code structure for [R scripts](https://r-pkgs.org/code.html) and [other components](https://r-pkgs.org/misc.html)
-   [Advanced deployment](https://engineering-shiny.org/deploy.html)
-   [Handling package dependencies](https://r-pkgs.org/dependencies-in-practice.html)

### Managing package dependencies

Dependencies need to be listed in 2 places:

1.  In the `DESCRIPTION` file under `Imports`. This add the dependency as a part of your package metadata. Importantly, this **does not load the dependency!**
    -   You can use the function `usethis::use_package` to automatically add the dependency to your imports section (e.g. run `usethis::use_package("shiny")` from the R console)
    -   Alternatively, you can manually edit the `DESCRIPTION` file.
2.  In `R/protigyRevamp-package.R` as an `@import` or `@importFrom` statement. This actually loads the package when the app runs.
    -   `@import` will load the entire package. It's similar to `library` or `require`.
    -   `@importFrom` lets you load only specific functions. This is best if you're only using a couple functions from a large package.
    -   **IMPORTANT:** If you edit any `@import` or `@importFrom` commands, you need to update your package's `NAMESPACE`! To do this, run `devtools::document()`.

### Testing

1.  To begin a test, open the script you want to write tests for and run `usethis::use_test()`. This will create a corresponding test file in `tests/testthat`.
2.  Add tests using the `testthat` format (a dummy example should be auto-generated so you can see the format). Common tests will utilize `expect_equal()` or `expect_snapshot()`. Read more [here](https://mastering-shiny.org/scaling-testing.html).
