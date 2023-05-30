# proteomics-protigy-revamp

### Useful resources

-   [Basics for R shiny modules](https://shiny.posit.co/r/articles/improve/modules/)
-   [More info on Shiny modules](https://mastering-shiny.org/scaling-modules.html)
-   [Basics for shiny code in an R package](https://mastering-shiny.org/scaling-packaging.html)
-   [Basics for testing your app](https://mastering-shiny.org/scaling-testing.html)
-   R package code structure for [R scripts](https://r-pkgs.org/code.html) and [other components](https://r-pkgs.org/misc.html)
-   [Advanced deployment](https://engineering-shiny.org/deploy.html)
-   [Handling package dependencies](https://r-pkgs.org/dependencies-in-practice.html)
-   [Custom CSS for shiny](https://unleash-shiny.rinterface.com/beautify-css.html#beautify-css)
-   [Testing your app](https://mastering-shiny.org/scaling-testing.html)

### Tips, Tricks, and Gotcha's

1.  **Loading changes:** Anytime you change anything in the package, you will need to re-load it with `devtools::load_all(".")`. This can be done using the shortcut `Cmd`/`Ctrl` + `shift` + `L`. Simply saving code changes is not enough!

2.  **Checking package validity:** Every once in a while, run `devtools::check()`. This makes sure everything in the package looks OK and checks for code errors. This check is also run automatically anytime you push to github.

3.  **When to use `ns()`:** The `ns()` function is critical for shiny modules. It allows you to keep input/output names contained within your module. *If you do not correctly use `ns()`, UI elements will not act properly, causing a world of frustrating errors :(*

    Use `ns()` in these cases:

    -   Anytime you set an `inputId` or `outputId` in your module's main UI function. For example: `textInput(inputId = ns('myText'))` or `plotOutput(outputId = ns("myPlot"))`.
    -   When you define UI within a `renderUI()` block in your server function. For example: `renderUI({textInput(inputId = ns('myText'))})`. (Note: you need to have the line `ns <- session$ns` somewhere in your module server function to be able to use `ns` from within the server.)
    -   Some special functions require `ns` as an input, such as `conditionalPanel`.

    Do NOT use `ns()` when:

    -   You are updating a UI element in the server *outside* of a `renderUI()` block. For example: `updateTextInput("myText")`.

4.  **Custom CSS**: If you need to customize the user interface, I recommend getting comfortable using "inspect element". This lets you see all the possible fields you can edit and what class(es) each element belongs to. Then [add your custom CSS](https://unleash-shiny.rinterface.com/beautify-css.html#beautify-css).

### Managing package dependencies

Dependencies need to be listed in 2 places:

1.  In the `DESCRIPTION` file under `Imports`. This add the dependency as a part of your package metadata. Importantly, this **does not load the dependency!**
    -   You can use the function `usethis::use_package` to automatically add the dependency to your imports section (e.g. run `usethis::use_package("shiny")` from the R console)
    -   Alternatively, you can manually edit the `DESCRIPTION` file.
2.  In `R/protigyRevamp-package.R` as a roxygen `@import` or `@importFrom` statement. This actually loads the package when the app runs.
    -   `@import` will load the entire package. It's similar to `library` or `require`.
    -   `@importFrom` lets you load only specific functions. This is best if you're only using a couple functions from a large package.
    -   Alternatively, you could put the `@import` or `@importFrom` statement just before your function definition. This would load the dependency only when that function is called. This is best for helper functions that require specific packages (e.g. some normalization and filtering functions).
    -   **IMPORTANT:** If you edit any `@import` or `@importFrom` commands, you need to update your package's `NAMESPACE`! To do this, run `devtools::document()`.

### Testing

1.  To begin a test, open the script you want to write tests for and run `usethis::use_test()`. This will create a corresponding test file in `tests/testthat`.
2.  Add tests using the `testthat` format (a dummy example should be auto-generated so you can see the format). Common tests will utilize `expect_equal()` or `expect_snapshot()`. Read more [here](https://mastering-shiny.org/scaling-testing.html).
3.  Check out the ways to run a test [here](https://devtools.r-lib.org/reference/test.html). My preference is using `devtools::test_active_file()`.

### Module Requirements
