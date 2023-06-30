# Module requirements for the Protigy Revamp App

**TLDR**: Check out these two scripts to see how to add a module to the app:

-   `R/tab_TEMPLATE.R`: Basic requirements and format for a module. Shows how to format a module server/UI function pair, get the `ns()` function, and obtain relevant inputs.

-   `R/tab_TEMPLATE_SINGLE-OME.R`: Format for how to create a module that displays a tab for each ome (similar to the "Summary" tab). Builds on `R/tab_TEMPLATE.R` with code for creating an ome tabBox, content for each module, and custom export functions. Contains basic example code that should be deleted once module is finalized.

------------------------------------------------------------------------

### Basic requirements

These are the most bare-bones requirements to make a functioning module in the Protigy revamp app. This functionality is mirrored in `R/tab_TEMPLATE.R` for your reference.

1.  A module must contain one UI function and one server function. Both of these functions must take in a string input `id` that is used to pair the module's UI and server together.
    -   `id` must be identical in the UI and server functions for them to communicate with each other.
    -   `id` can be set as a function default (like it is in `R/tab_TEMPLATE.R`) or fed into the server/UI function when it is called.
    -   No two modules can have the same `id`.
    -   Check out [this resource](https://mastering-shiny.org/scaling-modules.html#module-basics) for more information on how to format a module.
2.  Modules must make proper use of the namespace function `ns()`. Check out `dev/README.md` for tips on how to use `ns()`.
3.  The module's server function must be called somewhere in the `app_server()` function located in `R/app_server.R`.
4.  The module's UI function must be called in the `app_ui()` function located in `R/app_ui.R`.
5.  Any inputs that the module will use (e.g. GCT's, parameters, colors, etc.) must be fed in as inputs. Both the UI and server function can take inputs. Typically, inputs to the server are reactive.

------------------------------------------------------------------------

### General Architecture for Handling Omes

Many global objects in the app are set up to handle multiple omes. In general, any object that contains data from each ome should be set up as a named list. The names will be the user-provided ome label, and the value will be whatever data that object stores.

For example, the object `GCTs` is commonly used. This object is set up as a named list. So, if your omes are `"Prot"`, `"Phos"`, and `"RNA"`, it will look something like this:

```         
> GCTs
$Prot 
Formal class 'GCT' [package "cmapR"] with 7 slots ...

$Prot
Formal class 'GCT' [package "cmapR"] with 7 slots ...

$RNA
Formal class 'GCT' [package "cmapR"] with 7 slots ...
```

In instances where an object could also hold multi-ome data, the reserved word `"multi_ome"` is used as an additional name. `globals$colors` and `all_exports` are examples of where this format is used.

------------------------------------------------------------------------

### Explanation of Global Variables

------------------------------------------------------------------------

### Exporting from a Module

The "Export" tab is set up to handle all exports from the app. Follow these steps to export a plot/table/object/etc. from your module. An example is shown in `R/tab_TEMPLATE_SINGLE-OME.R`.

1.  Write a custom export function inside your module's server for each object you would like to export.
    -   The export function should take a single input, `dir_name`, which is the output directory where your export should be saved. You can assume this directory will already exist at the time that the export function is called.
    -   The export function should save a file inside of the directory `dir_name`.
    -   The contents of the function should generate your export file. You can assume that the function will be called in a reactive environment, so you should use reactive variables as such.
    -   This function can and should throw an error if the export cannot be generated (say, you are exporting a plot, but the GCTs have not been processed yet).
2.  Save each export function in a list of lists. The first level of the list should be the ome name or `"multi_ome"`. The second level of the list should contain export functions for each object and their names.
3.  Return the list of lists.

See here for a written out example. It doesn't quite handle the complexity of having multiple omes, but it gives the general idea. See `R/tab_TEMPLATE_SINGLE-OME.R` for a more complete example.

``` r
## In your module's server function ... ##

ome <- "Prot" # making plots for the proteome

# making my reactive plot
example_plot_reactive <- reactive({
  ggplot() + ggtitle(paste("Example for:", ome))
})

# Export function
example_plot_export_function <- function(dir_name) {
  ggsave(
    filename = paste0("example_plot_", ome, ".pdf"), 
    plot = example_plot_reactive(), # call the reactive object
    device = 'pdf',
    path = dir_name # save in the desired output directory
  )
}

module_exports <- list()
module_exports[[ome]] <- list(
  example_plot <- example_plot_export_function
  # add other functions for plots/exports here
)

return(module_exports)
```

------------------------------------------------------------------------

### Updating global variables
