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
5.  Any inputs that the module will use (e.g. GCT's, parameters, colors, etc.) must be fed in as inputs. Both the UI and server function can take inputs. Typically, inputs to the server are reactive and inputs to the UI are not reactive.

------------------------------------------------------------------------

### General Architecture for Handling Omes

Many global objects in the app are set up to handle multiple omes. In general, any object that contains data from each ome should be set up as a named list. The names will be the user-provided ome labels, and the value will be whatever data that object stores.

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

**`GCTs_and_params`:** A reactive list containing the individual ome GCTs, merged GCT with all omes, and corresponding parameters after setup/preprocessing. Call this using `GCTs_and_params()`. Update by using `GCTs_and_params(NEW_GCTS_AND_PARAMS)`. Really, this should only be updated during setup.

-   `GCTs_and_params()$GCTs`: A named list of parsed and processed GCT objects. Names correspond to the user provided ome labels. Multi-ome data is not included here. It will look something like this:

    ```         
    > GCTs_and_params()$GCTs
    $Prot 
    Formal class 'GCT' [package "cmapR"] with 7 slots ...

    $Prot
    Formal class 'GCT' [package "cmapR"] with 7 slots ...

    $RNA
    Formal class 'GCT' [package "cmapR"] with 7 slots ...
    ```

-   `GCTs_and_params()$GCTs_merged`: A GCT object. This is the merged GCT containing ***ALL*** omes. Each ome is identified with the `protigy.ome` column in the gct's `rdesc`, which contains the user-specified ome labels. This GCT is created using the `merge_processed_gcts()` function during preprocessing.

-   `GCTs_and_params()$parameters`: A named list of setup parameters for each ome. Names correspond to user provided ome labels. The user selects these parameters in the sidebar. It will look something like this:

    ```         
    > GCTs_and_params()$parameters # named list of parameters
    $Prot: List of 9
    $Phos: List of 9
    $RNA: List of 9

    > GCTs_and_params()$parameters$Prot # parameters for a single ome
    $gct_file_path
    [1] "/var/folders/52/lnhkwk791mb8x2xqdnkv837w0000gq/T//RtmpOUAtjA/a1b10dd3206e7ca97d4c209c/1.gct"

    $gct_file_name
    [1] "proteome-aggregate.gct"

    $log_transformation
    [1] "None"

    $data_normalization
    [1] "None"

    $data_filter
    [1] "None"

    $max_missing
    [1] 100

    $intensity_data
    [1] "No"

    $group_normalization
    [1] FALSE

    $annotation_column
    [1] "PAM50"
    ```

`globals`: A `reactiveValues` list of global app objects. This should contain objects that the user can change after setup that will affect multiple modules. Call an object from globals using `globals$object`. Update an object using `globals$object <- new_object`. Currently, `globals` contains:

-   `globals$colors`: A named list of colors produced by `set_annot_colors()` in the "Customize" tab. Names are user provided ome labels and `"multi-ome"`. It will look something like this:

    ```         
    > names(globals$colors)
    [1] "multi_ome" "Phos"      "Prot"      "RNA" 

    > names(globals$colors$Prot)
     [1] "Sample.ID"      "Experiment"      "Channel"      ... 

    > globals$colors$Prot$PAM50
    $is_discrete
    [1] TRUE

    $vals
    [1] "Basal" "Her2"  "LumA"  "LumB" 

    $colors
    [1] "#117733" "#88CCEE" "#CC6677" "#999933"
    ```

-   `globals$default_ome`: A string containing the user selected default ome. This should be the first ome tab a user sees when they navigate to a module.

`GCTs_original`: A `reactiveVal` named list of the original GCT objects that the user uploaded (before any processing). Names correspond to user-selected ome labels. This has the exact same structure as `GCTs_and_params()$GCTs`.

------------------------------------------------------------------------

### Exporting from a Module

The "Export" tab is set up to handle all exports from the app. Follow these steps to export a plot/table/object/etc. from your module. An example is shown in `R/tab_TEMPLATE_SINGLE-OME.R`.

1.  Write a custom export function inside your module's server for each object you would like to export.
    -   The export function should take a single input, `dir_name`, which is the output directory where your export should be saved. You can assume this directory will already exist at the time that the export function is called.
    -   The export function should save a file inside of the directory `dir_name`.
    -   The contents of the function should generate your export file. You can assume that the function will be called in a reactive environment, so you should use reactive variables as such.
    -   This function can and should throw an error if the export cannot be generated (say, you are exporting a plot, but the GCTs have not been processed yet).
2.  Save each export function in a list of lists. The first level of the list should be the ome name or `"multi_ome"`. The second level of the list should contain export functions for each object and their names.
3.  Return the list of lists from your module's server.
4.  Assign your modules output in `app_server()`. Then, add the output to the `all_exports` list at the bottom of `app_server()`.

See here for a written out example of what your modules server should look like. It doesn't quite handle the complexity of having multiple omes, but it gives the general idea. See `R/tab_TEMPLATE_SINGLE-OME.R` for a more complete example.

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

# create the nested list of exports
module_exports <- list()
module_exports[[ome]] <- list(
  example_plot <- example_plot_export_function
  # add other functions for plots/exports here
)

return(module_exports)
```

------------------------------------------------------------------------

### Updating global variables

Most modules should **not** need to update any global variables (beyond exporting plots/tables/etc.). However, some special modules may want to add/edit global variables that will affect multiple modules. Here's some suggestions for how to hand that logic.

**Editing/adding to `globals` (or any `reactiveValues` object):**

1.  Have your module's server return a reactive version of the object that you want to add to `globals`. The module can return just the reactive object or a list containing the reactive object if you also need to return other things. Note: make sure to return the reactive *object* (e.g. `return(my_reactive_object)`) instead of *calling* the reactive object (e.g. DO NOT DO `return(my_reactive_object())`).

2.  Assign the output to a variable in `app_server()`.

3.  Write an `observeEvent` statement in `app_server()` to update the correct field in `globals`. Something like:

    ``` r
    ## Inside of app_server ...

    # assign the output of your module's server
    my_reactive_output <- myModuleServer()

    # observeEvent to update globals each time the module's output changes
    observeEvent(my_reactive_output(), {
      globals$myField <- my_reactive_output()
    })
    ```

**Editing `GCTs_and_params`** **(or any `reactiveVal` object):** This can follow a similar logic as adding to `globals` (return reactive object, update using an `observe` statement in `app_server()`). However, the catch is that GCTs_and_params is on giant `reactiveVal` object, so you cannot just update an individual field....the whole object must be updated together. I imagine updating this would look something like:

``` r
# I want to update the `GCTs` part
new_GCTs_reactive <- ...

observeEvent(new_GCTs_reactive(), {
  # grab the current GCTs_and_params
  new_GCTs_and_params <- GCTs_and_params()
  
  # edit with your new GCTs
  new_GCTs_and_params$GCTs <- new_GCTs_reactive()
  
  # update the GCTs_and_params reactive val
  GCTs_and_params(new_GCTs_and_params)
})
```
