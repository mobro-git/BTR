# Net-GHG Emissions Projections for the UNFCCC Biennial Transparency Report 

Code for interactive exploration, QA, compilation, and visualization of emissions and energy data for the BTR.

## File and Folder Organization:

* /_targets -- Stores rData objects and metadata for completed 'targets' - !will only appear after the targets pipeline has run!
* /data-extra -- Data used in the pipeline, but not directly. It holds files that are used elsewhere or still need transformation before they're used in the pipeline.
* /data-raw -- all raw data, templates, variable definitions, and crosswalks
  + /archive -- data not currently used in the pipeline
  + /calculated-var -- workbooks to define calculated variables for energy model reported variables
  + /crosswalk -- crosswalks to define model and scenario names, map variables between usproj and model-runs workbooks, and define models and scenarios to be used for the full net GHG BTR projections
  + /lulucf -- LULUCF carbon stock change projections, from EPA and USDA
  + /model-runs -- energy model projections for current BTR as well as results for comparison - !results MUST be in reporting template format! (see template below)
  + /ncbr_comparison -- past NatCom/BR/Climate Action Report projections for comparison
  + /template -- reporting template for co2 fossil fuel combustion emissions and energy data, BTR2024 using EMF37_data_template_R2_v2.xlsx
  + /usproj-data -- current BTR projections for non-co2 gasses and co2 emissions from IPPU and NEU using NEMS activity drivers - from EPA's usproj repository
* /docs -- contains r markdowns and quarto reports
* /figure-maps -- Workbooks to define and create comparison figures
* /output -- output directory for data products, figures, and reports
* /R -- package-like functions only supporting the data pipeline and functions are organized by subject matter
* /scripts -- scripts and code to support the pipeline

## Getting Started:

The data pipeline is managed via targets. The overall plan is listed in _targets.R. To run the project, from the console run: ```targets::tar_make()``` from the project working directory. (e.g., via opening the LEEP.Rproj file). Final results are html and text in /outputs. 

TO run the project interactively:
1) Load packages: ```source("packages.R")```
2) Load custom functions: ```tar_source()```
3) Load cached intermediate objects: ```targets::tar_load(everything())``` (or specify names of particular targets to load). ```targets::tar_load(everything())``` will take a long while to load everything into the environment. Loading particular targets is advised. 

From there, individual functions or targets can be run interactively. The targets package also provides customized debugging tools beyond the scope of this document.

# I. Pipeline User Guide 

To use the pipeline to create necessary statistics and graphics, users should follow the following step:

## Step 1. Activate the Virtual Environment

Click on the `BTR.Rproj` file to enter the virtual environment for the project, which captures all the required versions of the packages used in the pipeline. 

## Step 2. Run the pipeline 

The `_targets.R` file details the entire pipeline, including the steps to loading and processing historical and projected data, calculating transformed data, creating summary statistics, and producing graphics. The main benefit of using a targets pipeline is that it tracks changes and dependencies so that we don't need to re-run everything every time we change something. Once the pipeline is run once, only the target objects and their relevant dependencies that have changes will be re-run. 

### 1) Targets File Structure 

The majority of the `_targets.R` file is nested within the `tar_plan()` function. This is a comma-separated large list of all of the targets, aka tracked action items, for the pipeline to run and is what the targets package looks to to calculate dependencies. Items in the `_targets.R` script before `tar_plan()` are automatically run before anything in `tar_plan()` every time the pipeline is run. It is where you can put items that are not action items that you want to track, and should always include `library(targets)`, `library(tarchetypes)`, `source("packages.R")`, and `tar_source()`.

The `tar_plan()` list contains roughly four types of objects: 

b) **settings**: a list of (sort of global) variables, e.g., all_scenarios. These variables are what we want to make available to all the `.R` and `.Rmd` files through the `tar_load(config)` command.

a) **config**: a list of (sort of global) variables, e.g., all_scenarios. These variables are what we want to make available to all the `.R` and `.Rmd` files through the `tar_load(config)` command.

b) **file objects**: these objects include the template, metadata, raw data files, and spreadsheet containing instructions on what new variables to create and what graphs to generate. These objects just track files and their locations, not what is in them. They are created using `tar_target(target_name, "filepath", format = "file")`. The last parameter, `format`, is what indicates that this target will track only the file, not what is in it. 

c) **functional and data objects**: these objects track or manipulate data. They can be created either using `tar_target()` or a more simplified format `target_name = whatever_you_do_to_create_the_target`. If you have a code chunk following the "=", you need to put the code within "{}" so that it executes correctly. 

There are many spots within the `_targets.R` file that contain pairs of file objects and functional/data objects, such as:

```{r targets data file demo, eval=FALSE}
tar_target(emf_template_csv, "data-raw/templates/EMF37_data_template_R1_v5.xlsx", format = "file"),
emf_template = read_emf_template_xlsx(emf_template_csv),
```

We use these pairs when we want to track both a file path and the data within that file.

The first line assigns the path to a given file to a targets object in the format of "file" with a predetermined name. In our example, the target object is in the format of file and has the name of `emf_template_csv`, and it tracks the template `EMF37_data_template_R1_v5.xlsx`. If the template file ever changes, target objects with `emf_template_csv` as a dependency would automatically re-run. The second line of the code then actually read in the data using the `read_emf_template_xlsx` function.

One target is created by each line of code: (1) `emf_template_csv` (2) `emf_template`. No two targets can have the same name. The first target is created using the function `tar_target()` while the second target omits that function and uses the second method of target creation, described above. If the `emf_template` target ran thorugh a chunk of code instead of just using one function (`read_emf_template_xlsx`), the code chunk would be within curly brackets {} and would look something like this:
`emf_template = {read_emf_template_xlsx(emf_template_csv) %>% filter(variable == "test_variable) %>% select(-region}`

d) **rmarkdown files**: in the `_targets.R` file, we also see code like:

```{r targets rmarkdown file demo, eval = FALSE}
  tar_render(
    audit_sums_report,
    "docs/round1/audit_sums.Rmd",
    output_dir = "output/round1/audit",
    output_file = "audit sums report",
    params = list(
      mode = "targets"),
  ),
```

Code like this renders the corresponding rmarkdown file. The first parameter names the target, the second one points to the file path of the rmarkdown, output_dir gives the file directory to save the rendered output, output_file specifies the file name, and params lets you set parameters that your rmakrdown has in the yaml header. To link dependencies between rmakrdown targets and other targets, use `tar_load()` within the markdown to pull in other targets.


### 2) Relevant Commands

To run the pipeline after loading the EMF37viz.Rproj, use `targets::tar_make()` command. 

To invalidate a pipeline (delete all caches), use `targets::tar_invalidate()` command. If you want to specify a specific target, use `tar_invalidate(target_name)`.

To load a target object inside a function/file into your local environment, use `targets::tar_load(xxx)` command, where xxx denotes the target name. 

To see a map of all of the targets and their linked dependencies, use `targets::tar_visnetwork(targets_only = TRUE)`


# II. Graphics Guide (for functions already in the pipeline)

To create new graphics of the existing types (e.g., stacked bar, time series, cone of uncertainty), users shall follow the following step: 

## Step 1. Create/Edit the Necessary Plot Mapping file 

Spreadsheets containing plotting specifications should be within the `plot_mapping/roundxxx` folder and follow a specific naming convention. Convention = `subject_figtype.csv`, for example `h2_diffbar.csv` for a hydrogen difference bar figure. Figtype options are as follows:
* cone - uncertainty band across models, scenarios, or variables
* diffbar - stacked bar showing difference between scenarios, models, or years
* sankey - sankey diagram with nodes and links
* scatter - scatter plot
* stackbar - stackied bar showing objective values
* timeseries - time series plots

The subject is just a name that is used to organize the outputs. E.g. "overview" is for our overview slides and contain a ton of figures to get an overview of the datasets.

**IMPORTANT**: Any number of plot maps can exist within the `plot_mapping/roundxxx` folder, but targets will only be created for those listed within the `figmap.csv` file. This file serves as an input to a target-producing function, `tar_map()`, that is used within the pipeline. This function automatically creates pairs of targets, one to track the .csv within the plot_mapping folder and one to track the data within each plot map. In the `figmap.csv` file, list the subject of the plot map in the fig_subject column and the figure type in the fig_type column. These names should match exactly with the filename of the plot map. The `figmap.csv` is tracked outside of `tar_plan()` at the beginning of the `_targets.R` file:

```{r figmap creation, eval = FALSE}
figmap_list_csv = "plot_mapping/round2/figmap.csv"
figmap_list = read_csv(figmap_list_csv) %>% as_tibble()
```

Within `tar_plan()`, there is separate code that creates the pairs of targets to track the plot map files and data:

```{r figmap targets creation, eval = FALSE}
tar_map(
    values = figmap_list,
    tar_target(figmap_csv, figmap_csv_path(fig_subject, fig_type, config), format = "file"),
    tar_target(figmap, import_figure_csv(figmap_csv, fig_type, config))
  )
```

The two targets created are in the following format: `figmap_subject_figtype_csv` for the format = "file" targets, and `figmap_subject_figtype` for the data targets. NOTE that the some of the figtype inserted into the target name are an abbreviated version: 

* stackbar -> sb
* diffbar -> db
* cone -> cu
* timeseries -> ts

To see the targets being created behind the scenes with `tar_map()`, run `tar_visnetwork(targets_only = TRUE)`.

## Step 2. Edit the `_target.R` file 

To create figures, add additional targets for each new plot map. E.g.: 

```{r new graphics demo, eval = FALSE}
overview_sb_graphs = create_graph("overview", "stacked_bar", config, emf_data_long, overview_sb_figmap),
```

The `create_graph` function is a large wrapper for many sub-functions for plotting. 

parameter 1 = subject, as specified in the plot map file name
parameter 2 = figure type, options include "stacked_bar", "diff_bar", "time_series", "cone_uncertainty", "scatterplot". NOTE the syntax of these figure types is different than those that you should use in the plot map file name.
parameter 3 = config
parameter 4 = data, emf_data_long is the cleaned data that works best, but you can use subsets of that dataset as well (e.g. usrep_reeds_data_long)
parameter 5 = plot map data target name, e.g. overview_sb_figmap
parameter 6 = pdfGraphs, default = TRUE
parameter 7 = pngGraphs, defualt = TRUE

For parameters 6 and 7, set = FALSE to either suppress the creation of the PDF or the PNGs.

## Step 3. Run the Pipeline 

After adding the necessary plot mapping files and create_graph commands, you can use `targets::tar_make()` to run the pipeline. If there is no error message, then the plots are successfully created. 

## Step 4. Check the outputs

Figure PDFs and PNGs are saved in the `output/subject/` folders. Within each subject folder should be all of the PDFs (one for each figure type) and a folder for each figure type containing the PNGs of each figure.


## Important: Function Design Logic and Troubleshooting

I have built in a series of error messages to help aid the debugging process in case anything goes wrong. 

### 1) Plot Mapping Step 

All the code used for processing the plot mapping csv files are in the `R/4a_plotmap_processing.R` file. Specifically, multiple functions are packed in the `import_figure_csv` function to import a given csv file and check if the file meets requirements. 

The first thing the `import_figure_csv` function checks is whether an input figure_type is valid (i.e., supported by the pipeline). Currently, that includes "stacked" (stacked bar), "ref_stacked" (reference stacked bar),"diff" (stacked bar for differences between variables/scenarios), "ts" (time series), "cu" (cone of uncertainty), and "scatter" (scatter plots). If the function passes the test, then it proceeds to the next step. Otherwise, it kills the pipeline and displays an error message reminding you to use a correct figure type. 

Next, the function reads in the plot mapping list. 

Then, the function checks whether the plot mapping file has standard columns as specified in the `R/4a_plotmap_processing.R` file via the `assert_figure_csv_has_standard_columns` function. Standard columns currently include the following steps
```{r new graphics error code standard column, eval= FALSE}
standard_fig_cols <- ("figure_no", "title_name", "variable", "variable_rename", "flip_sign",
                       "regions", "models", "years", "scenarios",
                       "type", "x", "y", "facet1", "facet2", "page_filter", "color", "scales")
```
and each type of graphics has more or fewer variables depending on their specific needs. 

If the plot mapping file does have all the standard columns, then the function continues to check the content in the file, namely whether 

1) All the variables are from the template or calculated variable spreadsheets (function `assert_vars_in_template_or_calculated`). If some variables do not meet the requirement, an error message of **"Missing at least one standard column in the plot mapping csv."** would be given. 

2) All the model configurations are named vectors in config or valid model names. If this is not true, i.e., if some values in the "models" column in a given spreadsheet are not valid model names or not in config, then an error message of **[The value] in 'models' column NOT in config** would be given. 

3) same for scenarios checked by the `assert_scenarios_in_config` function. 

4) same for regions checked by the  `assert_regions_in_config` function.

5) same for years checked by the `assert_years_in_config_or_numeric` function. 

6) Next, the function checks if all plots in the plot mapping file are using valid page filter, namely, "region", "year", "scenario", or "model". If not, an error message of **"in the 'page_filter' column need to be region, year, scenario, or model."** would be displayed. 

7) Finally, the `check_figure_specification` function checks if each plot indicated in the plot mapping file has unique specifications for specific columns. For example, for stacked bar plots, the columns 

```{r new graphs error code check figure, eval = FALSE}
stacked = c("title_name",
                  "regions", "models", "years", "scenarios",
                  "type", "x", "y", "facet1", "facet2", "page_filter", "color", "scales")
```

should all be the same for the same plot.


If all the checks are passed, the plot mapping file is then fully imported and converted to a target object. 


### 2) Figure Data Processing Step

With the plot mapping file imported, we can start processing and manipulating the data to create the grpahics we want. In the pipeline, this step and the next step are all packed in the `create_graph` function in the `R/4_create_graph.R` file for ease of use. But it contains several separate steps. If we look into the create_graph function, we can see that it 

1) creates new folders for graphics of a given type. If the folder already exists, nothing happens. 

2) The `emf_data_long` object containing all the emf data are passed through a `preliminary_data_processing_for_plotting` function, which processes the data preliminarily regardless of graphic type. The goal is to reduce computational costs. If we look into the function, we would note that it only filters out data containing variables specified in the given spreadsheet, and flip the sign of the values when indicated.


### 3) Plotting Step

3) The function then creates palettes for the specific graphs. Morgan can talk/write more about the palette creation functions.

4) The function calls the `pdf_plots` or the `png_plots` function in the `R/4c_figure_plotting_fn.R` file to actually create the plots. The functions, again, packed other layers. It, again, first checks whether the plot type is supported. If so, it uses a graph type specific function to process each figure in the spreadsheet. Example graph type specific functions include `time_series_figure_specific_data_processing` function in the `R/4c_figure_plotting_fn.R` file. It filters data down to relevant scenarios, years, regions, and models. Then, for each graph, we may need to create multiple copies for different categories. For example, we may need a time series of emissions from oil, coal, and gas for the United States and Canada individually. In this case, we will need to use "region" in the page filter as an indicator. In the `pdf_plots` and the `png_plots` functions, we loop through each of graph copies and generate the graph if it passes both the approved type test (function `approved_facet_type`) and the single unit test (`check_dat_before_plotting`). Otherwise, an error code would be given and the process stopped. 

