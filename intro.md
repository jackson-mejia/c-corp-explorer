## C-Corp Explorer
***

### What is this app?

This is a Shiny application created for our research team at the University of Minnesota to explore data extracted from the SEC's EDGAR database by Thomas May and Tobey Kass, whose unique contribution is the extraction of firm-level effective tax rates (and subcomponents) for all firms filing a 10K from the years 2009 onward.

Further plans for this app include:
* Regression
  * Select type of regression
  * Automatically take subsets of data if size is too large
* Clean up visuals
* More flexibility in subsetting
* Reset button
* Default Configuration
* Add relevant papers to bottom of readme (last thing)
<br> 

### How do I use it?

Navigate through the app using the tabs at the top. Further detail and instruction are provided on each tab. A high level overview of what each does presented below. __For the app to operate in an informative way, it is essential to start with the Data Setup tab__:

* __Data Setup:__ This tab contains three subtabs and is crucial for conducting analysis in the remainder of the app. 
  * __Select Variables:__ The first subtab, Select Variables, is used to select any variables desired for the analysis. 
  * __Filter and Subset:__ The second subtab, Filter and Subset, allows the user to filter and subset data based on the variables selected in the first subtab. 
  * __Data Viewer:__ Finally, the Data Viewer subtab allows the user to view the output of his selection and filtering/subsetting, with the option to download.
  * __Data Definitions__: View data definitions for all variables in the analysis.
* __Line Plot:__ A line plot which allows us to view any given numeric variable across time,  with tabs for different viewing options. 
  * __All Firms:__ Track all firms in the dataset across time with the ability to zoom in on a particular NAICS code and subset by numeric variables. 
  * __NAICS:__ View aggregated data for each NAICS code across time with the ability to subset by numeric variables.
  * __Single Firm:__ Track at least one firm at a time with the ability to zoom in on a particular firm and subset by numeric variables.
* __Scatterplot:__ Similarly to the previous tab, scatterplot allows the user to view numeric variables across time, with the size of a dot reflecting its relative size in terms of log sales and the color reflecting its NAICS code.
* __Distributions:__ This tab allows for two different views of how variables are distributed.
  * __Histogram:__ View numeric variables in a histogram, allowing for yearly NAICS subsetting, as well as an option to truncate.
  * __Summary Statistics__: View summary statistics for numeric variables, with an option to weight by sales.
  * __Outlier Analysis__: Choose to group by year or NAICS (or no grouping) and and view boxplots. Outliers are automatically generated, viewable in a table format, and downloadable.
* __Regression:__ This tab is used to generate regression models. There are two subtabs:
  * __Model Summary:__ Generate OLS or panel regression summaries, with an option to generate interaction terms. Also gives views of residual plots and fitted vs actual variables.
  * __Data with Residuals:__ View data with a column of residuals generated from the Model Summary tab. Option to download as csv.
* __Raw data:__ Data sourced from Thomas May and Tobey Kass. Option to download as csv.

<br>

### Who made it?

Jackson Mejia, a research assistant to Professors Anmol Bhandari and Ellen McGrattan. Contact him via email at <mejia069@umn.edu>.
