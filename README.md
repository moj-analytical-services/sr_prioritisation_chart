# sr_prioritisation_chart

This is a tool for creating charts (along with some narrative) to help illustrate the options in the Prioritisation Tool.

There are two sides to the code:

## charting_functions.R

This contains functions for cleaning the data and creating the charts.

There are two data cleaning steps:

#### 1. Rename_and_filter_data

This selects the columns required for charting, and renames them to something more easily dealt with by R. It then filters out rows where the option reference is blank, so you're only left with actual options rather than intermediate headings etc.

An optional step is to show only a shortlist of options. As currently implemented, within the function "rename_and_filter_data", there's an optional parameter that expects a vector of option references. If supplied, the code will filter the data to just those options. 

In the absence of any "shortlist" flag in the existing dataset, you'll presumably supply this in a separate file, which you'll load in separately. An alternative approach would be to add a "shortlist" column in the dataset, and change the code to have the "include_only_shortlist" be a boolean, where if set to TRUE, filters where shortlist == "yes" or something like that.

#### 2. reorder_categorical_variables
recodes and reorders the variables, such that the categories are presented in the correct order (i.e. from bad to good), and labels are made particularly clear.

#### Charts

There are four specified charts:

1. An overall chart that plots all dimensions, using a facet grid to allow two levels of X/Y coordinates.
2. Strategic alignment vs deliverability
3. Evidence vs Impact on outcomes
4. Strategic alignment vs impact on outcomes


## Prioritisation_charts.Rmd

This is an RMarkdown file for assembling the report based on the desired charts, mixing code and prose. This should hopefully be straightforward to follow. When you "knit" this file, it will output an HTML file which you can distribute.

## Report generation

To generate a report:

1. Clone this repository into RStudio
2. Upload the current version of the Part B collator to Amazon S3
3. Change the path in line 25 of prioritisation_charts.Rmd to direct to the new file
4. Click "knit" to knit the RMarkdown and turn into an HTML document
5. To download, in the Files pane, tick the newly-generated prioritisation_charts.html, click More --> Explort...

