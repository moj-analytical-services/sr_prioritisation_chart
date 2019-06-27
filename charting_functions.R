library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(forcats)
library(DT)



# Data cleaning functions -------------------------------------------------


get_shortlist <- function(chart_data) {
  # Returns chart_data with only the shortlisted options
  chart_data %>% filter(overall_recommendation %in% c("Yes", "Maybe"))
}

rename_and_filter_data <- function(chart_data, shortlist = FALSE) {
  # This takes the raw data, and selects just the columns we'ere interested in.
  # it's easier to work with variable names that are in camel_case without spaces,
  # as you don't have to use backticks.
  # Follow the pattern below to add columns where necessary:
  # select(new_variable = `old variable`).
  # Some of the columns have odd formatting, like carriage returns (see the \r\n) below.
  # To more easily determine how R is determining the column names, read in the file without any formatting,
  # then run names(chart_data)
  
  chart_data %>%
    select(option_ref = `Option Reference Number\r\n(hide col before sharing)`,
           type_of_option = `Option is`,
           option_description = `Option Description`,
           impact_on_outcomes = `Impact on outcomes (scale)`,
           financial_impacts = `Financial impacts`,
           strategic_alignment = `Strategic alignment (or other political priority) (1-3)`,
           deliverability_risk = `Deliverability risk is`,
           evidence = `The evidence on outcomes/\r\nperformance is`,
           total_cost = `The total cost (central estimate) is`,
           am_comments = `Info from Ams`,
           overall_recommendation = `Overall Recommendation`,
           recommendation_is_judgement = `* if judgement`) %>%
    filter(!is.na(option_ref),
           option_description != "Option Description")
  
  
  if(shortlist) {
    output <- output %>% get_shortlist()
  }
  else {
    output
  }
  
}

reorder_categorical_variables <- function(chart_data) {
  # this is used to manually set the order of the categories for categorical variables.
  # Generally useful to do in ascending order. Note that facet_grid outputs the y axis in reverse order,
  # so use forcats::fct_rev to reverse the order
  
  
  chart_data %>%
    mutate(evidence = recode_factor(evidence,
                                    "0" = "Not provided",
                                    .missing = "Not provided",
                                    "missing (Red RAG rating)" = "Missing evidence",
                                    "OK (Amber RAG rating)" = "OK evidence",
                                    "robust (green RAG rating)" = "Robust evidence"),
           deliverability_risk = recode_factor(deliverability_risk, 
                                               "0" = "Not provided",
                                               .missing = "Not provided",
                                               "Red RAG rating" = "Deliverability: Red",
                                               "Amber RAG rating" = "Deliverability: Amber",
                                               "Green RAG rating" = "Deliverability: Green"),
           financial_impacts = recode_factor(financial_impacts,
                                             "0" = "Financial impacts not provided",
                                             .missing = "Financial impacts not provided",
                                             "are missing" = "Financial impacts are missing",
                                             "are indicative" = "Financial impacts are indicative",
                                             "are robust" = "Financial impacts are robust"),
           impact_on_outcomes = recode_factor(impact_on_outcomes,
                                              "0" = "Not provided",
                                              .missing = "Not provided",
                                              "missing" = "missing impact on outcomes",
                                              "low" = "low impact",
                                              "medium" = "medium impact",
                                              "high" = "high impact on outcomes"),
           total_cost = recode_factor(total_cost,
                                      "0" = "Not provided",
                                      .missing = "Not provided",
                                      "More than £5.1m per year" = "More than £5.1m per year",
                                      "Between £1.1m and £5m per year" = "Between £1.1m and £5m per year",
                                      "Less than £1m per year / a net benefit" = "Less than £1m per year / a net benefit"),
           strategic_alignment = recode_factor(strategic_alignment,
                                               "0" = "Not provided",
                                               .missing = "Not provided",
                                               .default = "Not provided",
                                               "1" = "1: low alignment",
                                               "2" = "2",
                                               "3" = "3: high alignment"),
           recommendation_is_judgement = recode_factor(recommendation_is_judgement,
                                                       "*" = "judgement",
                                                       .missing = "scorecard"),
           type_of_option = recode_factor(type_of_option,
                                          "0" = "missing",
                                          .missing = "missing",
                                          "likely to cost more" = "likely to cost more",
                                          "invest to save" = "invest to save",
                                          "likely to save" = "likely to save")
           
    )
}


import_chart_data <- function(s3_path, shortlist = NULL) {
  # reads in data from a specified path in s3, then cleans it using the cleaning functions above
  s3tools::read_using(readxl::read_excel,
                      s3_path,
                      sheet = "Options Scorecard (summary)",
                      skip = 6) %>%
    rename_and_filter_data(shortlist) %>% # this function has an optional "shortlist" parameter, which should be a vector of option references. If included, the data will be filtered to just those options.
    reorder_categorical_variables()
  
}


# charting functions ------------------------------------------------------



all_dimensions_chart <- function(chart_data) {
  ggplot(data = chart_data, 
         mapping = aes(x = strategic_alignment,
                       y = evidence,
                       text = paste("Option ref:", option_ref))) +
    geom_point(position = position_jitterdodge(jitter.width = 0.3,
                                               jitter.height = 0.3,
                                               dodge.width = 0.75),
               shape = 21,
               stroke = 0.3,
               alpha = 0.5,
               mapping = aes(fill = total_cost,
                             size = financial_impacts)) +
    facet_grid(forcats::fct_rev(deliverability_risk) ~ impact_on_outcomes) +
    scale_fill_manual(values = c("black",
                                 "red",
                                 "orange",
                                 "green4")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}


strategic_alignment_vs_impact <- function(chart_data) {
  
  fills <- c("black",
             "red",
             "orange",
             "green4")
  
  
  ggplot(data = chart_data, 
         mapping = aes(x = strategic_alignment,
                       y = impact_on_outcomes,
                       text = paste("</br>Option ref:", option_ref,
                                    "</br> Strategic alignment: ", strategic_alignment,
                                    "</br> impact: ", impact_on_outcomes,
                                    "</br> financial impact: ", financial_impacts,
                                    "</br> total cost: " ,total_cost))) +
    geom_point(position = position_jitterdodge(jitter.width = 0.3,
                                               jitter.height = 0.3,
                                               dodge.width = 0.75),
               shape = 21,
               stroke = 0.3,
               alpha = 0.5,
               mapping = aes(fill = total_cost,
                             size = financial_impacts)) +
    scale_fill_manual(values = fills) +
    ggtitle("Strategic alignment vs impact on outcomes")
  
  
}

evidence_vs_impact <- function(chart_data) {
  
  fills <- c("black",
             "red",
             "orange",
             "green4")
  
  
  ggplot(data = chart_data, 
         mapping = aes(x = evidence,
                       y = impact_on_outcomes,
                       text = paste("</br>Option ref:", option_ref,
                                    "</br> evidence: ", evidence,
                                    "</br> impact: ", impact_on_outcomes,
                                    "</br> financial impact: ", financial_impacts,
                                    "</br> total cost: " ,total_cost))) +
    geom_point(position = position_jitterdodge(jitter.width = 0.3,
                                               jitter.height = 0.3,
                                               dodge.width = 0.75),
               shape = 21,
               stroke = 0.3,
               alpha = 0.5,
               mapping = aes(fill = total_cost,
                             size = financial_impacts)) +
    scale_fill_manual(values = fills) +
    ggtitle("Evidence vs impact on outcomes")
  
}

strategic_alignment_vs_deliverability <- function(chart_data) {
  fills <- c("black",
             "red",
             "orange",
             "green4")
  
  
  ggplot(data = chart_data, 
         mapping = aes(x = strategic_alignment,
                       y = deliverability_risk,
                       text = paste("</br>Option ref:", option_ref,
                                    "</br> strategic alignment: ", strategic_alignment,
                                    "</br> deliverability: ", deliverability_risk,
                                    "</br> financial impact: ", financial_impacts,
                                    "</br> total cost: " ,total_cost))) +
    geom_point(position = position_jitterdodge(jitter.width = 0.3,
                                               jitter.height = 0.3,
                                               dodge.width = 0.75),
               shape = 21,
               stroke = 0.3,
               alpha = 0.5,
               mapping = aes(fill = total_cost,
                             size = financial_impacts)) +
    scale_fill_manual(values = fills) +
    ggtitle("Strategic alignment vs deliverability")
}

frequency_chart <- function(chart_data, column) {
  # Plots a simple bar chart, with a specified column as the x value.
  
  column <- enquo(column)
  ggplot(chart_data) + 
    geom_bar(aes(x = !!column,
                 fill = !!column)) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title.x = element_blank()) +
    ggtitle(column) +
    scale_fill_brewer(palette = "RdYlGn")
  
}

