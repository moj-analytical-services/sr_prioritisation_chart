library(plotly)
library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)


rename_and_filter_data <- function(chart_data) {
# This takes the raw data, and selects just the columns we'ere interested in.
# it's easier to work with variable names that are in camel_case, as you don't have to use backticks.
# Follow the pattern below to add columns where necessary:
# select(new_variable = `old variable`)
  chart_data %>%
    select(option_description = `Option Description`,
           impact = `3. Impact on outcomes (scale)`,
           financial_impacts = `1. Financial impacts`,
           strategic_alignment = `2. Strategic alignment (or other political priority)`,
           deliverability_risk = `5. Deliverability risk is`,
           evidence = `6. The evidence on outcomes/\r\nperformance is`,
           total_cost = `12. The total cost (central estimate) is`)
  
}

reorder_categorical_variables <- function(chart_data) {
  # this is used to manually set the order of the categories for categorical variables.
  # Generally useful to do in ascending order. Note that facet_grid outputs the y axis in reverse order,
  # so use forcats::fct_rev to reverse the order
  
  chart_data %>%
    mutate(evidence = factor(evidence,
                             levels = c("0",
                                        "missing (Red RAG rating)",
                                        "OK (Amber RAG rating)",
                                        "robust (green RAG rating)")),
    deliverability_risk = factor(deliverability_risk, 
                                 levels = c("0",
                                            "Red RAG rating",
                                            "Amber RAG rating",
                                            "Green RAG rating")),
    financial_impacts = factor(financial_impacts,
                               levels = c("are missing",
                                          "are indicative",
                                          "are robust")),
    impact = factor(impact,
                    levels = c("0",
                               "missing",
                               "low",
                               "medium",
                               "high")),
    total_cost = factor(total_cost,
                        levels = c("0",
                                   "Less than £1m per year / a net benefit",
                                   "Between £1.1m and £5m per year",
                                   "More than £5.1m per year"))
    
    )
}


# Imprts the data and runs the above functions to clean.
# Probably want to turn this into a function, but 
chart_data <- readxl::read_excel("QA returns 04062019_for thomas.xlsx", skip = 3) %>%
  rename_and_filter_data() %>%
  reorder_categorical_variables()


sr_chart <- ggplot(data = chart_data, 
                   mapping = aes(x = impact,
                                 y = evidence,
                                 fill = financial_impacts,
                                 text = paste("Option name:", option_description))) +
  geom_point(position = position_jitter(),
             shape = 16,
             stroke = 0,
             mapping = aes(size = total_cost)) +
  facet_grid(forcats::fct_rev(deliverability_risk) ~ strategic_alignment) +
  scale_fill_manual(values = c("red",
                               "orange",
                               "green4")) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Makes it interactive via plotly
ggplotly(sr_chart)

