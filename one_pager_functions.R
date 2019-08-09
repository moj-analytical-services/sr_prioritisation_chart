source("charting_functions.R")

get_financial_impacts <- function(s3_path) {
  s3tools::read_using(readxl::read_excel,
                      s3_path,
                      sheet = "Options Table") %>%
    filter(Scenario == "Central") %>%
    rename(option_ref = `Option Reference number`) %>%
    group_by(option_ref) %>%
    mutate(finance_2021 = if_else(Table == "Existing Spend",
                                  -abs(`Financial Impact Overlay 2020/21`),
                                  `Financial Impact Overlay 2020/21`)) %>% # Make existing spend negative to subtract it when summing
    summarise(finance_2021 = sum(finance_2021, na.rm = T)) %>%
    mutate(net_financial_impact = case_when(finance_2021 < 0     ~ "1. Net saving",
                                            finance_2021 < 1000  ~ "2. Under £1m",
                                            finance_2021 < 10000 ~ "3. £1m to £10m",
                                            finance_2021 < 50000 ~ "4. £10m to £50m",
                                            TRUE                 ~ "5. Over £50m"))
  
}

get_options_categories <- function(s3_path) {
  s3tools::read_using(readr::read_csv, s3_path) %>%
    filter(category_of_option != "#N/A") %>%
    mutate(category_of_option = factor(category_of_option,
                                       levels = c("Committed Existing Activity and Pressures",
                                                  "Further Investment to Mitigate Significant Operational Risk",
                                                  "Efficiency and Income Opportunities",
                                                  "Additional Investment to Improve Outcomes and Performance")))
}

one_pager_chart <- function(chart_data) {
  fills <- c("black",
             "red",
             "orange",
             "green4")
  
  
  ggplot(data = chart_data, 
         mapping = aes(x = net_financial_impact,
                       y = evidence)) +
    geom_point(position = position_jitterdodge(jitter.width = 0.3,
                                               jitter.height = 0.3,
                                               dodge.width = 0.75),
               shape = 21,
               stroke = 0.3,
               alpha = 0.5,
               size = 5,
               mapping = aes(fill = deliverability_risk)) +
    scale_fill_manual(values = fills) +
    ggtitle("Spending Review Options") +
    guides(fill = guide_legend(override.aes = list(size=5))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~category_of_option,
               labeller = label_wrap_gen(),
               nrow = 1)
  
}
