install.packages("gt")
install.packages("gtsummary")

library(gt)
library(gtsummary)
library(readr)

adaptation_data <- read_csv("UG_22-23_data_adapttion.csv");
baseline_data <- read_csv("UG_22-23_data_baseline.csv");

table <- adaptation_data %>%
  tbl_summary(
    by = ...1, # Replace 'variable_name' with the column name to group by
    type = all_continuous() ~ "continuous2",
    missing = "no"
  ) %>%
  add_n() %>%
  modify_header(label ~ "**Variable**", n ~ "**n**", all_stat_cols() ~ "**Value**") %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 1", # Edit table number
    subtitle = "Your table caption"
  ) %>%
  gt::tab_footnote(
    "Your table note",
    locations = cells_stubhead()
  )

# Print the table
table
