# library(devtools)
# devtools::load_all()

# create df with all widths and heights and calculate respective values
additional_variables <- "Serum"
length(additional_variables)
df <- read.csv("/Users/alicezhang/Downloads/Example_Data/new_fastr/newFASTR_Analysis_Report_2024-07-21.csv")
other_add_var <- "Serum"
# Prepare data
remove_background = TRUE
df <- if (remove_background) {
  dplyr::filter(df, !stringr::str_detect(df$Condition, "_background"))
} else {
  df
}
df <- df %>%
  dplyr::mutate(
    # Change % to proportions for scales::percent calls to work
    dplyr::across(dplyr::starts_with("percentage"), ~ . / 100),
    # Convert additional variables to factors
    dplyr::across(dplyr::all_of(additional_variables), factor)
  )

# test function

df <- tibble::tibble(
  dim = c("width", "height",
          "width_comparison", "height_comparison",
          "width_percentages", "height_percentages"),
  version = c("72")) %>%
  dplyr::mutate(value = dplyr::case_when(
    dim == "width" ~ ifelse(length(additional_variables) > 0,
                            300 + 300 * length(unique(df[[ additional_variables[1] ]])),
                            600),
    dim == "height" ~ ifelse(length(additional_variables) > 1,
                             100 + 250 * length(unique(df[[ additional_variables[2] ]])),
                             350),
    dim == "width_comparison" ~ 300 + 300 * length(unique(df$Condition)),
    dim == "height_comparison" ~ ifelse(length(other_add_var) > 0,
                                        100 + 250 * length(unique(df[[other_add_var]])),
                                        350),
    dim == "width_percentages" ~ ifelse(length(additional_variables) > 0,
                                        300 + 300 * length(unique(df[[ additional_variables[1] ]])),
                                        600),
    dim == "height_percentages" ~ ifelse(length(other_add_var) > 0,
                                         60 * length(unique(df$Condition)) * length(unique(df[[ additional_variables[2] ]])),
                                         60 * length(unique(df$Condition)))
  ))
dim(df)
