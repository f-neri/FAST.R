get_dim <- function(dims_plot, dim_value, version_value) {
  dimension <- dims_plot %>% dplyr::filter(dim == dim_value, version == version_value)
  dimension$value
}